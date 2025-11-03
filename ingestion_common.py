"""Common ingestion helpers for COBOL Azure Search population.

Goals:
- Walk source tree (COBOL_SOURCE_ROOT) collecting .cbl, .cob, .cpy files
- Extract paragraphs, symbols (data names), calls and generic code chunks
- Provide normalization & ID helpers
- Provide batching + upload utility reused across ingest_* scripts

Parsing Strategy:
Lightweight regex-based extraction with conservative patterns to avoid
false positives. For more exact parsing, integrate existing cobolparser
later; this module keeps zero-dependency for quick bootstrap.
"""
from __future__ import annotations
import os, re, hashlib, json, time, math
from pathlib import Path
from typing import Iterable, List, Dict, Any, Generator, Tuple, Optional, Callable
import requests

COBOL_EXTENSIONS = {".cbl", ".cob"}
COPY_EXTENSIONS = {".cpy"}

PARA_PATTERN = re.compile(r"^\s{0,7}([0-9A-Za-z][0-9A-Za-z\-]*)\s+SECTION\.|^\s{0,7}([0-9A-Za-z][0-9A-Za-z\-]*)\s*\.\s*$", re.IGNORECASE)
CALL_PATTERN = re.compile(r"CALL\s+['\"]?([A-Z0-9_\-]+)['\"]?", re.IGNORECASE)
DATA_DIVISION_PATTERN = re.compile(r"^\s*01\s+([A-Z0-9\-]+)", re.IGNORECASE)

# Simple sentence boundary to create chunks (N lines window)
DEFAULT_CHUNK_LINES = 25

class SourceWalker:
    def __init__(self, root: str):
        self.root = Path(root)
        if not self.root.exists():
            raise FileNotFoundError(f"COBOL source root not found: {root}")
    def iter_files(self) -> Generator[Path, None, None]:
        for p in self.root.rglob('*'):
            if not p.is_file():
                continue
            ext = p.suffix.lower()
            if ext in COBOL_EXTENSIONS or ext in COPY_EXTENSIONS:
                yield p

# ID helpers

def stable_hash(parts: Iterable[str]) -> str:
    h = hashlib.sha1()
    for p in parts:
        h.update(p.encode('utf-8', 'ignore'))
        h.update(b'\x00')
    return h.hexdigest()

# Extraction functions

def extract_paragraphs(lines: List[str]) -> List[Dict[str, Any]]:
    paras = []
    current = None
    for idx, raw in enumerate(lines, start=1):
        m = PARA_PATTERN.search(raw)
        if m:
            name = m.group(1) or m.group(2)
            if current:
                current['end_line'] = idx - 1
                paras.append(current)
            current = { 'name': name.upper(), 'start_line': idx, 'end_line': idx }
    if current:
        current['end_line'] = len(lines)
        paras.append(current)
    # assign para_id
    for p in paras:
        p['para_id'] = stable_hash([p['name'], str(p['start_line']), str(p['end_line'])])
    return paras

def extract_calls(lines: List[str], file_path: str | None = None) -> List[Dict[str, Any]]:
    """Extract CALL statements with added line column + per-line occurrence index.

    To avoid ID collisions (multiple CALLs on same line producing identical snippet),
    we capture:
      - line number
      - column (1-based start of match)
      - occurrence (0-based within the line)
    The final call_id (when file_path provided) is a stable hash of
      [file_path, called_program, line, col, occurrence]
    If file_path is None, a provisional call_id using just called_program+line+occurrence is used.
    """
    out: List[Dict[str, Any]] = []
    for idx, raw in enumerate(lines, start=1):
        occurrence = 0
        for m in CALL_PATTERN.finditer(raw):
            prog = m.group(1).upper()
            col = m.start() + 1  # 1-based column
            rec = {
                'called_program': prog,
                'line': idx,
                'col': col,
                'occurrence': occurrence,
                'snippet': raw.strip()[:400]
            }
            if file_path:
                rec['call_id'] = stable_hash([file_path, prog, str(idx), str(col), str(occurrence)])
            else:
                rec['call_id'] = stable_hash([prog, str(idx), str(col), str(occurrence)])
            out.append(rec)
            occurrence += 1
    return out

def extract_data_names(lines: List[str]) -> List[Dict[str, Any]]:
    out=[]
    for idx, raw in enumerate(lines, start=1):
        m = DATA_DIVISION_PATTERN.search(raw)
        if m:
            name = m.group(1).upper()
            out.append({'name': name, 'line': idx})
    for d in out:
        d['item_id'] = stable_hash([d['name'], str(d['line'])])
    return out

def split_chunks(lines: List[str], window: int = DEFAULT_CHUNK_LINES) -> List[Dict[str, Any]]:
    chunks=[]
    total = len(lines)
    if total == 0:
        return chunks
    step = window
    for start in range(0, total, step):
        end = min(start + window, total)
        text = ''.join(lines[start:end])
        chunks.append({
            'start_line': start+1,
            'end_line': end,
            'text': text[:3000]
        })
    for ch in chunks:
        ch['chunk_id'] = stable_hash([str(ch['start_line']), str(ch['end_line']), ch['text'][:60]])
    return chunks

def split_chunks_overlapping(lines: List[str], window: int = DEFAULT_CHUNK_LINES, stride: int | None = None) -> List[Dict[str, Any]]:
    """Produce possibly overlapping chunks with a stride smaller than window.

    Returns list of dicts each containing: start_line, end_line, text, window_index, window_size, stride.
    If stride is None or stride >= window, behaves like non-overlapping (same as split_chunks).
    """
    if stride is None or stride <= 0 or stride >= window:
        stride = window
    out: List[Dict[str, Any]] = []
    total = len(lines)
    if total == 0:
        return out
    win_idx = 0
    for start in range(0, total, stride):
        end = min(start + window, total)
        if start >= total:
            break
        text = ''.join(lines[start:end])
        rec = {
            'start_line': start + 1,
            'end_line': end,
            'text': text[:6000],  # allow a bit larger cap for overlapped windows
            'window_index': win_idx,
            'window_size': window,
            'stride': stride,
        }
        rec['chunk_id'] = stable_hash([str(rec['start_line']), str(rec['end_line']), rec['text'][:60], str(win_idx)])
        out.append(rec)
        if end >= total:
            break
        win_idx += 1
    return out

# Document builders per index

def build_paragraph_docs(file_path: Path, lines: List[str], program_id: str) -> List[Dict[str, Any]]:
    docs=[]
    for p in extract_paragraphs(lines):
        docs.append({
            'para_id': p['para_id'],
            'file_id': program_id,
            'name': p['name'],
            'kind': 'paragraph',
            'start_line': p['start_line'],
            'end_line': p['end_line'],
            'name_vector': None,  # to be filled
            'has_vector': False
        })
    return docs

def build_symbol_docs(file_path: Path, lines: List[str], program_id: str) -> List[Dict[str, Any]]:
    docs=[]
    for s in extract_data_names(lines):
        docs.append({
            'item_id': s['item_id'],
            'file_id': program_id,
            'path': str(file_path),
            'program_id': program_id,
            'name': s['name'],
            'qualified_name': s['name'],
            'section': 'WORKING-STORAGE',
            'level': None,
            'pic': None,
            'usage': None,
            'start_line': s['line'],
            'end_line': s['line'],
            'name_vector': None,
            'has_vector': False
        })
    return docs

def build_call_docs(file_path: Path, lines: List[str], program_id: str) -> List[Dict[str, Any]]:
    docs: List[Dict[str, Any]] = []
    for c in extract_calls(lines, str(file_path)):
        docs.append({
            'call_id': c['call_id'],
            'file_id': program_id,
            'caller_para': None,
            'callee_program': c['called_program'],
            'callee_data_name': None,
            'is_dynamic': False,
            'line': c['line'],
            'col': c['col'],
            'occurrence': c['occurrence'],
            'snippet': c['snippet'],
            'snippet_vector': None,
            'has_vector': False
        })
    return docs

def build_chunk_docs(file_path: Path, lines: List[str], program_id: str) -> List[Dict[str, Any]]:
    docs=[]
    for ch in split_chunks(lines):
        docs.append({
            'chunk_id': ch['chunk_id'],
            'file_id': program_id,
            'path': str(file_path),
            'program_id': program_id,
            'scope': 'file',
            'name': Path(file_path).name,
            'start_line': ch['start_line'],
            'end_line': ch['end_line'],
            'text': ch['text'],
            'text_vector': None,
            'has_vector': False
        })
    return docs

# Batch upload helpers

def chunk_iter(seq: List[Any], size: int) -> Generator[List[Any], None, None]:
    for i in range(0, len(seq), size):
        yield seq[i:i+size]

class AzureSearchUploader:
    def __init__(self, endpoint: str, key: str, index_name: str):
        self.endpoint = endpoint.rstrip('/')
        self.key = key
        self.index_name = index_name
        self.headers = { 'api-key': key, 'Content-Type': 'application/json' }
    def upload(self, docs: List[Dict[str, Any]], batch_size: int = 1000, vector_field: Optional[str] = None):
        url = f"{self.endpoint}/indexes/{self.index_name}/docs/index?api-version=2024-07-01"
        sent = 0
        for batch in chunk_iter(docs, batch_size):
            body = { 'value': [ {**d, '@search.action': 'mergeOrUpload'} for d in batch ] }
            resp = requests.post(url, headers=self.headers, json=body, timeout=30)
            if resp.status_code >= 300:
                raise RuntimeError(f"Upload failed {resp.status_code}: {resp.text[:200]}")
            sent += len(batch)
        return sent

__all__ = [
    'SourceWalker','stable_hash','extract_paragraphs','extract_calls','extract_data_names',
    'split_chunks','split_chunks_overlapping','build_paragraph_docs','build_symbol_docs','build_call_docs','build_chunk_docs',
    'AzureSearchUploader'
]
