"""Ingest COBOL paragraphs into new_cobol_paragraphs index.

Extraction heuristics:
  - Detect SECTION headers: ^\s{0,8}([0-9A-Z-]+)\s+SECTION\.
  - Detect paragraph labels: ^\s{0,8}([0-9A-Z-]+)\.
  - Exclude PROCEDURE DIVISION line.
  - Accumulate lines until next label or EOF.

Generates documents with fields defined in create_paragraphs_index.py.

Usage:
  python ingest_cobol_paragraphs.py --roots src_dir1 src_dir2 --limit-files 20 --dry-run
"""
import os, re, sys, json, argparse, hashlib, datetime, time, requests
from typing import List, Dict, Any, Optional, Tuple
from embedding_utils import batch_embed, provider_info
from search_schema_utils import get_index_field_dimension

INDEX_NAME = 'new_cobol_paragraphs'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

COBOL_EXTENSIONS = {'.cbl', '.cob', '.cobol', '.cpy'}
PARA_RE = re.compile(r'^\s{0,8}([0-9A-Z][0-9A-Z0-9-]*)\.$')
SECTION_RE = re.compile(r'^\s{0,8}([0-9A-Z][0-9A-Z0-9-]*)\s+SECTION\.')
# Relaxed patterns: allow lower case and trailing inline comments; more permissive label chars
RELAXED_PARA_RE = re.compile(r'^\s{0,12}([0-9A-Z][0-9A-Z0-9-]{0,62})\.(?:\s|$)', re.IGNORECASE)
RELAXED_SECTION_RE = re.compile(r'^\s{0,12}([0-9A-Z][0-9A-Z0-9-]{0,62})\s+SECTION\.', re.IGNORECASE)
PROC_DIV_RE = re.compile(r'PROCEDURE\s+DIVISION', re.IGNORECASE)

CHECKPOINT_FILE = 'paragraph_ingest.ckpt'

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def walk_files(roots: List[str]) -> List[str]:
    out = []
    for root in roots:
        for dirpath, _, files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_EXTENSIONS:
                    path = os.path.join(dirpath, f)
                    out.append(path)
    return sorted(out)

def hash_text(s: str) -> str:
    return hashlib.sha256(s.encode('utf-8', errors='replace')).hexdigest()

def normalize_program_id(path: str) -> str:
    base = os.path.basename(path)
    name = os.path.splitext(base)[0]
    return re.sub(r'[^A-Za-z0-9_-]+','_', name).upper()

def extract_paragraphs(path: str) -> List[Dict[str,Any]]:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"[WARN] Failed to read {path}: {e}", file=sys.stderr)
        return []
    program_id = normalize_program_id(path)
    paras: List[Dict[str,Any]] = []
    current_name = None
    current_section = None
    current_kind = None
    current_start = None
    buffer: List[str] = []
    def flush(end_line_index: int):
        nonlocal paras, current_name, current_section, current_kind, current_start, buffer
        if current_name is None:
            buffer = []
            return
        text = ''.join(buffer).strip('\n')
        if not text:
            buffer = []
            return
        length_lines = end_line_index - current_start + 1
        para_id = f"{program_id}-{current_name}-{current_start+1}-{end_line_index+1}"[:256]
        rec = {
            'para_id': para_id,
            'program_id': program_id,
            'file_id': path.replace('\\','/'),
            'paragraph_name': current_name,
            'section_name': current_section or '',
            'kind': current_kind or 'paragraph',
            'line_start': current_start + 1,
            'line_end': end_line_index + 1,
            'length_lines': length_lines,
            'source_excerpt': text,
            'hash': hash_text(text),
            'has_vector': False,
            'para_vector': None,
            'ingested_at': None
        }
        paras.append(rec)
        buffer = []
    for idx, raw in enumerate(lines):
        line = raw.rstrip('\n')
        if PROC_DIV_RE.search(line):
            # treat as boundary but include line with next paragraph if labeled after
            pass
        m_sec = SECTION_RE.match(line.strip())
        if m_sec:
            # flush previous
            flush(idx-1)
            current_section = m_sec.group(1).upper()
            current_name = current_section
            current_kind = 'section'
            current_start = idx
            buffer = [line + '\n']
            continue
        m_para = PARA_RE.match(line.strip())
        if m_para and not SECTION_RE.match(line.strip()):
            flush(idx-1)
            current_name = m_para.group(1).upper()
            current_kind = 'paragraph'
            current_start = idx
            buffer = [line + '\n']
            continue
        # accumulate
        if current_name is not None:
            buffer.append(line + '\n')
    # final flush
    flush(len(lines)-1)
    return paras

def extract_paragraphs_relaxed(path: str) -> List[Dict[str,Any]]:
    """Relaxed variant for re-auditing zero-coverage programs.

    Differences:
      - Case-insensitive label and SECTION detection
      - Allows up to 12 leading spaces
      - Accepts labels with up to 63 chars (still truncated in para_id if needed)
    """
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"[WARN] Failed to read {path}: {e}", file=sys.stderr)
        return []
    program_id = normalize_program_id(path)
    paras: List[Dict[str,Any]] = []
    current_name = None
    current_section = None
    current_kind = None
    current_start = None
    buffer: List[str] = []
    def flush(end_line_index: int):
        nonlocal paras, current_name, current_section, current_kind, current_start, buffer
        if current_name is None:
            buffer = []
            return
        text = ''.join(buffer).strip('\n')
        if not text:
            buffer = []
            return
        length_lines = end_line_index - current_start + 1
        para_id = f"{program_id}-{current_name}-{current_start+1}-{end_line_index+1}"[:256]
        rec = {
            'para_id': para_id,
            'program_id': program_id,
            'file_id': path.replace('\\','/'),
            'paragraph_name': current_name,
            'section_name': current_section or '',
            'kind': current_kind or 'paragraph',
            'line_start': current_start + 1,
            'line_end': end_line_index + 1,
            'length_lines': length_lines,
            'source_excerpt': text,
            'hash': hash_text(text),
            'has_vector': False,
            'para_vector': None,
            'ingested_at': None
        }
        paras.append(rec)
        buffer = []
    for idx, raw in enumerate(lines):
        line = raw.rstrip('\n')
        if PROC_DIV_RE.search(line):
            pass
        stripped = line.strip()
        m_sec = RELAXED_SECTION_RE.match(stripped)
        if m_sec:
            flush(idx-1)
            current_section = m_sec.group(1).upper()
            current_name = current_section
            current_kind = 'section'
            current_start = idx
            buffer = [line + '\n']
            continue
        m_para = RELAXED_PARA_RE.match(stripped)
        if m_para and not RELAXED_SECTION_RE.match(stripped):
            flush(idx-1)
            current_name = m_para.group(1).upper()
            current_kind = 'paragraph'
            current_start = idx
            buffer = [line + '\n']
            continue
        if current_name is not None:
            buffer.append(line + '\n')
    flush(len(lines)-1)
    return paras

def chunk_upload(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs:
        return
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type":"application/json"}
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def read_checkpoint() -> Optional[Dict[str,Any]]:
    try:
        return json.load(open(CHECKPOINT_FILE,'r'))
    except Exception:
        return None

def write_checkpoint(data: Dict[str,Any]):
    try:
        with open(CHECKPOINT_FILE,'w') as f:
            json.dump(data,f)
    except Exception:
        pass

def main():
    ap = argparse.ArgumentParser(description='Ingest COBOL paragraphs into new_cobol_paragraphs index.')
    ap.add_argument('--roots', nargs='+', default=['.'])
    ap.add_argument('--limit-files', type=int)
    ap.add_argument('--limit-paragraphs', type=int)
    ap.add_argument('--batch-size', type=int, default=500)
    ap.add_argument('--embed-batch-size', type=int, default=64)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--resume', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key()
    files = walk_files(args.roots)
    if args.limit_files:
        files = files[:args.limit_files]
    print(f"Discovered {len(files)} COBOL files")
    expected_dim = get_index_field_dimension(ep, key, INDEX_NAME, 'para_vector')
    if expected_dim is None:
        print('Warning: could not determine vector field dimension (index missing?)')
    checkpoint = read_checkpoint() if args.resume else None
    start_file_index = 0
    processed_paras = 0
    if checkpoint:
        start_file_index = checkpoint.get('file_index',0)
        processed_paras = checkpoint.get('paragraphs_uploaded',0)
        print(f"Resuming from file index {start_file_index} (paragraphs so far {processed_paras})")
    ingest_ts = datetime.datetime.utcnow().isoformat(timespec='seconds') + 'Z'
    batch: List[Dict[str,Any]] = []
    uploaded = processed_paras
    for fi in range(start_file_index, len(files)):
        path = files[fi]
        paras = extract_paragraphs(path)
        for p in paras:
            if args.limit_paragraphs and uploaded >= args.limit_paragraphs:
                break
            batch.append(p)
            if len(batch) >= args.batch_size:
                if args.dry_run:
                    print(f"Dry-run: would upload batch of {len(batch)} paragraphs")
                    batch.clear()
                else:
                    texts = [d['source_excerpt'] for d in batch]
                    vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
                    if expected_dim and vecs and len(vecs[0]) != expected_dim:
                        raise SystemExit(f"Vector dim mismatch expected {expected_dim} got {len(vecs[0])}")
                    for d,v in zip(batch, vecs):
                        d['para_vector'] = v
                        d['has_vector'] = True
                        d['ingested_at'] = ingest_ts
                    # sub-chunk
                    for i in range(0, len(batch), 500):
                        chunk_upload(ep, key, batch[i:i+500])
                    uploaded += len(batch)
                    print(f"Uploaded paragraphs: {uploaded}")
                    write_checkpoint({'file_index': fi, 'paragraphs_uploaded': uploaded})
                    batch.clear()
        if args.limit_paragraphs and uploaded >= args.limit_paragraphs:
            break
    # Flush remaining
    if batch:
        if args.dry_run:
            print(f"Dry-run: would upload final batch of {len(batch)} paragraphs")
        else:
            texts = [d['source_excerpt'] for d in batch]
            vecs = batch_embed(texts, target_dim=expected_dim, batch_size=args.embed_batch_size)
            for d,v in zip(batch, vecs):
                d['para_vector'] = v
                d['has_vector'] = True
                d['ingested_at'] = ingest_ts
            for i in range(0, len(batch), 500):
                chunk_upload(ep, key, batch[i:i+500])
            uploaded += len(batch)
            write_checkpoint({'file_index': len(files), 'paragraphs_uploaded': uploaded})
            print(f"Uploaded paragraphs: {uploaded}")
    print('Paragraph ingestion complete. Provider=', provider_info())

if __name__ == '__main__':
    main()
