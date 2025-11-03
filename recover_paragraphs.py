#!/usr/bin/env python3
"""Attempt to recover real paragraph definitions for high-priority unresolved tokens.

Strategy:
 1. Load paragraph stubs (from generate_paragraph_stubs.py) -> unresolved tokens + top_files list.
 2. For each token, inspect candidate source files (heuristic: look for files named <file_id>.cbl, .cob, or present in workspace).
 3. Scan file text for paragraph label patterns:   <LABEL>.<whitespace|EOL>
    Accept forms: LABEL., LABEL: , LABEL SECTION., plus optional leading spaces.
 4. Record recovered definitions with start line and an approximate end line (next label start - 1).
 5. Output JSONL containing new paragraph records: { para_id, name, file_id, start_line, end_line, recovered: true, strategy: 'label-scan' }

Assumptions:
  - Source files are somewhere under workspace; file_id corresponds to a filename root (case-insensitive) within any path.
  - COBOL lines end with newline; paragraph labels appear at column 8+ or anywhere left trimmed (heuristic lenient).

Limitations:
  - Does not parse nested DECLARATIVES or SECTION scoping; purely label scanning.

Usage:
  python recover_paragraphs.py --stubs JSONL/paragraph_stubs.jsonl --source-root . --out JSONL/paragraphs_recovered.jsonl --max-files 40
"""
from __future__ import annotations
import os, re, json, argparse, hashlib
from typing import List, Dict, Tuple

# Enhanced label pattern:
#  - Optional leading sequence area (handled in code by secondary attempt)
#  - Label followed by optional 'SECTION'
#  - Terminator can be '.' or ':' (some shops use colon) or a '.' omitted (rare) but we still require . or : to reduce false positives
#  - Allow inline comment after terminator
LABEL_RE = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]{0,80})(?:\s+SECTION)?[\.:](?:\s|$)")

def load_stubs(path: str) -> List[Dict]:
    out=[]
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                out.append(json.loads(line))
            except Exception:
                continue
    return out

def collect_candidate_files(root: str) -> List[str]:
    exts = {'.cbl','.cob','.cpy','.cobol'}
    paths=[]
    for base,dirs,files in os.walk(root):
        for fn in files:
            ext = os.path.splitext(fn)[1].lower()
            if ext in exts:
                paths.append(os.path.join(base,fn))
    return paths

def index_by_basename(paths: List[str]) -> Dict[str, List[str]]:
    idx={}
    for p in paths:
        base = os.path.splitext(os.path.basename(p))[0].upper()
        idx.setdefault(base,[]).append(p)
    return idx

def _strip_sequence_area(line: str) -> str:
    # If first 6 cols are digits/spaces and col 7 is space or non-space, treat remainder as code (fixed format)
    if len(line) >= 7 and all(c in ' 0123456789' for c in line[:6]):
        return line[6:]
    return line

def scan_file_for_labels(path: str) -> List[Tuple[str,int]]:
    labels=[]
    try:
        with open(path,'r',encoding='utf-8',errors='ignore') as f:
            for i,line in enumerate(f, start=1):
                raw = line.rstrip('\n')
                # Primary match
                m = LABEL_RE.match(raw)
                if not m:
                    # Try after stripping sequence area
                    candidate = _strip_sequence_area(raw)
                    if candidate is not raw:
                        m = LABEL_RE.match(candidate)
                if m:
                    name = m.group(1).upper()
                    labels.append((name, i))
    except Exception:
        return labels
    return labels

def compute_end_lines(label_positions: List[Tuple[str,int]], total_lines: int) -> Dict[str, Tuple[int,int]]:
    spans={}
    for idx,(name,start) in enumerate(label_positions):
        end = total_lines
        if idx+1 < len(label_positions):
            end = label_positions[idx+1][1]-1
        spans[name]=(start,end)
    return spans

def recover(stubs_path: str, source_root: str, out_path: str, max_files: int):
    stubs = load_stubs(stubs_path)
    all_source = collect_candidate_files(source_root)
    by_base = index_by_basename(all_source)

    recovered = 0
    seen_para_keys = set()
    with open(out_path,'w',encoding='utf-8') as fout:
        for stub in stubs:
            token = stub.get('normalized') or stub.get('name')
            top_files = stub.get('top_files') or []  # list of [file_id, count]
            for fid,_cnt in top_files[:max_files]:
                base = fid.upper()
                if base not in by_base:
                    continue
                for path in by_base[base]:
                    # Scan
                    labels = scan_file_for_labels(path)
                    if not labels:
                        continue
                    # Build spans
                    try:
                        total_lines = sum(1 for _ in open(path,'r',encoding='utf-8',errors='ignore'))
                    except Exception:
                        total_lines = 0
                    spans = compute_end_lines(labels, total_lines)
                    # Emit only if token label found
                    if token in spans:
                        start,end = spans[token]
                        para_id = hashlib.sha1(f"{fid}:{token}:{path}:{start}".encode()).hexdigest()[:20]
                        key = (token, fid, path)
                        if key in seen_para_keys:
                            continue
                        rec = {
                            'para_id': para_id,
                            'name': token,
                            'file_id': fid,
                            'source_path': path,
                            'start_line': start,
                            'end_line': end,
                            'recovered': True,
                            'strategy': 'label-scan',
                        }
                        fout.write(json.dumps(rec, ensure_ascii=False) + '\n')
                        recovered +=1
                        seen_para_keys.add(key)
    print(f"Recovered {recovered} paragraph definitions -> {out_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--stubs', required=True)
    ap.add_argument('--source-root', default='.')
    ap.add_argument('--out', required=True)
    ap.add_argument('--max-files', type=int, default=40)
    args = ap.parse_args()
    recover(args.stubs, args.source_root, args.out, args.max_files)

if __name__ == '__main__':
    main()
