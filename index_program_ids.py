#!/usr/bin/env python3
"""Scan COBOL sources to extract PROGRAM-ID and ENTRY points.

Outputs JSONL records:
  { "kind": "program-id", "name": <UPPER_NAME>, "file_path": <path>, "line": <line> }
  { "kind": "entry",      "name": <UPPER_NAME>, "file_path": <path>, "line": <line> }

Heuristics:
- Fixed and free format tolerated; sequence area (cols 1-6) stripped if numeric/blank.
- PROGRAM-ID. <name> [IS] [COMMON|INITIAL] PROGRAM.
- ENTRY "name" or ENTRY name (alphanumeric) or ENTRY 'name'
- Normalizes name by stripping quotes then uppercasing.

Usage:
  python index_program_ids.py --root cobol_src --out JSONL/program_ids.jsonl
"""
from __future__ import annotations
import os, re, json, argparse
from typing import List

PROG_RE = re.compile(r"\bPROGRAM-ID\.?\s+([A-Z0-9][A-Z0-9-_]*)", re.IGNORECASE)
ENTRY_RE = re.compile(r"\bENTRY\s+(['\"])?([A-Z0-9][A-Z0-9-_]*)(['\"])?:?", re.IGNORECASE)

COBOL_EXTS = {'.cbl','.cob','.cpy','.cobol'}

def strip_seq(line: str) -> str:
    if len(line) > 6 and all(c in ' 0123456789' for c in line[:6]):
        return line[6:]
    return line

def iter_files(root: str):
    for base,dirs,files in os.walk(root):
        for fn in files:
            if os.path.splitext(fn)[1].lower() in COBOL_EXTS:
                yield os.path.join(base,fn)

def extract(path: str):
    out=[]
    try:
        with open(path,'r',encoding='utf-8',errors='ignore') as f:
            for ln,raw in enumerate(f, start=1):
                line = strip_seq(raw.rstrip('\n'))
                for m in PROG_RE.finditer(line):
                    name = m.group(1).strip().strip('"\'').upper()
                    out.append({"kind":"program-id","name":name,"file_path":path,"line":ln})
                for m in ENTRY_RE.finditer(line):
                    name = m.group(2).strip().upper()
                    out.append({"kind":"entry","name":name,"file_path":path,"line":ln})
    except Exception:
        return out
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', default='cobol_src')
    ap.add_argument('--out', default='JSONL/program_ids.jsonl')
    args = ap.parse_args()
    recs: List[dict] = []
    for path in iter_files(args.root):
        recs.extend(extract(path))
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out,'w',encoding='utf-8') as f:
        for r in recs:
            f.write(json.dumps(r, ensure_ascii=False) + '\n')
    prog_cnt = sum(1 for r in recs if r['kind']=='program-id')
    entry_cnt = sum(1 for r in recs if r['kind']=='entry')
    print(f"Indexed program-id={prog_cnt} entry={entry_cnt} total={len(recs)} -> {args.out}")

if __name__ == '__main__':
    main()
