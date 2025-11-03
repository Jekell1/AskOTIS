"""Gap analysis between filesystem COBOL/copybook files and indexed documents.

Outputs:
  - Counts of filesystem documents, indexed documents (queried), overlap.
  - List of ids missing in index (gap_missing_ids.log)
  - List of ids present in index but no longer on disk (gap_orphan_ids.log)

Relies on same stable id logic as ingestion.
"""
from __future__ import annotations
import os, re, argparse, hashlib, json
from pathlib import Path
from typing import List, Dict, Set
import requests
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
SUPPORTED_EXT = {'.cbl', '.cob', '.cobol', '.cpy', '.copy'}
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9_-]+)', re.IGNORECASE)

def extract_program_id(text: str, fallback: str) -> str:
    m = PROGRAM_ID_RE.search(text)
    return m.group(1).upper() if m else fallback.upper()

def stable_id(program: str, path: Path) -> str:
    rel = path.as_posix()
    h = hashlib.sha1(rel.encode('utf-8')).hexdigest()[:10]
    return f"{program}-{h}"

def discover_files(roots: List[str]) -> List[Path]:
    out: List[Path] = []
    for root in roots:
        p = Path(root)
        if not p.exists():
            continue
        for f in p.rglob('*'):
            if f.is_file() and f.suffix.lower() in SUPPORTED_EXT:
                out.append(f)
    return out

def build_fs_ids(files: List[Path]) -> Set[str]:
    ids: Set[str] = set()
    for f in files:
        try:
            raw = f.read_bytes().decode('utf-8', errors='replace')
        except Exception:
            raw = ''
        prog_guess = f.stem.upper()
        prog = extract_program_id(raw, prog_guess)
        sid = stable_id(prog, f)
        ids.add(sid)
    return ids

def fetch_all_ids(endpoint: str, key: str, page_size: int = 1000) -> Set[str]:
    gathered: Set[str] = set()
    skip = 0
    while True:
        params = {
            'api-version': API_VERSION,
            '$skip': str(skip),
            '$top': str(page_size),
            '$select': 'id'
        }
        url = f"{endpoint}/indexes/{INDEX_NAME}/docs"
        r = requests.get(url, headers={'api-key': key}, params=params, timeout=60)
        if r.status_code != 200:
            raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:160]}")
        vals = r.json().get('value', [])
        if not vals:
            break
        for v in vals:
            gathered.add(v['id'])
        skip += len(vals)
    return gathered

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--page-size', type=int, default=1000)
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY')

    files = discover_files(args.roots)
    fs_ids = build_fs_ids(files)
    index_ids = fetch_all_ids(endpoint, key, args.page_size)

    missing = sorted(fs_ids - index_ids)
    orphan = sorted(index_ids - fs_ids)

    print(f"Filesystem ids: {len(fs_ids)}; Index ids: {len(index_ids)}; Missing: {len(missing)}; Orphan: {len(orphan)}")

    if missing:
        with open('gap_missing_ids.log','w',encoding='utf-8') as fh:
            for mid in missing:
                fh.write(mid+'\n')
        print('Wrote gap_missing_ids.log')
    if orphan:
        with open('gap_orphan_ids.log','w',encoding='utf-8') as fh:
            for oid in orphan:
                fh.write(oid+'\n')
        print('Wrote gap_orphan_ids.log')

if __name__ == '__main__':
    main()
