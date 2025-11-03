"""Verify that indexed documents correspond to current filesystem contents.

Approach:
 1. Walk filesystem for COBOL/copybook files (same extensions as ingestion).
 2. Recompute stable id (programId detection + path hash) as ingestion does.
 3. For each file: compute a SHA1 hash of full decoded content (utf-8 replace) and store mapping id->hash.
 4. Query index in pages selecting id and a short prefix of content (contentShort + maybe content) to recompute a comparable hash subset.

Because the index stores full 'content', we fetch only 'id' and 'content' and hash the full string returned.

Outputs:
  - JSON lines file 'integrity_mismatches.log' listing any id with differing hash.
  - Summary counts printed.

Limitations:
  - If index truncated content (should not, after schema fix) hashes will differ.
"""
from __future__ import annotations
import os, re, json, argparse, hashlib
from pathlib import Path
from typing import Dict, List
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

def hash_content(text: str) -> str:
    return hashlib.sha1(text.encode('utf-8')).hexdigest()

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

def build_fs_hash_map(files: List[Path]) -> Dict[str, str]:
    mapping = {}
    for f in files:
        try:
            raw = f.read_bytes().decode('utf-8', errors='replace')
        except Exception:
            raw = ''
        prog_guess = f.stem.upper()
        prog = extract_program_id(raw, prog_guess)
        sid = stable_id(prog, f)
        mapping[sid] = hash_content(raw)
    return mapping

def fetch_page(endpoint: str, key: str, skip: int, top: int):
    params = {
        'api-version': API_VERSION,
        '$skip': str(skip),
        '$top': str(top),
        '$select': 'id,content'
    }
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs"
    r = requests.get(url, headers={'api-key': key}, params=params, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value', [])

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--page-size', type=int, default=1000)
    ap.add_argument('--max-pages', type=int, default=0)
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY')

    files = discover_files(args.roots)
    fs_map = build_fs_hash_map(files)
    print(f"Filesystem docs: {len(fs_map)}")

    mismatches = 0
    missing_in_index = 0
    seen_ids = set()
    skip = 0
    pages = 0
    while True:
        page_docs = fetch_page(endpoint, key, skip, args.page_size)
        if not page_docs:
            break
        skip += len(page_docs)
        pages += 1
        for d in page_docs:
            doc_id = d['id']
            seen_ids.add(doc_id)
            content = d.get('content','') or ''
            idx_hash = hash_content(content)
            fs_hash = fs_map.get(doc_id)
            if fs_hash is None:
                # Present in index but not filesystem
                continue
            if idx_hash != fs_hash:
                with open('integrity_mismatches.log','a',encoding='utf-8') as fh:
                    fh.write(json.dumps({'id': doc_id, 'fsHash': fs_hash, 'indexHash': idx_hash})+'\n')
                mismatches += 1
        if args.max_pages and pages >= args.max_pages:
            break
    # Count filesystem ids missing in index
    for fid in fs_map.keys():
        if fid not in seen_ids:
            missing_in_index += 1
    print(f"Indexed docs iterated: {len(seen_ids)}; mismatches: {mismatches}; missingInIndex: {missing_in_index}")
    if mismatches:
        print('See integrity_mismatches.log for details.')

if __name__ == '__main__':
    main()
