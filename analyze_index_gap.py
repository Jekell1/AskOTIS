"""Analyze gap between discovered COBOL/copybook files and indexed documents in new-cobol-files.

Outputs:
 - gap_missing_ids.txt : IDs expected from filesystem but not found in index
 - gap_missing_paths.txt : Corresponding file paths for missing IDs
 - gap_orphan_ids.txt  : IDs present in index but with no corresponding local file path (path mismatch or file removed)
 - summary printed to stdout

Method:
 1. Discover files (same extensions as ingestion).
 2. Recompute stable IDs (must match stable_id logic used in ingestion).
 3. Page through index selecting id + path fields (no need to fetch vectors/content).
 4. Compare sets and write results.

Usage:
  python analyze_index_gap.py --roots .
  python analyze_index_gap.py --roots src_cobol copybooks --limit 0

Optional args:
  --limit N : only analyze first N discovered files (debug) 

Assumptions:
 - Index name: new-cobol-files
 - Same stable ID function hashing path relative form (using posix path of absolute).
"""
from __future__ import annotations
import os, argparse, hashlib, json, time
from pathlib import Path
import requests
import env_autoload

INDEX = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
SUPPORTED_EXT = {'.cbl', '.cob', '.cobol', '.cpy', '.copy'}
PAGE_SIZE = 1000


def discover_paths(roots):
    out = []
    for r in roots:
        p = Path(r)
        if not p.exists():
            continue
        for f in p.rglob('*'):
            if f.is_file() and f.suffix.lower() in SUPPORTED_EXT:
                out.append(f)
    return out


def extract_program_id(content: str, fallback: str):
    # Light extraction; robust version lives elsewhere. Simpler for speed.
    import re
    m = re.search(r'PROGRAM-ID\.?\s+([A-Z0-9_-]+)', content, re.IGNORECASE)
    if m:
        return m.group(1).upper()
    return fallback.upper()


def stable_id(program: str, path: Path) -> str:
    rel = path.as_posix()
    h = hashlib.sha1(rel.encode('utf-8')).hexdigest()[:10]
    return f"{program}-{h}"


def build_expected_id_map(paths):
    id_to_path = {}
    for p in paths:
        try:
            txt = p.read_text(errors='ignore')
        except Exception:
            continue
        program_guess = p.stem.upper()
        prog = extract_program_id(txt, program_guess)
        sid = stable_id(prog, p)
        id_to_path[sid] = p
    return id_to_path


def fetch_all_index_ids():
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    all_ids = {}
    skip = 0
    while True:
        body = {
            "search": "*",
            "select": "id,path",
            "count": True,
            "top": PAGE_SIZE,
            "skip": skip
        }
        r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, data=json.dumps(body).encode('utf-8'))
        if r.status_code != 200:
            raise RuntimeError(f"Fetch page failed {r.status_code}: {r.text[:200]}")
        data = r.json()
        batch = data.get('value', [])
        for d in batch:
            all_ids[d['id']] = d.get('path')
        if len(batch) < PAGE_SIZE:
            break
        skip += PAGE_SIZE
    return all_ids


def write_list(path: str, items):
    with open(path, 'w', encoding='utf-8') as fh:
        for it in items:
            fh.write(str(it)+"\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--limit', type=int, default=0)
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])

    print('Discovering filesystem...')
    paths = discover_paths(args.roots)
    if args.limit:
        paths = paths[:args.limit]
    print(f"Discovered {len(paths)} candidate files")

    print('Computing expected IDs...')
    id_map = build_expected_id_map(paths)
    print(f"Computed {len(id_map)} expected IDs")

    print('Fetching index IDs...')
    index_ids = fetch_all_index_ids()
    print(f"Fetched {len(index_ids)} index documents")

    expected_ids = set(id_map.keys())
    indexed_ids = set(index_ids.keys())

    missing = sorted(expected_ids - indexed_ids)
    orphan = sorted(indexed_ids - expected_ids)

    write_list('gap_missing_ids.txt', missing)
    write_list('gap_missing_paths.txt', [id_map[m].as_posix() for m in missing])
    write_list('gap_orphan_ids.txt', orphan)

    print('\nGAP ANALYSIS SUMMARY:')
    print(f" Expected (filesystem) IDs: {len(expected_ids)}")
    print(f" Indexed IDs:              {len(indexed_ids)}")
    print(f" Missing (not indexed):    {len(missing)} -> gap_missing_ids.txt / gap_missing_paths.txt")
    print(f" Orphan (no local file):   {len(orphan)} -> gap_orphan_ids.txt")

    if missing:
        sample = missing[:5]
        print(' Sample missing IDs:', ', '.join(sample))
    if orphan:
        sample = orphan[:5]
        print(' Sample orphan IDs:', ', '.join(sample))

if __name__ == '__main__':
    main()
