"""Audit potential truncation by comparing filesystem content lengths with indexed lengths.

Process:
 1. Walk filesystem for supported extensions.
 2. Build map id -> fs_length (characters after utf-8 decode replace).
 3. Page through index selecting id, content.
 4. For each indexed doc compute index_length.
 5. Compare lengths; if index_length < fs_length by more than threshold (default 8 chars) record potential truncation.

Outputs:
  - Prints distribution stats (min/max/avg diff, count truncated, worst cases) .
  - Writes JSON lines file 'content_truncation_suspects.log' with entries {id, fsLength, indexLength, diff} when suspected.

Heuristic: small differences (e.g., trailing newlines) are ignored via threshold.
"""
from __future__ import annotations
import os, re, argparse, hashlib, json, statistics
from pathlib import Path
from typing import List, Dict
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


def build_fs_lengths(files: List[Path]) -> Dict[str, int]:
    mapping: Dict[str, int] = {}
    for f in files:
        try:
            raw = f.read_bytes().decode('utf-8', errors='replace')
        except Exception:
            raw = ''
        prog_guess = f.stem.upper()
        prog = extract_program_id(raw, prog_guess)
        sid = stable_id(prog, f)
        mapping[sid] = len(raw)
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
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:160]}")
    return r.json().get('value', [])


def audit(fs_lengths: Dict[str, int], endpoint: str, key: str, page_size: int, threshold: int):
    diffs = []  # (id, fs_len, idx_len, diff)
    skip = 0
    while True:
        docs = fetch_page(endpoint, key, skip, page_size)
        if not docs:
            break
        skip += len(docs)
        for d in docs:
            doc_id = d['id']
            idx_len = len((d.get('content') or ''))
            fs_len = fs_lengths.get(doc_id)
            if fs_len is None:
                continue  # orphan in index
            diff = fs_len - idx_len
            if diff > threshold:
                diffs.append((doc_id, fs_len, idx_len, diff))
    return diffs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--page-size', type=int, default=1000)
    ap.add_argument('--threshold', type=int, default=8, help='Min char difference to flag potential truncation')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY')

    print('Discovering filesystem...')
    files = discover_files(args.roots)
    fs_lengths = build_fs_lengths(files)
    print(f'Filesystem docs: {len(fs_lengths)}')

    print('Auditing index content lengths...')
    diffs = audit(fs_lengths, endpoint, key, args.page_size, args.threshold)

    if not diffs:
        print('No truncation suspects beyond threshold.')
        return

    diffs.sort(key=lambda x: x[3], reverse=True)
    with open('content_truncation_suspects.log','w',encoding='utf-8') as fh:
        for doc_id, fs_len, idx_len, diff in diffs:
            fh.write(json.dumps({'id': doc_id, 'fsLength': fs_len, 'indexLength': idx_len, 'diff': diff})+'\n')

    diff_values = [d[3] for d in diffs]
    print(f'Suspected truncated docs: {len(diffs)} (see content_truncation_suspects.log)')
    print(f'Max diff: {max(diff_values)}; Avg diff: {statistics.mean(diff_values):.2f}; Median diff: {statistics.median(diff_values):.2f}')
    print('Top 10 worst diffs:')
    for doc_id, fs_len, idx_len, diff in diffs[:10]:
        print(f'  {doc_id}: fs={fs_len} idx={idx_len} diff={diff}')

if __name__ == '__main__':
    main()
