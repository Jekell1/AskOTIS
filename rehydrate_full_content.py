"""Rehydrate (merge/update) full content for documents that may have been truncated in earlier ingestion.

Logic:
 1. Discover files (.cbl/.cob/.cobol/.cpy/.copy)
 2. Recompute stable id (same as bulk_ingest_cobol_files)
 3. Read full content; build minimal merge doc with id + full content + updatedAt
 4. Batch mergeOrUpload to overwrite truncated field.

Usage:
  python rehydrate_full_content.py --roots . --batch-size 64 --max-files 0

NOTE: Does NOT regenerate embeddings (contentVector unchanged). If you need updated vectors
      run a separate re-embed pass.
"""
from __future__ import annotations
import os, json, time, argparse, hashlib, struct
from pathlib import Path
from typing import List, Dict
import requests
import env_autoload

SUPPORTED_EXT = {'.cbl', '.cob', '.cobol', '.cpy', '.copy'}
INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

PROGRAM_ID_RE = __import__('re').compile(r'PROGRAM-ID\.?\s+([A-Z0-9_-]+)', __import__('re').IGNORECASE)

def stable_id(program: str, path: Path) -> str:
    rel = path.as_posix()
    h = hashlib.sha1(rel.encode('utf-8')).hexdigest()[:10]
    return f"{program}-{h}"

def extract_program_id(text: str, fallback: str) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return fallback.upper()

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

def upload_batch(docs: List[Dict]):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'value': docs}, timeout=120)
    if r.status_code >= 300:
        raise RuntimeError(f"Upload failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    failed = [res for res in data.get('value', []) if not res.get('status')]
    if failed:
        print(f"WARNING: {len(failed)} failures")
    return data

def build_merge_doc(path: Path) -> Dict:
    raw = path.read_text(errors='ignore')
    program_guess = path.stem.upper()
    program = extract_program_id(raw, program_guess)
    return {
        '@search.action': 'mergeOrUpload',
        'id': stable_id(program, path),
        'content': raw,
        'updatedAt': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    }

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True)
    ap.add_argument('--batch-size', type=int, default=64)
    ap.add_argument('--max-files', type=int, default=0)
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])

    files = discover_files(args.roots)
    if args.max_files:
        files = files[:args.max_files]
    print(f"Discovered {len(files)} files for rehydration")

    docs: List[Dict] = []
    for idx, f in enumerate(files, 1):
        docs.append(build_merge_doc(f))
        if idx % 500 == 0:
            print(f"Prepared {idx} merge docs")
    print(f"Prepared {len(docs)} merge docs")

    if args.dry_run:
        print("Dry run complete; no uploads.")
        return

    uploaded = 0
    start = time.time()
    for i in range(0, len(docs), args.batch_size):
        batch = docs[i:i+args.batch_size]
        upload_batch(batch)
        uploaded += len(batch)
        if (i//args.batch_size) % 10 == 0:
            elapsed = time.time() - start
            rate = uploaded/elapsed if elapsed else 0
            print(f"Progress: {uploaded}/{len(docs)} ({rate:.2f} docs/sec)")
    elapsed = time.time() - start
    print(f"DONE. Rehydrated {uploaded} docs in {elapsed:.1f}s ({uploaded/elapsed if elapsed else 0:.2f} docs/sec)")

if __name__ == '__main__':
    main()
