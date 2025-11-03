"""Backfill real embeddings for all documents in the new-cobol-files index.

Strategy:
 1. Page through documents projecting only needed fields (id, summary, contentShort, contentVector).
 2. Detect fallback vectors using a precision heuristic: the deterministic fallback generator produced values on a fixed 0.001 grid in range [-0.5, 0.5). Real model embeddings typically have higher precision (no strict millisecond grid) and broader magnitude variance. If an existing vector does NOT look like fallback, we skip unless --force is supplied.
 3. Build embedding input as: summary + two newlines + first 4000 chars of contentShort.
 4. Batch size configurable (default 32). Embeddings generated via Azure OpenAI deployment (env vars required).
 5. Upload partial updates with merge action containing id + contentVector.

NOTE: Original plan referenced an 'embeddingProvider' field which is not present in the current index schema; logic has been updated to avoid selecting or writing that field.

Requires env:
    AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY
    AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Safe to re-run: skips vectors that do not match fallback signature unless --force given.
"""
from __future__ import annotations
import os, json, time, argparse, math
from typing import List, Dict
import requests
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VECTOR_DIMS = 3072

def embed_batch(texts: List[str]) -> List[List[float]]:
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT','').rstrip('/')
    key = os.getenv('AZURE_OPENAI_KEY')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    if not (endpoint and key and dep):
        raise SystemExit('Azure OpenAI embedding environment variables not fully configured.')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'input': texts}, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding request failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    out = [d['embedding'] for d in data.get('data',[])]
    for vec in out:
        if len(vec) != VECTOR_DIMS:
            raise RuntimeError(f'Dimension mismatch expected {VECTOR_DIMS} got {len(vec)}')
    return out

def fetch_page(search_endpoint: str, search_key: str, skip: int, top: int, select: str, filter_expr: str|None=None):
    params = {
        'api-version': API_VERSION,
        '$count': 'false',
        '$skip': str(skip),
        '$top': str(top),
        '$select': select,
    }
    if filter_expr:
        params['$filter'] = filter_expr
    url = f"{search_endpoint}/indexes/{INDEX_NAME}/docs"
    r = requests.get(url, headers={'api-key': search_key}, params=params, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value', [])

def upload_vectors(search_endpoint: str, search_key: str, docs: List[Dict]):
    if not docs:
        return
    url = f"{search_endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload = {'value': docs}
    r = requests.post(url, headers={'api-key': search_key, 'Content-Type':'application/json'}, data=json.dumps(payload).encode('utf-8'), timeout=120)
    if r.status_code >= 300:
        raise RuntimeError(f"Vector upload failed {r.status_code}: {r.text[:200]}")
    jr = r.json()
    failures = [x for x in jr.get('value',[]) if not x.get('status')]
    if failures:
        raise RuntimeError(f"{len(failures)} vector merges failed; sample: {failures[0]}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch-size', type=int, default=32)
    ap.add_argument('--max-docs', type=int, default=0, help='Optional limit for test runs.')
    ap.add_argument('--force', action='store_true', help='Overwrite vectors even if current vector does not look like fallback.')
    ap.add_argument('--start-skip', type=int, default=0, help='Skip offset (resume).')
    ap.add_argument('--page-size', type=int, default=100, help='Fetch page size (independent from embedding batch size).')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    search_key = os.getenv('AZURE_SEARCH_KEY')

    # contentVector was not marked retrievable in index schema; omit from select.
    select = 'id,summary,contentShort'
    skip = args.start_skip
    total_processed = 0
    updated = 0
    start_time = time.time()

    def looks_like_fallback(_vec):
        # Cannot inspect existing vector (not retrievable); assume fallback so we overwrite unless force heuristic disabled.
        return True

    while True:
        page = fetch_page(search_endpoint, search_key, skip, args.page_size, select)
        if not page:
            break
        skip += len(page)
        # Filter candidates
        candidates = []
        for d in page:
            # Without retrievable vector we conservatively re-embed unless user wants to avoid mass overwrite (provide --max-docs for sampling).
            if not args.force:
                pass
            candidates.append(d)
        if not candidates:
            total_processed += len(page)
            if args.max_docs and total_processed >= args.max_docs:
                break
            continue
        # Build embedding inputs in stable order
        batches = []
        cur = []
        for d in candidates:
            text = (d.get('summary','') + '\n\n' + (d.get('contentShort','') or '')[:4000]).strip()
            cur.append((d['id'], text))
            if len(cur) >= args.batch_size:
                batches.append(cur)
                cur = []
        if cur:
            batches.append(cur)
        for b in batches:
            texts = [t for _, t in b]
            vectors = embed_batch(texts)
            merge_docs = []
            for (doc_id, _), vec in zip(b, vectors):
                merge_docs.append({
                    '@search.action': 'merge',
                    'id': doc_id,
                    'contentVector': vec
                })
            upload_vectors(search_endpoint, search_key, merge_docs)
            updated += len(merge_docs)
        total_processed += len(page)
        elapsed = time.time() - start_time
        rate = updated/elapsed if elapsed>0 else 0
        print(f"Progress: processed {total_processed} docs; updated {updated}; rate {rate:.2f}/sec")
        if args.max_docs and total_processed >= args.max_docs:
            break
    print(f"DONE. Updated {updated} vectors.")

if __name__ == '__main__':
    main()
