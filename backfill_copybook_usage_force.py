#!/usr/bin/env python3
"""Backfill copybook_usage vectors - FORCE MODE (ignore has_vector flags)."""
import os, json, requests, argparse, time, random
from embedding_utils import batch_embed

API = '2023-11-01'
INDEX = 'new_cobol_copybook_usage'
VEC_FIELD = 'context_vector'
DIM = 3072
SELECT = 'usage_id,program_id,copybook_name,context_snippet'

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_batch_streaming(ep, key, skip, page_size):
    """Fetch one batch of docs using skip/top pagination (max skip=100000)."""
    body = {'search': '*', 'top': page_size, 'skip': skip, 'select': SELECT}
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=120
    )
    
    if r.status_code != 200:
        raise SystemExit(f"Fetch failed: {r.status_code} - {r.text[:500]}")
    
    return r.json().get('value', [])

def upload(ep, key, docs):
    """Upload in batches of 128."""
    if not docs:
        return
    
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    for i in range(0, len(docs), 128):
        payload = {'value': [{'@search.action': 'merge', **d} for d in docs[i:i+128]]}
        r = requests.post(url, headers=headers, json=payload, timeout=60)
        if r.status_code not in (200, 201):
            raise SystemExit(f"Upload failed: {r.status_code} - {r.text[:300]}")

def build_text(d):
    return f"Program {d.get('program_id')} copies {d.get('copybook_name')} snippet: {(d.get('context_snippet') or '')[:900]}"[:1000]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--max-skip', type=int, default=100000, help='Max skip value before stopping')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL: {INDEX}.{VEC_FIELD} (FORCE MODE)")
    print("=" * 80)
    print(f"Note: Will process up to skip={args.max_skip:,} docs (Azure limit)")
    print("=" * 80)
    
    # Stream and process in batches (don't load all into memory)
    embedded = 0
    start = time.time()
    skip = 0
    page_size = 1000
    
    while skip < args.max_skip:
        # Fetch a page
        page_docs = fetch_batch_streaming(ep, key, skip, page_size)
        
        if not page_docs:
            print(f"\nNo more documents at skip={skip:,}")
            break
        
        skip += len(page_docs)
        
        # Process this page in embedding batches
        for i in range(0, len(page_docs), args.batch):
            batch = page_docs[i:i+args.batch]
            texts = [build_text(d) for d in batch]
        
        # Embed with retry
        attempt = 0
        while True:
            try:
                vecs = batch_embed(texts, target_dim=None, batch_size=len(batch))
                break
            except Exception as e:
                attempt += 1
                if attempt > 5:
                    raise
                back = min(60, 2**attempt + random.random())
                print(f"  [WARN] embed error, retry {attempt}/5 in {back:.1f}s")
                time.sleep(back)
        
        # Prepare upload
        payload = []
        for d, v in zip(batch, vecs):
            if len(v) != DIM:
                if len(v) > DIM:
                    v = v[:DIM]
                else:
                    v = v + [0.0] * (DIM - len(v))
            payload.append({
                'usage_id': d['usage_id'],
                VEC_FIELD: v,
                'has_vector': True
            })
        
            # Upload
            upload(ep, key, payload)
            embedded += len(payload)
            
            # Progress
            if embedded % 512 == 0:
                elapsed = time.time() - start
                rate = embedded / elapsed if elapsed > 0 else 0
                print(f"Progress: {embedded:,} docs | {rate:.1f} docs/s | "
                      f"Elapsed: {elapsed/60:.1f} min | Skip: {skip:,}")
    
    dur = time.time() - start
    print(f"\n{'=' * 80}")
    print(f"COMPLETED: {embedded:,} embeddings in {dur/60:.1f} minutes")
    print(f"{'=' * 80}")

if __name__ == '__main__':
    main()
