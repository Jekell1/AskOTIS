#!/usr/bin/env python3
"""Backfill copybook_usage vectors - streaming mode (handles >100K docs)."""
import os, json, requests, argparse, time, random
from embedding_utils import batch_embed

API = '2023-11-01'
INDEX = 'new_cobol_copybook_usage'
VEC_FIELD = 'context_vector'
DIM = 3072
SELECT = 'usage_id,program_id,copybook_name,context_snippet'
TOTAL_EXPECTED = 114307  # Known total from analysis

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

def fetch_page(ep, key, skip, top):
    """Fetch one page of docs."""
    body = {'search': '*', 'top': top, 'skip': skip, 'select': SELECT}
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=120
    )
    if r.status_code != 200:
        return []
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
            print(f"  [WARN] Upload failed: {r.status_code}")

def build_text(d):
    return f"Program {d.get('program_id')} copies {d.get('copybook_name')} snippet: {(d.get('context_snippet') or '')[:900]}"[:1000]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--page', type=int, default=1000, help='Fetch page size')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL: {INDEX}.{VEC_FIELD} (STREAMING MODE)")
    print(f"Expected total: {TOTAL_EXPECTED:,} docs")
    print("=" * 80)
    
    embedded = 0
    start = time.time()
    skip = 0
    buffer = []  # Accumulate docs for batching
    
    # Fetch and process in streaming fashion
    while skip < 100000:  # Azure skip limit
        page = fetch_page(ep, key, skip, args.page)
        if not page:
            break
        
        skip += len(page)
        buffer.extend(page)
        
        # Process buffer in embedding batches
        while len(buffer) >= args.batch:
            batch = buffer[:args.batch]
            buffer = buffer[args.batch:]
            
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
                        print(f"  [ERROR] Failed after {attempt} retries: {e}")
                        break
                    back = min(60, 2**attempt + random.random())
                    print(f"  [WARN] Retry {attempt}/5 in {back:.1f}s")
                    time.sleep(back)
            
            if attempt <= 5:  # Success
                # Prepare upload
                payload = []
                for d, v in zip(batch, vecs):
                    if len(v) != DIM:
                        v = v[:DIM] if len(v) > DIM else v + [0.0] * (DIM - len(v))
                    payload.append({
                        'usage_id': d['usage_id'],
                        VEC_FIELD: v,
                        'has_vector': True
                    })
                
                upload(ep, key, payload)
                embedded += len(payload)
                
                # Progress
                if embedded % 512 == 0:
                    elapsed = time.time() - start
                    rate = embedded / elapsed if elapsed > 0 else 0
                    pct = embedded / TOTAL_EXPECTED * 100 if TOTAL_EXPECTED > 0 else 0
                    eta = (TOTAL_EXPECTED - embedded) / rate / 60 if rate > 0 else 0
                    print(f"{embedded:,}/{TOTAL_EXPECTED:,} ({pct:.1f}%) | {rate:.1f}/s | ETA: {eta:.0f}min | Skip: {skip:,}")
    
    # Process remaining buffer
    if buffer:
        print(f"\nProcessing final {len(buffer)} docs...")
        for i in range(0, len(buffer), args.batch):
            batch = buffer[i:i+args.batch]
            texts = [build_text(d) for d in batch]
            vecs = batch_embed(texts, target_dim=None, batch_size=len(batch))
            payload = []
            for d, v in zip(batch, vecs):
                if len(v) != DIM:
                    v = v[:DIM] if len(v) > DIM else v + [0.0] * (DIM - len(v))
                payload.append({'usage_id': d['usage_id'], VEC_FIELD: v, 'has_vector': True})
            upload(ep, key, payload)
            embedded += len(payload)
    
    dur = time.time() - start
    print(f"\n{'=' * 80}")
    print(f"COMPLETED: {embedded:,}/{TOTAL_EXPECTED:,} embeddings in {dur/60:.1f} minutes")
    print(f"{'=' * 80}")

if __name__ == '__main__':
    main()
