#!/usr/bin/env python3
"""Backfill paragraphs vectors - FORCE MODE (ignore has_vector flags)."""
import os, json, requests, argparse, time, random
from embedding_utils import batch_embed

API = '2023-11-01'
INDEX = 'new_cobol_paragraphs'
VEC_FIELD = 'para_vector'
DIM = 3072
SELECT = 'para_id,program_id,paragraph_name,source_excerpt'

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

def fetch_all_docs(ep, key):
    """Fetch ALL docs to embed (ignore has_vector)."""
    docs = []
    skip = 0
    page_size = 1000
    
    while True:
        body = {'search': '*', 'top': page_size, 'skip': skip, 'select': SELECT}
        r = requests.post(
            f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json=body,
            timeout=120
        )
        
        if r.status_code != 200:
            raise SystemExit(f"Fetch failed: {r.status_code} - {r.text[:500]}")
        
        batch = r.json().get('value', [])
        docs.extend(batch)
        
        if len(batch) < page_size:
            break
        
        skip += page_size
        if len(docs) % 10000 == 0:
            print(f"  Loaded {len(docs):,} docs...")
    
    return docs

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
    """Build embedding text from paragraph document."""
    program = d.get('program_id', 'unknown')
    para_name = d.get('paragraph_name', 'unnamed')
    para_text = (d.get('source_excerpt') or '')[:2000]
    
    if not para_text.strip():
        para_text = f"Paragraph {para_name} in program {program}"
    
    return f"Program {program} paragraph {para_name}: {para_text}"[:2500]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL: {INDEX}.{VEC_FIELD} (FORCE MODE)")
    print("=" * 80)
    
    # Fetch all docs
    print("\nFetching all documents (ignoring has_vector)...")
    all_docs = fetch_all_docs(ep, key)
    print(f"Total docs to embed: {len(all_docs):,}")
    
    # Process in batches
    embedded = 0
    start = time.time()
    
    for i in range(0, len(all_docs), args.batch):
        batch = all_docs[i:i+args.batch]
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
                'para_id': d['para_id'],
                VEC_FIELD: v,
                'has_vector': True
            })
        
        # Upload
        upload(ep, key, payload)
        embedded += len(payload)
        
        # Progress
        if embedded % 512 == 0 or embedded == len(all_docs):
            elapsed = time.time() - start
            rate = embedded / elapsed if elapsed > 0 else 0
            remaining = (len(all_docs) - embedded) / rate if rate > 0 else 0
            print(f"Progress: {embedded:,}/{len(all_docs):,} ({embedded/len(all_docs)*100:.1f}%) "
                  f"| {rate:.1f} docs/s | ETA: {remaining/60:.1f} min")
    
    dur = time.time() - start
    print(f"\n{'=' * 80}")
    print(f"COMPLETED: {embedded:,} embeddings in {dur/60:.1f} minutes")
    print(f"{'=' * 80}")

if __name__ == '__main__':
    main()
