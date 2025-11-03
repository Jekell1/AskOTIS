#!/usr/bin/env python3
"""Backfill para_vector embeddings for new_cobol_paragraphs where has_vector == false.

Usage:
  python backfill_embeddings_paragraphs.py --page 500 --batch 64
"""
from __future__ import annotations
import os, json, requests, argparse, time, random
from typing import List
from embedding_utils import batch_embed

API = os.getenv('AZURE_SEARCH_API_VERSION', '2023-11-01')
INDEX = 'new_cobol_paragraphs'
VEC_FIELD = 'para_vector'
DIM = 3072

# Index fields for paragraphs
SELECT = 'para_id,program_id,paragraph_name,source_excerpt,has_vector'

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

def fetch_batch(ep, key, top):
    """Fetch up to 'top' docs still needing vectors.
    
    Strategy: repeatedly pull the first page (skip=0) until no documents match.
    This guarantees eventual coverage as has_vector flips from false->true.
    """
    body = {
        'search': '*',
        'top': top,
        'select': SELECT,
        'filter': 'has_vector eq false'
    }
    
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
    """Upload documents in batches of 128."""
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
    para_text = (d.get('source_excerpt') or '')[:2000]  # Truncate long paragraphs
    
    # Fallback for empty text
    if not para_text.strip():
        para_text = f"Paragraph {para_name} in program {program}"
    
    return f"Program {program} paragraph {para_name}: {para_text}"[:2500]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--page', type=int, default=400, help='Search page size (docs pulled per loop)')
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--preview', action='store_true', help='Show a single transformed doc then exit')
    ap.add_argument('--max', type=int, help='Optional maximum documents to embed (for testing)')
    ap.add_argument('--sleep', type=float, default=0.0, help='Sleep seconds between outer fetch loops')
    
    args = ap.parse_args()
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL EMBEDDINGS: {INDEX}.{VEC_FIELD}")
    print("=" * 80)
    
    embedded = 0
    start = time.time()
    loops = 0
    
    while True:
        rows = fetch_batch(ep, key, args.page)
        if not rows:
            break
        
        loops += 1
        idx = 0
        
        while idx < len(rows):
            batch = rows[idx:idx+args.batch]
            idx += args.batch
            
            if not batch:
                break
            
            if args.max and embedded >= args.max:
                print('[INFO] Reached max limit; stopping early')
                rows = []
                break
            
            # Build texts for embedding
            texts = [build_text(r) for r in batch]
            
            # Embed with retry logic
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
                    print(f"[WARN] embed error {e} retry {attempt} in {back:.1f}s")
                    time.sleep(back)
            
            # Prepare upload payload
            payload = []
            for r, v in zip(batch, vecs):
                # Ensure correct dimensions
                if len(v) != DIM:
                    if len(v) > DIM:
                        v = v[:DIM]
                    else:
                        v = v + [0.0] * (DIM - len(v))
                
                payload.append({
                    'para_id': r['para_id'],
                    VEC_FIELD: v,
                    'has_vector': True
                })
            
            if args.preview:
                print(json.dumps({'preview': payload[0]}, indent=2))
                return
            
            # Upload
            upload(ep, key, payload)
            embedded += len(payload)
            
            # Progress reporting
            if embedded % max(256, args.batch * 8) == 0:
                rate = embedded / max(0.001, (time.time() - start))
                eta = (224655 - embedded) / rate if rate > 0 else 0
                print(f"Progress embedded={embedded:,}/224,655 ({embedded/224655*100:.1f}%) "
                      f"rate={rate:.1f}/s loops={loops} ETA={eta/60:.1f}min")
        
        if args.sleep:
            time.sleep(args.sleep)
    
    dur = time.time() - start
    print("\n" + "=" * 80)
    print(f"COMPLETED: embedded={embedded:,} in {dur/60:.1f} minutes")
    print("=" * 80)

if __name__ == '__main__':
    main()
