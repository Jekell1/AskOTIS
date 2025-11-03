#!/usr/bin/env python3
"""Backfill paragraphs vectors - ID-based filtering (bypasses skip limit).

Uses $orderby + $filter with ID comparisons to paginate through ALL documents
without relying on $skip parameter. This is the TRUE workaround for the 100K limit.

Strategy:
1. Query with $orderby=para_id (ascending sort)
2. Fetch batch (e.g., 1000 docs at a time)
3. Track last para_id from batch
4. Next query uses $filter="para_id gt '{last_id}'" + $orderby
5. Repeat until no more docs

Environment:
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Usage:
  python backfill_paragraphs_id_filter.py --batch 64 --page 1000
  python backfill_paragraphs_id_filter.py --batch 64 --resume-after "PROG001_PARA001"
"""
import os, json, time, argparse
import requests
from embedding_utils import batch_embed

API = '2023-11-01'
INDEX = 'new_cobol_paragraphs'
VEC_FIELD = 'para_vector'
DIM = 3072
SELECT = 'para_id,program_id,paragraph_name,source_excerpt'
TOTAL_EXPECTED = 224655

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

def fetch_ordered_page(ep, key, page_size, last_id=None):
    """Fetch page using orderby + optional filter (NO $skip).
    
    Returns list of docs ordered by para_id ascending.
    """
    body = {
        'search': '*',
        'top': page_size,
        'orderby': 'para_id',
        'select': SELECT
    }
    
    # If resuming, filter to IDs greater than last processed
    if last_id:
        body['filter'] = f"para_id gt '{last_id}'"
    
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=120
    )
    
    if r.status_code != 200:
        print(f"  [ERROR] Fetch failed {r.status_code}: {r.text[:200]}")
        return []
    
    return r.json().get('value', [])

def upload(ep, key, docs):
    """Upload vectors in batches of 128."""
    if not docs:
        return
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    for i in range(0, len(docs), 128):
        payload = {'value': [{'@search.action': 'merge', **d} for d in docs[i:i+128]]}
        r = requests.post(url, headers=headers, json=payload, timeout=60)
        if r.status_code not in (200, 201):
            print(f"  [WARN] Upload failed {r.status_code}: {r.text[:150]}")

def build_text(d):
    """Build embedding text from document."""
    excerpt = (d.get('source_excerpt') or '')[:2000]
    return f"Program {d.get('program_id')} paragraph {d.get('paragraph_name')}: {excerpt}"[:2500]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--batch', type=int, default=64, help='Embedding batch size')
    ap.add_argument('--page', type=int, default=1000, help='Fetch page size')
    ap.add_argument('--resume-after', type=str, default=None, help='Resume after this para_id')
    ap.add_argument('--max-docs', type=int, default=None, help='Max docs to process (testing)')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"BACKFILL: {INDEX}.{VEC_FIELD} (ID-BASED FILTERING - NO SKIP LIMIT)")
    print(f"Expected total: {TOTAL_EXPECTED:,} docs")
    print(f"Strategy: $orderby=para_id + $filter=para_id gt 'last_id'")
    if args.resume_after:
        print(f"Resuming after ID: {args.resume_after}")
    print("=" * 80)
    
    embedded = 0
    start = time.time()
    buffer = []
    last_id = args.resume_after
    pages_fetched = 0
    
    try:
        while True:
            # Fetch next page ordered by ID
            page = fetch_ordered_page(ep, key, args.page, last_id)
            if not page:
                break
            
            pages_fetched += 1
            
            # Track last ID for next iteration
            last_id = page[-1]['para_id']
            
            # Process documents
            for doc in page:
                text = build_text(doc)
                if not text.strip():
                    continue
                
                buffer.append({
                    'id': doc['para_id'],
                    'text': text
                })
                
                # Embed batch when full
                if len(buffer) >= args.batch:
                    texts = [b['text'] for b in buffer]
                    vectors = batch_embed(texts, dim=DIM)
                    
                    # Prepare upload
                    upload_docs = [
                        {'para_id': b['id'], VEC_FIELD: vec}
                        for b, vec in zip(buffer, vectors)
                    ]
                    
                    upload(ep, key, upload_docs)
                    embedded += len(buffer)
                    
                    # Progress report
                    elapsed = time.time() - start
                    rate = embedded / elapsed if elapsed > 0 else 0
                    pct = (embedded / TOTAL_EXPECTED) * 100 if TOTAL_EXPECTED else 0
                    eta_sec = (TOTAL_EXPECTED - embedded) / rate if rate > 0 else 0
                    eta_min = int(eta_sec / 60)
                    
                    print(f"{embedded:,}/{TOTAL_EXPECTED:,} ({pct:.1f}%) | {rate:.1f}/s | ETA: {eta_min}min | Pages: {pages_fetched} | Last ID: {last_id[:40]}")
                    
                    buffer = []
                    
                    # Check max limit
                    if args.max_docs and embedded >= args.max_docs:
                        print(f"\nReached max docs limit: {args.max_docs:,}")
                        raise KeyboardInterrupt
        
        # Process final partial batch
        if buffer:
            print(f"Processing final {len(buffer)} docs...")
            texts = [b['text'] for b in buffer]
            vectors = batch_embed(texts, dim=DIM)
            upload_docs = [
                {'para_id': b['id'], VEC_FIELD: vec}
                for b, vec in zip(buffer, vectors)
            ]
            upload(ep, key, upload_docs)
            embedded += len(buffer)
    
    except KeyboardInterrupt:
        print(f"\n\nInterrupted at {embedded:,} docs")
        print(f"Resume with: --resume-after \"{last_id}\"")
        return
    except Exception as e:
        print(f"\n\nERROR: {e}")
        import traceback
        traceback.print_exc()
        print(f"\nProcessed {embedded:,} docs before error")
        print(f"Resume with: --resume-after \"{last_id}\"")
        return
    
    elapsed = time.time() - start
    print("\n" + "=" * 80)
    print(f"COMPLETED: {embedded:,}/{TOTAL_EXPECTED:,} embeddings in {elapsed/60:.1f} minutes")
    print(f"Average rate: {embedded/elapsed:.1f} docs/sec")
    print(f"Pages fetched: {pages_fetched}")
    print("=" * 80)

if __name__ == '__main__':
    main()
