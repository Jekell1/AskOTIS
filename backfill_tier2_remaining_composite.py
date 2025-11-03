#!/usr/bin/env python3
"""Complete Tier 2 using composite ordering strategy.

Since usage_id/para_id are not sortable, we use program_id + line_start (both sortable).
This allows ordered pagination beyond 100K without $skip.

Strategy:
1. Order by program_id, line_start (both sortable)
2. Track last (program_id, line_start) tuple
3. Filter: program_id gt 'last' OR (program_id eq 'last' AND line_start gt last_line)
4. Fetch pages until exhausted

This processes ALL documents in deterministic order without skip limitation.

Usage:
  python backfill_tier2_remaining_composite.py --index copybook_usage --batch 64
  python backfill_tier2_remaining_composite.py --index paragraphs --batch 64
"""
import os, json, time, argparse
import requests
from embedding_utils import batch_embed

API = '2023-11-01'
DIM = 3072

CONFIGS = {
    'copybook_usage': {
        'index': 'new_cobol_copybook_usage',
        'vec_field': 'context_vector',
        'id_field': 'usage_id',
        'select': 'usage_id,program_id,copybook_name,context_snippet,line_start',
        'total': 114307,
        'text_fn': lambda d: f"Program {d.get('program_id')} copies {d.get('copybook_name')} snippet: {(d.get('context_snippet') or '')[:900]}"[:1000]
    },
    'paragraphs': {
        'index': 'new_cobol_paragraphs',
        'vec_field': 'para_vector',
        'id_field': 'para_id',
        'select': 'para_id,program_id,paragraph_name,source_excerpt,line_start',
        'total': 224655,
        'text_fn': lambda d: f"Program {d.get('program_id')} paragraph {d.get('paragraph_name')}: {(d.get('source_excerpt') or '')[:2000]}"[:2500]
    }
}

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

def fetch_composite_page(ep, key, index, select, page_size, last_prog=None, last_line=None):
    """Fetch page using composite orderby (program_id, line_start)."""
    body = {
        'search': '*',
        'top': page_size,
        'orderby': 'program_id,line_start',
        'select': select
    }
    
    # Build composite filter for continuation
    if last_prog is not None and last_line is not None:
        # Either program_id > last_prog OR (program_id == last_prog AND line_start > last_line)
        body['filter'] = f"(program_id gt '{last_prog}') or (program_id eq '{last_prog}' and line_start gt {last_line})"
    
    r = requests.post(
        f"{ep}/indexes/{index}/docs/search?api-version={API}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=120
    )
    
    if r.status_code != 200:
        print(f"  [ERROR] Fetch failed {r.status_code}: {r.text[:300]}")
        return []
    
    return r.json().get('value', [])

def upload(ep, key, index, docs):
    """Upload vectors in batches of 128."""
    if not docs:
        return
    url = f"{ep}/indexes/{index}/docs/index?api-version={API}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    for i in range(0, len(docs), 128):
        payload = {'value': [{'@search.action': 'merge', **d} for d in docs[i:i+128]]}
        r = requests.post(url, headers=headers, json=payload, timeout=60)
        if r.status_code not in (200, 201):
            print(f"  [WARN] Upload failed {r.status_code}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True, choices=['copybook_usage', 'paragraphs'])
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--page', type=int, default=1000)
    ap.add_argument('--resume-prog', type=str, default=None)
    ap.add_argument('--resume-line', type=int, default=None)
    ap.add_argument('--start-from-100k', action='store_true', help='Estimate starting point for docs beyond 100K')
    args = ap.parse_args()
    
    load_settings()
    ep, key = resolve()
    cfg = CONFIGS[args.index]
    
    print("=" * 80)
    print(f"BACKFILL: {cfg['index']}.{cfg['vec_field']} (COMPOSITE ORDERING)")
    print(f"Expected total: {cfg['total']:,} docs")
    print(f"Strategy: $orderby=program_id,line_start + composite $filter")
    if args.resume_prog:
        print(f"Resuming from: program_id={args.resume_prog}, line={args.resume_line}")
    print("=" * 80)
    
    embedded = 0
    start = time.time()
    buffer = []
    last_prog = args.resume_prog
    last_line = args.resume_line
    pages_fetched = 0
    
    try:
        while True:
            page = fetch_composite_page(ep, key, cfg['index'], cfg['select'], args.page, last_prog, last_line)
            if not page:
                break
            
            pages_fetched += 1
            
            # Track last composite key
            last_prog = page[-1]['program_id']
            last_line = page[-1]['line_start']
            
            # Process documents
            for doc in page:
                text = cfg['text_fn'](doc)
                if not text.strip():
                    continue
                
                buffer.append({
                    'id': doc[cfg['id_field']],
                    'text': text
                })
                
                # Embed batch when full
                if len(buffer) >= args.batch:
                    texts = [b['text'] for b in buffer]
                    vectors = batch_embed(texts, target_dim=DIM)
                    
                    upload_docs = [
                        {cfg['id_field']: b['id'], cfg['vec_field']: vec}
                        for b, vec in zip(buffer, vectors)
                    ]
                    
                    upload(ep, key, cfg['index'], upload_docs)
                    embedded += len(buffer)
                    
                    # Progress
                    elapsed = time.time() - start
                    rate = embedded / elapsed if elapsed > 0 else 0
                    pct = (embedded / cfg['total']) * 100
                    eta_min = int((cfg['total'] - embedded) / rate / 60) if rate > 0 else 0
                    
                    print(f"{embedded:,}/{cfg['total']:,} ({pct:.1f}%) | {rate:.1f}/s | ETA: {eta_min}min | Pages: {pages_fetched} | Pos: {last_prog}/{last_line}")
                    
                    buffer = []
        
        # Final batch
        if buffer:
            print(f"Processing final {len(buffer)} docs...")
            texts = [b['text'] for b in buffer]
            vectors = batch_embed(texts, target_dim=DIM)
            upload_docs = [
                {cfg['id_field']: b['id'], cfg['vec_field']: vec}
                for b, vec in zip(buffer, vectors)
            ]
            upload(ep, key, cfg['index'], upload_docs)
            embedded += len(buffer)
    
    except KeyboardInterrupt:
        print(f"\n\nInterrupted at {embedded:,} docs")
        print(f"Resume with: --resume-prog \"{last_prog}\" --resume-line {last_line}")
        return
    except Exception as e:
        print(f"\n\nERROR: {e}")
        import traceback
        traceback.print_exc()
        print(f"\nProcessed {embedded:,} docs")
        print(f"Resume with: --resume-prog \"{last_prog}\" --resume-line {last_line}")
        return
    
    elapsed = time.time() - start
    print("\n" + "=" * 80)
    print(f"COMPLETED: {embedded:,}/{cfg['total']:,} in {elapsed/60:.1f} minutes")
    print(f"Rate: {embedded/elapsed:.1f} docs/sec | Pages: {pages_fetched}")
    print("=" * 80)

if __name__ == '__main__':
    main()
