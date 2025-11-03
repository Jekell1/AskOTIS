"""Backfill embeddings for new_cobol_program_meta.program_summary into summary_vector.

Strategy:
  - Query for docs missing has_vector or summary_vector (null/empty)
  - Embed program_summary text using Azure OpenAI
  - Update docs with summary_vector and has_vector=true

Usage:
  python backfill_program_meta_embeddings.py [--batch 64] [--page 400] [--dry-run]
"""
from __future__ import annotations
import os, json, argparse, time, requests
from secrets_loader import load_secrets
from typing import List

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_meta'
VEC_FIELD = 'summary_vector'
HAS = 'has_vector'
EMB_DEPLOY = os.getenv('EMBEDDINGS_DEPLOYMENT','text-embedding-3-large')
MISSING_FILTER = "(has_vector eq false) or (has_vector eq null)"


def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: 
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def resolve_openai():
    base = os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')
    key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    ver = os.getenv('OPENAI_API_VERSION','2024-08-01-preview')
    if not base or not key: 
        raise SystemExit('Missing OpenAI base/key')
    return base.rstrip('/'), key, ver

def fetch_page(ep, key, top, skip, filter_expr: str | None = None):
    body = {
        'search': '*',
        'top': top,
        'skip': skip,
        'select': 'program_id,program_summary,has_vector'
    }
    if filter_expr:
        body['filter'] = filter_expr
    
    r = requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", 
                     headers={'api-key': key, 'Content-Type': 'application/json'}, 
                     json=body, timeout=60)
    if r.status_code != 200: 
        raise SystemExit(r.text[:200])
    return r.json().get('value', [])

def embed_texts(base, key, ver, texts: List[str]):
    url = f"{base}/openai/deployments/{EMB_DEPLOY}/embeddings?api-version={ver}"
    r = requests.post(url, 
                     headers={'api-key': key, 'Content-Type': 'application/json'}, 
                     json={'input': texts}, timeout=120)
    if r.status_code != 200: 
        raise SystemExit(f'Embed failed {r.status_code}: {r.text[:200]}')
    return [d['embedding'] for d in r.json()['data']]

def upload(ep, key, docs):
    if not docs: 
        return
    
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    for d in docs: 
        d['@search.action'] = 'merge'
    
    for i in range(0, len(docs), 128):
        batch = docs[i:i+128]
        r = requests.post(url, 
                         headers={'api-key': key, 'Content-Type': 'application/json'}, 
                         json={'value': batch}, timeout=60)
        if r.status_code not in (200, 201): 
            raise SystemExit(r.text[:200])

def main():
    ap = argparse.ArgumentParser(description='Backfill program summary embeddings')
    ap.add_argument('--page', type=int, default=400)
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--limit', type=int, default=0)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--all', dest='missing_only', action='store_false', 
                   help='Process all docs (not just missing)')
    ap.set_defaults(missing_only=True)
    
    args = ap.parse_args()
    load_secrets()
    ep, key = resolve()
    base, okey, ver = resolve_openai()
    
    skip = 0
    total = 0
    embedded = 0
    start = time.time()
    filter_expr = MISSING_FILTER if args.missing_only else None
    
    while True:
        if args.limit and total >= args.limit:
            break
            
        page = fetch_page(ep, key, args.page, skip, filter_expr=filter_expr)
        if not page:
            break
            
        skip += len(page)
        
        # Collect docs needing embedding
        pending = [d for d in page if (not d.get('has_vector')) or (not args.missing_only)]
        if not pending:
            continue
            
        # Process in batches
        idx = 0
        while idx < len(pending):
            batch_docs = pending[idx:idx+args.batch]
            idx += args.batch
            
            # Extract text to embed
            texts = []
            for d in batch_docs:
                summary = d.get('program_summary', '').strip()
                if not summary:
                    summary = f"Program {d.get('program_id', 'UNKNOWN')} metadata"
                texts.append(summary[:8000])  # Truncate very long summaries
            
            if args.dry_run:
                print(f"[DRY-RUN] Would embed {len(texts)} texts")
                print(f"Sample: {texts[0][:100]}...")
                return
            
            # Get embeddings
            embs = embed_texts(base, okey, ver, texts)
            
            # Prepare upload payload
            payload = [
                {
                    'program_id': d['program_id'],
                    VEC_FIELD: e,
                    HAS: True
                } 
                for d, e in zip(batch_docs, embs)
            ]
            
            upload(ep, key, payload)
            embedded += len(payload)
            total += len(batch_docs)
            
            if embedded % (args.batch * 10) == 0:
                print(f"Progress embedded={embedded} scanned={total}")
        
        if len(page) < args.page:
            break
    
    dur = time.time() - start
    print(f"Done embedded={embedded} scanned={total} in {dur:.1f}s")

if __name__ == '__main__':
    main()