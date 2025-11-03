#!/usr/bin/env python3
"""Backfill embeddings for new_cobol_calls.snippet_vector."""
import os, sys, json, time, requests

# Config
INDEX = 'new_cobol_calls'
VECTOR_FIELD = 'snippet_vector'
TEXT_FIELD = 'snippet'
KEY_FIELD = 'call_id'
API_VERSION = '2023-11-01'
BATCH_SIZE = int(os.getenv('CALLS_BACKFILL_BATCH', '64'))
TRUNCATE = 4000
RETRIES = 5

def load_config():
    vals = json.load(open('local.settings.json')).get('Values', {})
    return {
        'search_ep': vals['AZURE_SEARCH_ENDPOINT'].rstrip('/'),
        'search_key': vals['AZURE_SEARCH_KEY'],
        'openai_ep': vals['AZURE_OPENAI_ENDPOINT'],
        'openai_key': vals['AZURE_OPENAI_KEY'],
        'deployment': vals.get('AZURE_OPENAI_EMBEDDING_DEPLOYMENT', 'text-embedding-3-large'),
        'api_version': vals.get('AZURE_OPENAI_API_VERSION', '2024-08-01-preview')
    }

def fetch_all_docs(ep, key):
    """Fetch all call documents."""
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    docs = []
    skip = 0
    page_size = 1000
    
    while True:
        payload = {
            'search': '*',
            'select': f'{KEY_FIELD},{TEXT_FIELD},has_vector',
            'top': page_size,
            'skip': skip
        }
        
        r = requests.post(url, headers=headers, json=payload, timeout=60)
        if r.status_code != 200:
            print(f"Error fetching docs: {r.status_code}")
            break
        
        batch = r.json().get('value', [])
        docs.extend(batch)
        
        if len(batch) < page_size:
            break
        skip += page_size
        if len(docs) % 5000 == 0:
            print(f"  Loaded {len(docs):,} docs...")
    
    return docs

def embed_batch(openai_ep, openai_key, api_version, deployment, texts):
    """Embed texts with retry logic."""
    url = f"{openai_ep}/openai/deployments/{deployment}/embeddings?api-version={api_version}"
    headers = {'api-key': openai_key, 'Content-Type': 'application/json'}
    
    for attempt in range(RETRIES):
        try:
            r = requests.post(url, headers=headers, json={'input': texts}, timeout=120)
            if r.status_code == 200:
                data = r.json()
                return [d['embedding'] for d in data.get('data', [])]
            elif r.status_code == 429:
                wait = (2 ** attempt) * 1.0
                print(f"  Rate limit, waiting {wait}s...")
                time.sleep(wait)
            else:
                if attempt < RETRIES - 1:
                    time.sleep(1)
                else:
                    raise RuntimeError(f"Embed failed: {r.status_code}")
        except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
            if attempt < RETRIES - 1:
                wait = (2 ** attempt) * 0.5
                print(f"  Network error, retry {attempt+1}/{RETRIES}, waiting {wait}s...")
                time.sleep(wait)
            else:
                raise
    
    raise RuntimeError("Embedding failed after all retries")

def upload_batch(ep, key, docs):
    """Upload docs with retry."""
    if not docs:
        return
    
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    actions = [{'@search.action': 'mergeOrUpload', **d} for d in docs]
    
    for attempt in range(RETRIES):
        try:
            r = requests.post(url, headers=headers, json={'value': actions}, timeout=120)
            if r.status_code == 200 or r.status_code == 207:
                return
            elif attempt < RETRIES - 1:
                wait = (2 ** attempt) * 0.5
                time.sleep(wait)
            else:
                raise RuntimeError(f"Upload failed: {r.status_code}")
        except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
            if attempt < RETRIES - 1:
                wait = (2 ** attempt) * 0.5
                time.sleep(wait)
            else:
                raise

def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument('--force', action='store_true', help='Re-embed all regardless of has_vector')
    args = ap.parse_args()
    
    print("=" * 80)
    print(f"BACKFILL EMBEDDINGS: {INDEX}.{VECTOR_FIELD}")
    print("=" * 80)
    
    cfg = load_config()
    
    # Fetch all docs
    print(f"\nFetching all call documents...")
    all_docs = fetch_all_docs(cfg['search_ep'], cfg['search_key'])
    print(f"Total docs: {len(all_docs):,}")
    
    # Filter to those needing embeddings
    if args.force:
        to_embed = all_docs
        print(f"Force mode: embedding all {len(to_embed):,} docs")
    else:
        to_embed = [d for d in all_docs if not d.get('has_vector')]
        print(f"Found {len(to_embed):,} docs needing embeddings")
    
    if not to_embed:
        print("\n✅ All docs already have embeddings!")
        return
    
    # Process in batches
    total = len(to_embed)
    processed = 0
    start_time = time.time()
    
    for i in range(0, total, BATCH_SIZE):
        batch = to_embed[i:i+BATCH_SIZE]
        
        # Build texts
        texts = []
        for doc in batch:
            snippet = doc.get(TEXT_FIELD, '') or ''
            # Truncate long snippets
            if len(snippet) > TRUNCATE:
                snippet = snippet[:TRUNCATE]
            # Fallback if empty
            if not snippet.strip():
                snippet = f"COBOL call site {doc.get(KEY_FIELD, 'unknown')}"
            texts.append(snippet)
        
        # Embed
        try:
            vectors = embed_batch(cfg['openai_ep'], cfg['openai_key'], 
                                cfg['api_version'], cfg['deployment'], texts)
            
            # Prepare upload
            upload_docs = []
            for doc, vec in zip(batch, vectors):
                upload_docs.append({
                    KEY_FIELD: doc[KEY_FIELD],
                    VECTOR_FIELD: vec,
                    'has_vector': True
                })
            
            # Upload
            upload_batch(cfg['search_ep'], cfg['search_key'], upload_docs)
            
            processed += len(batch)
            elapsed = time.time() - start_time
            rate = processed / elapsed if elapsed > 0 else 0
            remaining = (total - processed) / rate if rate > 0 else 0
            
            print(f"Processed {processed:,}/{total:,} ({processed/total*100:.1f}%) "
                  f"| {rate:.1f} docs/s | ETA: {remaining/60:.1f} min")
            
        except Exception as e:
            print(f"  ❌ Batch failed: {str(e)[:200]}")
            continue
    
    dur = time.time() - start_time
    print(f"\n{'=' * 80}")
    print(f"COMPLETED: {processed:,}/{total:,} calls embedded in {dur/60:.1f} minutes")
    print(f"{'=' * 80}")

if __name__ == '__main__':
    main()
