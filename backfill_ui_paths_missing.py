#!/usr/bin/env python3
"""Backfill missing UI path embeddings with small batches and retry logic."""
import os, sys, json, time, requests
from typing import List, Dict, Any

# Config
INDEX = 'new_cobol_ui_paths'
VECTOR_FIELD = 'path_vector'
API_VERSION = '2023-11-01'
BATCH_SIZE = 32  # Small batches for network stability

def load_config():
    vals = json.load(open('local.settings.json')).get('Values', {})
    return {
        'search_ep': vals['AZURE_SEARCH_ENDPOINT'].rstrip('/'),
        'search_key': vals['AZURE_SEARCH_KEY'],
        'openai_ep': vals['AZURE_OPENAI_ENDPOINT'],
        'openai_key': vals['AZURE_OPENAI_KEY'],
        'deployment': vals.get('AZURE_OPENAI_EMBEDDING_DEPLOYMENT', 'text-embedding-3-large'),
    }

def fetch_missing_paths(ep, key, batch_size=100):
    """Fetch paths where has_vector is false or null."""
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    docs = []
    skip = 0
    while True:
        payload = {
            'search': '*',
            'filter': 'has_vector eq false or has_vector eq null',
            'select': 'path_id,program_sequence_json,screen_ids_json,start_program_id,end_program_id',
            'top': batch_size,
            'skip': skip
        }
        
        max_retries = 3
        for attempt in range(max_retries):
            try:
                r = requests.post(url, headers=headers, json=payload, timeout=60)
                if r.status_code == 200:
                    break
                elif attempt < max_retries - 1:
                    time.sleep(1)
            except Exception as e:
                if attempt < max_retries - 1:
                    print(f"  Retry fetch: {str(e)[:80]}")
                    time.sleep(2)
                else:
                    raise
        
        if r.status_code != 200:
            raise RuntimeError(f"Search failed: {r.status_code} {r.text[:200]}")
        
        batch = r.json().get('value', [])
        docs.extend(batch)
        
        if len(batch) < batch_size:
            break
        skip += batch_size
    
    return docs

def build_text(doc: Dict[str, Any]) -> str:
    """Build embedding text from path."""
    seq_json = doc.get('program_sequence_json', '[]')
    try:
        seq = json.loads(seq_json)
    except:
        seq = []
    
    screens_json = doc.get('screen_ids_json', '[]')
    try:
        screens = json.loads(screens_json)
    except:
        screens = []
    
    parts = []
    parts.append(f"UI navigation path with {len(seq)} programs")
    
    if seq:
        parts.append(f"starting at {seq[0]} and ending at {seq[-1]}")
        if len(seq) > 2:
            parts.append(f"passing through {' -> '.join(seq[1:-1])}")
    
    if screens:
        parts.append(f"involving screens: {', '.join(screens[:10])}")
    
    return '. '.join(parts)

def embed_batch(openai_ep, openai_key, deployment, texts: List[str]) -> List[List[float]]:
    """Embed texts with retry logic."""
    url = f"{openai_ep}/openai/deployments/{deployment}/embeddings?api-version=2024-02-15-preview"
    headers = {'api-key': openai_key, 'Content-Type': 'application/json'}
    
    max_retries = 5
    for attempt in range(max_retries):
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
                if attempt < max_retries - 1:
                    time.sleep(1)
                else:
                    raise RuntimeError(f"Embed failed: {r.status_code} {r.text[:200]}")
        except (requests.exceptions.Timeout, requests.exceptions.ConnectionError, 
                requests.exceptions.ChunkedEncodingError) as e:
            if attempt < max_retries - 1:
                wait = (2 ** attempt) * 0.5
                print(f"  Network error ({type(e).__name__}), retry {attempt+1}/{max_retries}, waiting {wait}s...")
                time.sleep(wait)
            else:
                print(f"  FAILED after {max_retries} retries: {str(e)[:100]}")
                raise
    
    raise RuntimeError("Embedding failed after all retries")

def upload_batch(ep, key, docs: List[Dict[str, Any]]):
    """Upload docs with retry."""
    if not docs:
        return
    
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    actions = [{'@search.action': 'mergeOrUpload', **d} for d in docs]
    
    max_retries = 5
    for attempt in range(max_retries):
        try:
            r = requests.post(url, headers=headers, json={'value': actions}, timeout=120)
            if r.status_code == 200 or r.status_code == 207:
                return
            elif attempt < max_retries - 1:
                wait = (2 ** attempt) * 0.5
                print(f"  Upload retry {attempt+1}/{max_retries}, waiting {wait}s...")
                time.sleep(wait)
            else:
                raise RuntimeError(f"Upload failed: {r.status_code} {r.text[:200]}")
        except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
            if attempt < max_retries - 1:
                wait = (2 ** attempt) * 0.5
                print(f"  Network error, retry {attempt+1}/{max_retries}, waiting {wait}s...")
                time.sleep(wait)
            else:
                raise

def main():
    print("=" * 80)
    print("BACKFILL MISSING UI PATH EMBEDDINGS")
    print("=" * 80)
    
    cfg = load_config()
    
    # Fetch missing
    print("\nFetching paths with has_vector=false...")
    missing = fetch_missing_paths(cfg['search_ep'], cfg['search_key'])
    print(f"Found {len(missing)} paths needing embeddings")
    
    if not missing:
        print("\n✅ All paths already have embeddings!")
        return
    
    # Process in small batches
    total = len(missing)
    processed = 0
    
    for i in range(0, total, BATCH_SIZE):
        batch = missing[i:i+BATCH_SIZE]
        batch_num = (i // BATCH_SIZE) + 1
        total_batches = (total + BATCH_SIZE - 1) // BATCH_SIZE
        
        print(f"\nBatch {batch_num}/{total_batches} ({len(batch)} paths)")
        
        # Build texts
        texts = [build_text(doc) for doc in batch]
        
        # Embed
        try:
            print(f"  Embedding...")
            vectors = embed_batch(cfg['openai_ep'], cfg['openai_key'], cfg['deployment'], texts)
            
            # Prepare upload
            upload_docs = []
            for doc, vec in zip(batch, vectors):
                upload_docs.append({
                    'path_id': doc['path_id'],
                    VECTOR_FIELD: vec,
                    'has_vector': True
                })
            
            # Upload
            print(f"  Uploading {len(upload_docs)} vectors...")
            upload_batch(cfg['search_ep'], cfg['search_key'], upload_docs)
            
            processed += len(batch)
            print(f"  ✅ Success! Progress: {processed}/{total} ({processed/total*100:.1f}%)")
            
            # Brief pause between batches
            if i + BATCH_SIZE < total:
                time.sleep(0.5)
                
        except Exception as e:
            print(f"  ❌ Batch failed: {str(e)[:200]}")
            print(f"  Continuing with next batch...")
            continue
    
    print(f"\n{'=' * 80}")
    print(f"COMPLETED: {processed}/{total} paths embedded ({processed/total*100:.1f}%)")
    print(f"{'=' * 80}")

if __name__ == '__main__':
    main()
