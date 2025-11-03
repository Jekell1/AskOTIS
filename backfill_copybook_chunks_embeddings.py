"""
Backfill embeddings for copybook chunks in code-chunks index.

This script adds embeddings to all chunks without vectors (has_vector = false).
It's designed to resume automatically if interrupted.

USAGE:
  python backfill_copybook_chunks_embeddings.py [--batch-size 256] [--max-batches 1000]

RESUMPTION:
  Script automatically skips chunks that already have embeddings (has_vector = true)
"""

import os
import json
import sys
import time
import requests
from typing import List, Dict, Any

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
KEY = os.getenv('AZURE_SEARCH_KEY')
API_VERSION = '2024-07-01'
INDEX_NAME = 'code-chunks'

OPENAI_ENDPOINT = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
OPENAI_KEY = os.getenv('AZURE_OPENAI_KEY')
EMBED_DEPLOYMENT = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-3-large')

HEADERS = {
    'Content-Type': 'application/json',
    'api-key': KEY
}

def get_chunks_without_vectors(skip: int = 0, top: int = 1000) -> List[Dict]:
    """Fetch chunks that need embeddings"""
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
    
    body = {
        'search': '*',
        'filter': 'has_vector ne true',
        'select': 'chunk_id,text',
        'top': top,
        'skip': skip
    }
    
    r = requests.post(url, headers=HEADERS, json=body, timeout=60)
    
    if r.status_code != 200:
        print(f'ERROR: Failed to fetch chunks: {r.status_code} {r.text[:300]}')
        return []
    
    result = r.json()
    return result.get('value', [])

def count_chunks_without_vectors() -> int:
    """Count how many chunks need embeddings"""
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
    
    body = {
        'search': '*',
        'filter': 'has_vector ne true',
        'top': 0,
        'count': True
    }
    
    r = requests.post(url, headers=HEADERS, json=body, timeout=30)
    
    if r.status_code != 200:
        print(f'WARNING: Could not count chunks: {r.status_code}')
        return -1
    
    return r.json().get('@odata.count', 0)

def generate_embeddings(texts: List[str]) -> List[List[float]]:
    """Generate embeddings for a batch of texts"""
    url = f'{OPENAI_ENDPOINT}/openai/deployments/{EMBED_DEPLOYMENT}/embeddings?api-version=2024-02-15-preview'
    
    payload = {
        'input': texts
        # NOTE: No dimensions parameter - use full 3072 dimensions for code-chunks index
    }
    
    headers = {
        'api-key': OPENAI_KEY,
        'Content-Type': 'application/json'
    }
    
    max_retries = 5
    for attempt in range(max_retries):
        r = requests.post(url, headers=headers, json=payload, timeout=120)
        
        if r.status_code == 200:
            data = r.json().get('data', [])
            return [item['embedding'] for item in data]
        
        elif r.status_code == 429:
            wait = 2 ** attempt + 1
            print(f'    Rate limited, waiting {wait}s...', flush=True)
            time.sleep(wait)
        
        else:
            print(f'ERROR: Embedding failed {r.status_code}: {r.text[:300]}')
            if attempt < max_retries - 1:
                time.sleep(2 ** attempt)
            else:
                return None
    
    print(f'ERROR: Failed after {max_retries} retries')
    return None

def upload_vectors(chunk_updates: List[Dict[str, Any]]) -> bool:
    """Upload embeddings to Azure Search"""
    if not chunk_updates:
        return True
    
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}'
    
    docs = []
    for update in chunk_updates:
        docs.append({
            '@search.action': 'merge',  # Only update vector fields
            'chunk_id': update['chunk_id'],
            'text_vector': update['vector'],
            'has_vector': True
        })
    
    payload = {'value': docs}
    
    r = requests.post(url, headers=HEADERS, json=payload, timeout=120)
    
    if r.status_code >= 300:
        print(f'ERROR: Upload failed {r.status_code}: {r.text[:500]}')
        return False
    
    result = r.json()
    failed = [v for v in result.get('value', []) if not v.get('status')]
    if failed:
        print(f'WARNING: {len(failed)} updates failed')
        return False
    
    return True

def main():
    import argparse
    
    ap = argparse.ArgumentParser(description='Backfill embeddings for code-chunks')
    ap.add_argument('--batch-size', type=int, default=256, help='Embedding batch size')
    ap.add_argument('--max-batches', type=int, help='Max batches to process (for testing)')
    ap.add_argument('--start-skip', type=int, default=0, help='Skip first N chunks (for manual resume)')
    args = ap.parse_args()
    
    print('=' * 80)
    print('BACKFILL EMBEDDINGS FOR code-chunks')
    print('=' * 80)
    print()
    
    print('1. Counting chunks without embeddings...')
    missing_count = count_chunks_without_vectors()
    
    if missing_count == 0:
        print('✅ All chunks already have embeddings!')
        return
    
    if missing_count > 0:
        print(f'   Found {missing_count:,} chunks without embeddings')
        est_time = (missing_count / args.batch_size) / 60 * 2  # ~2 seconds per batch
        print(f'   Estimated time: {est_time:.1f} minutes')
    print()
    
    print('2. Starting embedding process...')
    print(f'   Batch size: {args.batch_size}')
    print(f'   Model: {EMBED_DEPLOYMENT}')
    print(f'   Dimensions: 3072 (full dimensionality for code-chunks)')
    print()
    
    total_processed = 0
    total_embedded = 0
    batch_num = 0
    skip = args.start_skip
    
    start_time = time.time()
    last_progress_time = start_time
    
    while True:
        if args.max_batches and batch_num >= args.max_batches:
            print(f'Reached max batches limit ({args.max_batches})')
            break
        
        # Fetch batch
        chunks = get_chunks_without_vectors(skip=0, top=args.batch_size)
        
        if not chunks:
            print('No more chunks to process!')
            break
        
        batch_num += 1
        
        # Generate embeddings
        texts = [chunk.get('text', '') for chunk in chunks]
        vectors = generate_embeddings(texts)
        
        if not vectors or len(vectors) != len(chunks):
            print(f'ERROR: Embedding generation failed for batch {batch_num}')
            break
        
        # Prepare updates
        updates = []
        for chunk, vector in zip(chunks, vectors):
            updates.append({
                'chunk_id': chunk['chunk_id'],
                'vector': vector
            })
        
        # Upload
        if upload_vectors(updates):
            total_processed += len(chunks)
            total_embedded += len(chunks)
            
            # Progress update
            current_time = time.time()
            if current_time - last_progress_time >= 30:  # Every 30 seconds
                elapsed = current_time - start_time
                rate = total_processed / elapsed
                
                # Re-count remaining
                remaining = count_chunks_without_vectors()
                if remaining > 0:
                    eta = remaining / rate
                    print(f'Progress: {total_processed:,} embedded, {remaining:,} remaining - Rate: {rate:.1f}/sec - ETA: {eta/60:.1f} min')
                else:
                    print(f'Progress: {total_processed:,} embedded')
                
                last_progress_time = current_time
        else:
            print(f'ERROR: Upload failed for batch {batch_num}')
            break
        
        # Small delay to avoid rate limits
        time.sleep(0.1)
    
    elapsed = time.time() - start_time
    
    print()
    print('=' * 80)
    print('EMBEDDING BACKFILL COMPLETE!')
    print('=' * 80)
    print(f'  Chunks processed: {total_processed:,}')
    print(f'  Embeddings added: {total_embedded:,}')
    print(f'  Time: {elapsed/60:.1f} minutes')
    print(f'  Rate: {total_processed/elapsed:.1f} chunks/sec')
    print()
    
    # Final verification
    remaining = count_chunks_without_vectors()
    if remaining == 0:
        print('✅ SUCCESS: All chunks now have embeddings!')
    else:
        print(f'⚠️  {remaining:,} chunks still need embeddings')
        print('   Run this script again to continue')
    print('=' * 80)

if __name__ == '__main__':
    main()
