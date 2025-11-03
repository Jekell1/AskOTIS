"""Robust backfill for flow_edges_v2 with retry logic."""
import os, sys, json, requests, time
from embedding_utils import batch_embed

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_flow_edges_v2'
BATCH_SIZE = 32
PAGE_SIZE = 64

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()
ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

def fetch_missing(skip=0):
    body = {
        'search': '*',
        'top': PAGE_SIZE,
        'skip': skip,
        'select': 'edge_id,edge_text',
        'filter': '(has_vector eq false) or (has_vector eq null)'
    }
    try:
        r = requests.post(
            f'{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}',
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        if r.status_code != 200:
            print(f'Fetch error: {r.status_code}')
            return []
        return r.json().get('value', [])
    except Exception as e:
        print(f'Fetch exception: {e}')
        return []

def upload_batch(docs):
    if not docs:
        return True
    actions = [{'@search.action': 'mergeOrUpload', **d} for d in docs]
    try:
        r = requests.post(
            f'{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}',
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json={'value': actions},
            timeout=30
        )
        if r.status_code not in (200, 201):
            print(f'Upload error: {r.status_code}')
            return False
        return True
    except Exception as e:
        print(f'Upload exception: {e}')
        return False

total_processed = 0
consecutive_failures = 0
max_failures = 5

print(f'Starting flow_edges_v2 embedding backfill (small batches)...')
print(f'Batch size: {BATCH_SIZE}, Page size: {PAGE_SIZE}')

while consecutive_failures < max_failures:
    docs = fetch_missing(skip=0)  # Always skip=0 since we filter for missing
    
    if not docs:
        print(f'✅ No more docs to process!')
        break
    
    print(f'Fetched {len(docs)} docs without embeddings')
    
    texts = [d.get('edge_text', '') for d in docs]
    keys = [d['edge_id'] for d in docs]
    
    try:
        vectors = batch_embed(texts, batch_size=BATCH_SIZE, target_dim=3072)
        consecutive_failures = 0  # Reset on success
    except Exception as e:
        print(f'❌ Embedding error: {e}')
        consecutive_failures += 1
        print(f'Waiting 10 seconds before retry (failure {consecutive_failures}/{max_failures})...')
        time.sleep(10)
        continue
    
    updates = [
        {'edge_id': k, 'edge_vector': v, 'has_vector': True}
        for k, v in zip(keys, vectors)
    ]
    
    if not upload_batch(updates):
        print('❌ Upload failed')
        consecutive_failures += 1
        print(f'Waiting 10 seconds before retry (failure {consecutive_failures}/{max_failures})...')
        time.sleep(10)
        continue
    
    total_processed += len(updates)
    print(f'✅ Processed: {total_processed} docs')
    time.sleep(0.5)  # Small delay between batches

if consecutive_failures >= max_failures:
    print(f'⚠️  Stopped after {max_failures} consecutive failures')
    print(f'Total processed: {total_processed} docs')
else:
    print(f'✅ Complete! Processed {total_processed} total docs')
