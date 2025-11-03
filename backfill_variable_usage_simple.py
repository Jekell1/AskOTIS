"""Simple backfill for variable_usage embeddings using embedding_utils."""
import os, sys, json, requests, time
from embedding_utils import batch_embed

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_variable_usage'
BATCH_SIZE = 64
PAGE_SIZE = 128

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
        'select': 'symbol_id_global,symbol_name,program_id,data_type,read_count,write_count,param_in_count,param_out_count,first_write_program,first_write_location',
        'filter': '(has_vector eq false) or (has_vector eq null)'
    }
    r = requests.post(
        f'{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}',
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=60
    )
    if r.status_code != 200:
        print(f'Fetch error: {r.status_code} {r.text[:200]}')
        return []
    return r.json().get('value', [])

def upload_batch(docs):
    if not docs:
        return True
    actions = [{'@search.action': 'mergeOrUpload', **d} for d in docs]
    r = requests.post(
        f'{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}',
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'value': actions},
        timeout=60
    )
    if r.status_code not in (200, 201):
        print(f'Upload error: {r.status_code} {r.text[:300]}')
        return False
    return True

def make_text(doc):
    name = doc.get('symbol_name', '')
    prog = doc.get('program_id', '')
    dtype = doc.get('data_type', '')
    reads = doc.get('read_count', 0)
    writes = doc.get('write_count', 0)
    pin = doc.get('param_in_count', 0)
    pout = doc.get('param_out_count', 0)
    fw_prog = doc.get('first_write_program', '')
    fw_loc = doc.get('first_write_location', '')
    return f"Variable: {name} in Program: {prog}\nType: {dtype}\nReads: {reads} Writes: {writes}\nParams In: {pin} Out: {pout}\nFirst Write: {fw_prog} @ {fw_loc}"

total_processed = 0
print(f'Starting variable_usage embedding backfill...')

while True:
    docs = fetch_missing(skip=0)  # Always skip=0 since we're filtering for missing
    if not docs:
        print('No more docs to process')
        break
    
    print(f'Fetched {len(docs)} docs without embeddings')
    
    texts = [make_text(d) for d in docs]
    keys = [d['symbol_id_global'] for d in docs]
    
    try:
        vectors = batch_embed(texts, batch_size=BATCH_SIZE, target_dim=3072)
    except Exception as e:
        print(f'Embedding error: {e}')
        time.sleep(5)
        continue
    
    updates = [
        {'symbol_id_global': k, 'usage_summary_vector': v, 'has_vector': True}
        for k, v in zip(keys, vectors)
    ]
    
    if not upload_batch(updates):
        print('Upload failed, waiting before retry...')
        time.sleep(5)
        continue
    
    total_processed += len(updates)
    print(f'Processed: {total_processed} docs')
    time.sleep(0.1)

print(f'✅ Complete! Processed {total_processed} total docs')
