"""Backfill embeddings for symbol_refs documents without vectors.

Processes symbol_refs in batches, generates embeddings for excerpt_text,
and uploads with has_vector=true flag.

Usage:
  python backfill_symbol_refs_embeddings.py --batch 100
"""
import os, sys, json, time, requests
from openai import AzureOpenAI

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_symbol_refs'
BATCH_SIZE = 100

def load_settings():
    vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
    for k, v in vals.items():
        if k not in os.environ:
            os.environ[k] = str(v)

def resolve():
    ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[ERROR] Missing search endpoint/key')
        sys.exit(1)
    return ep, key

def get_openai_client():
    """Get OpenAI client for embeddings."""
    api_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_ENDPOINT')
    
    if not api_key or not endpoint:
        print('[ERROR] Missing OpenAI credentials')
        sys.exit(1)
    
    return AzureOpenAI(
        api_key=api_key,
        api_version='2024-08-01-preview',
        azure_endpoint=endpoint
    )

def fetch_batch_without_vectors(ep, key, skip=0):
    """Fetch documents where has_vector != true."""
    body = {
        'search': '*',
        'filter': 'has_vector ne true',
        'select': 'ref_id,excerpt_text',
        'top': BATCH_SIZE,
        'skip': skip
    }
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=30
    )
    if r.status_code != 200:
        print(f'[ERROR] Fetch failed: {r.status_code} {r.text[:200]}')
        return []
    return r.json().get('value', [])

def generate_embeddings(client, texts):
    """Generate embeddings for a batch of texts (1536d for symbol_refs)."""
    # Use text-embedding-3-large with dimensions=1536 to match index schema
    deployment = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large'
    
    try:
        response = client.embeddings.create(
            input=texts,
            model=deployment,
            dimensions=1536  # Reduce to 1536d to match symbol_refs index
        )
        return [item.embedding for item in response.data]
    except Exception as e:
        print(f'[ERROR] Embedding generation failed: {e}')
        return []

def upload_vectors(ep, key, docs):
    """Upload documents with vectors."""
    if not docs:
        return
    
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'value': docs},
        timeout=60
    )
    if r.status_code not in (200, 201):
        print(f'[ERROR] Upload failed: {r.status_code} {r.text[:300]}')
        sys.exit(1)

def main():
    load_settings()
    ep, key = resolve()
    client = get_openai_client()
    
    print('[1/4] Checking symbol_refs status...')
    
    # Get total count needing embeddings
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'filter': 'has_vector ne true', 'top': 0, 'count': True}
    )
    remaining = r.json().get('@odata.count', 0)
    print(f'      Documents needing embeddings: {remaining:,}')
    
    if remaining == 0:
        print('[OK] All symbol_refs already have embeddings!')
        return
    
    print(f'[2/4] Generating embeddings for {remaining:,} documents...')
    
    processed = 0
    start = time.time()
    skip = 0
    
    while True:
        # Fetch batch
        docs = fetch_batch_without_vectors(ep, key, skip)
        if not docs:
            print('      No more documents to process')
            break
        
        # Generate embeddings
        texts = [doc.get('excerpt_text', '') or f"Symbol ref in {doc.get('ref_id', '')}" for doc in docs]
        vectors = generate_embeddings(client, texts)
        
        if not vectors or len(vectors) != len(docs):
            print(f'[WARN] Embedding count mismatch: {len(vectors)} vs {len(docs)}')
            skip += BATCH_SIZE
            continue
        
        # Prepare upload documents
        upload_docs = []
        for doc, vector in zip(docs, vectors):
            upload_docs.append({
                '@search.action': 'merge',
                'ref_id': doc['ref_id'],
                'excerpt_vector': vector,
                'has_vector': True
            })
        
        # Upload
        upload_vectors(ep, key, upload_docs)
        processed += len(upload_docs)
        
        elapsed = time.time() - start
        rate = processed / elapsed if elapsed > 0 else 0
        print(f'      Progress: {processed:,}/{remaining:,} ({rate:.1f} docs/sec)')
        
        # Don't increment skip - we're filtering by has_vector ne true
        # so processed docs disappear from results
    
    elapsed = time.time() - start
    print(f'\n[3/4] Verifying...')
    
    r = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'filter': 'has_vector eq true', 'top': 0, 'count': True}
    )
    embedded_count = r.json().get('@odata.count', 0)
    
    r2 = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True}
    )
    total_count = r2.json().get('@odata.count', 0)
    
    print(f'      Documents with vectors: {embedded_count:,} / {total_count:,}')
    print(f'      Embedding coverage: {embedded_count/total_count*100:.1f}%')
    
    print(f'\n[4/4] COMPLETE')
    print(f'      Processed: {processed:,} documents')
    print(f'      Time: {elapsed:.1f}s ({rate:.1f} docs/sec)')

if __name__ == '__main__':
    main()
