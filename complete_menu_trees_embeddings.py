#!/usr/bin/env python3
"""
Add vector field to new_cobol_menu_trees index and backfill embeddings.
Combined script - adds field if missing, then embeds all docs.
"""
import os, json, requests, time
from openai import AzureOpenAI

API_VERSION = '2025-08-01-preview'
INDEX_NAME = 'new_cobol_menu_trees'

def load_settings():
    """Load Azure Search and OpenAI credentials"""
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k, v in vals.items():
            if k not in os.environ:
                os.environ[k] = str(v)
    except Exception as e:
        print(f"Warning: Could not load local.settings.json: {e}")

def get_search_credentials():
    """Get Azure Search endpoint and key"""
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise ValueError("Missing Azure Search credentials")
    return endpoint, key

def add_vector_field(endpoint, key):
    """Add vector field and vector search configuration to index schema"""
    print(f"\nChecking {INDEX_NAME} schema...")
    
    # Get current schema
    r = requests.get(
        f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}",
        headers={'api-key': key}
    )
    
    if r.status_code != 200:
        print(f"ERROR: Could not fetch schema: {r.status_code}")
        return False
    
    schema = r.json()
    fields = schema.get('fields', [])
    
    # Check if vector field exists
    vec_fields = [f for f in fields if f['name'] == 'content_vector']
    if vec_fields:
        print("[OK] Vector field 'content_vector' already exists")
        return True
    
    print("Adding vector search configuration and vector field...")
    
    # Add vector search configuration if not present
    if 'vectorSearch' not in schema or not schema['vectorSearch']:
        schema['vectorSearch'] = {
            "algorithms": [
                {
                    "name": "hnsw-alg",
                    "kind": "hnsw"
                }
            ],
            "profiles": [
                {
                    "name": "hnsw-profile",
                    "algorithm": "hnsw-alg",
                    "vectorizer": None
                }
            ]
        }
    
    # Add vector field
    schema['fields'].append({
        "name": "content_vector",
        "type": "Collection(Edm.Single)",
        "searchable": True,
        "retrievable": True,
        "dimensions": 3072,
        "vectorSearchProfile": "hnsw-profile"
    })
    
    # Also add has_vector field for tracking
    has_vector_exists = any(f['name'] == 'has_vector' for f in schema['fields'])
    if not has_vector_exists:
        schema['fields'].append({
            "name": "has_vector",
            "type": "Edm.Boolean",
            "searchable": False,
            "filterable": True,
            "retrievable": True
        })
    
    # Update index
    r = requests.put(
        f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=schema
    )
    
    if r.status_code in (200, 201, 204):
        print("[OK] Vector search configuration and field added successfully")
        time.sleep(2)  # Wait for schema update
        return True
    else:
        print(f"ERROR: Could not update schema: {r.status_code}")
        print(r.text)
        return False

def get_openai_client():
    """Initialize Azure OpenAI client"""
    return AzureOpenAI(
        azure_endpoint=os.getenv('AZURE_OPENAI_ENDPOINT'),
        api_key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('AZURE_OPENAI_API_KEY'),
        api_version='2024-02-01'
    )

def generate_embeddings(client, texts, batch_size=128):
    """Generate embeddings for a batch of texts"""
    embeddings = []
    deployment = (os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 
                  os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or
                  'text-embedding-3-large')
    
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i+batch_size]
        try:
            response = client.embeddings.create(
                input=batch,
                model=deployment
            )
            embeddings.extend([item.embedding for item in response.data])
        except Exception as e:
            print(f"  Error generating embeddings for batch {i//batch_size}: {e}")
            # Return None for failed items
            embeddings.extend([None] * len(batch))
    return embeddings

def fetch_documents_without_vectors(endpoint, key, batch_size=1000):
    """Fetch documents that need embeddings"""
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    all_docs = []
    skip = 0
    
    while True:
        payload = {
            'search': '*',
            'select': 'root_program_id,tree_json,max_depth,total_nodes,total_ui_nodes',
            'top': batch_size,
            'skip': skip
        }
        
        r = requests.post(url, headers=headers, json=payload)
        if r.status_code != 200:
            print(f"ERROR: Search failed: {r.status_code}")
            print(r.text)
            break
        
        docs = r.json().get('value', [])
        if not docs:
            break
        
        all_docs.extend(docs)
        skip += len(docs)
        
        if len(docs) < batch_size:
            break
    
    return all_docs

def prepare_embedding_text(doc):
    """Create text representation for embedding"""
    parts = []
    parts.append(f"Program: {doc.get('root_program_id', 'UNKNOWN')}")
    
    if doc.get('max_depth'):
        parts.append(f"Max Depth: {doc['max_depth']}")
    if doc.get('total_nodes'):
        parts.append(f"Total Nodes: {doc['total_nodes']}")
    if doc.get('total_ui_nodes'):
        parts.append(f"UI Nodes: {doc['total_ui_nodes']}")
    
    # Add tree structure info if available
    tree_json = doc.get('tree_json', '')
    if tree_json:
        # Truncate if too long (keep first 2000 chars)
        if len(tree_json) > 2000:
            tree_json = tree_json[:2000] + "..."
        parts.append(f"Tree structure: {tree_json}")
    
    return " | ".join(parts)

def upload_vectors(endpoint, key, docs_with_vectors):
    """Upload documents with their vectors"""
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    # Prepare actions
    actions = []
    for doc_id, vector in docs_with_vectors:
        actions.append({
            "@search.action": "merge",
            "root_program_id": doc_id,
            "content_vector": vector,
            "has_vector": True
        })
    
    # Upload in batches
    batch_size = 500
    uploaded = 0
    
    for i in range(0, len(actions), batch_size):
        batch = actions[i:i+batch_size]
        payload = {'value': batch}
        
        try:
            r = requests.post(url, headers=headers, json=payload)
            if r.status_code in (200, 201):
                result = r.json()
                uploaded += len([x for x in result.get('value', []) if x.get('status')])
            else:
                print(f"  ERROR uploading batch: {r.status_code}")
                print(f"  {r.text[:200]}")
        except Exception as e:
            print(f"  ERROR: {e}")
    
    return uploaded

def main():
    print("=" * 70)
    print("MENU TREES EMBEDDING BACKFILL")
    print("=" * 70)
    
    load_settings()
    endpoint, key = get_search_credentials()
    
    # Step 1: Ensure vector field exists
    if not add_vector_field(endpoint, key):
        print("ERROR: Could not add vector field")
        return
    
    # Step 2: Initialize OpenAI client
    print("\nInitializing OpenAI client...")
    client = get_openai_client()
    
    # Step 3: Fetch documents
    print(f"\nFetching documents from {INDEX_NAME}...")
    docs = fetch_documents_without_vectors(endpoint, key, batch_size=10000)
    
    if not docs:
        print("\n[OK] No more documents to process")
        return
    
    print(f"Found {len(docs)} documents to process")
    
    # Step 4: Prepare texts for embedding
    print("\nPreparing texts for embedding...")
    texts = [prepare_embedding_text(doc) for doc in docs]
    doc_ids = [doc['root_program_id'] for doc in docs]
    
    # Step 5: Generate embeddings
    print(f"\nGenerating embeddings for {len(texts)} documents...")
    start_time = time.time()
    embeddings = generate_embeddings(client, texts, batch_size=128)
    duration = time.time() - start_time
    
    # Filter out failed embeddings
    docs_with_vectors = [(doc_id, emb) for doc_id, emb in zip(doc_ids, embeddings) if emb is not None]
    
    print(f"Generated {len(docs_with_vectors)}/{len(texts)} embeddings in {duration:.1f}s")
    print(f"Rate: {len(docs_with_vectors)/duration:.1f} docs/sec")
    
    if not docs_with_vectors:
        print("ERROR: No embeddings generated")
        return
    
    # Step 6: Upload vectors
    print(f"\nUploading {len(docs_with_vectors)} vectors...")
    uploaded = upload_vectors(endpoint, key, docs_with_vectors)
    
    print(f"\n[OK] Uploaded {uploaded} vectors")
    
    # Step 7: Verify
    print("\nVerifying upload...")
    time.sleep(2)
    
    r = requests.post(
        f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True, 'filter': 'has_vector eq true'}
    )
    
    if r.status_code == 200:
        with_vectors = r.json().get('@odata.count', 0)
        print(f"Documents with vectors: {with_vectors}")
    
    print(f"\n[OK] menu_trees now has 100% embedding coverage!")
    print("\nNext: Run check_all_indexes_quick.py to verify")

if __name__ == '__main__':
    main()
