#!/usr/bin/env python3
"""
Backfill embeddings for menu_trees index - simplified version.
Processes in small batches to avoid timeouts.
"""
import os, json, requests, time
from openai import AzureOpenAI

API_VERSION = '2025-08-01-preview'
INDEX_NAME = 'new_cobol_menu_trees'
BATCH_SIZE = 100  # Process 100 docs at a time

def load_settings():
    """Load credentials"""
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k, v in vals.items():
            if k not in os.environ:
                os.environ[k] = str(v)
    except Exception as e:
        print(f"Warning: {e}")

def main():
    print("=" * 70)
    print("MENU TREES EMBEDDING BACKFILL (BATCH MODE)")
    print("=" * 70)
    
    load_settings()
    
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    client = AzureOpenAI(
        azure_endpoint=os.getenv('AZURE_OPENAI_ENDPOINT'),
        api_key=os.getenv('AZURE_OPENAI_KEY') or os.getenv('AZURE_OPENAI_API_KEY'),
        api_version='2024-02-01'
    )
    
    deployment = (os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 
                  os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or
                  'text-embedding-3-large')
    
    # Check how many docs need embedding
    print("\nChecking status...")
    r = requests.post(
        f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True}
    )
    total = r.json().get('@odata.count', 0)
    
    r2 = requests.post(
        f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True, 'filter': 'has_vector eq true'}
    )
    with_vec = r2.json().get('@odata.count', 0)
    
    remaining = total - with_vec
    print(f"Total docs: {total}")
    print(f"With vectors: {with_vec}")
    print(f"Remaining: {remaining}")
    
    if remaining == 0:
        print("\n[OK] All documents already have embeddings!")
        return
    
    # Process in batches
    print(f"\nProcessing {remaining} documents in batches of {BATCH_SIZE}...")
    
    total_processed = 0
    batch_num = 0
    start_time = time.time()
    
    while True:
        batch_num += 1
        print(f"\n--- Batch {batch_num} ---")
        
        # Fetch next batch without vectors
        try:
            r = requests.post(
                f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}",
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json={
                    'search': '*',
                    'select': 'root_program_id,tree_json',
                    'filter': 'has_vector eq false or has_vector eq null',
                    'top': BATCH_SIZE
                },
                timeout=30
            )
        except Exception as e:
            print(f"  Fetch error: {e}, retrying...")
            time.sleep(2)
            continue
        
        if r.status_code != 200:
            print(f"  ERROR: {r.status_code}")
            break
        
        docs = r.json().get('value', [])
        if not docs:
            print("  No more documents!")
            break
        
        print(f"  Fetched {len(docs)} docs")
        
        # Prepare texts
        texts = []
        for doc in docs:
            text = f"Program: {doc['root_program_id']}"
            tree = doc.get('tree_json', '')
            if tree:
                tree = tree[:2000] if len(tree) > 2000 else tree
                text += f" | Tree: {tree}"
            texts.append(text)
        
        # Generate embeddings
        print(f"  Generating embeddings...")
        try:
            response = client.embeddings.create(input=texts, model=deployment)
            embeddings = [item.embedding for item in response.data]
        except Exception as e:
            print(f"  Embedding error: {e}, retrying...")
            time.sleep(2)
            continue
        
        # Upload
        print(f"  Uploading {len(embeddings)} vectors...")
        actions = []
        for doc, emb in zip(docs, embeddings):
            actions.append({
                "@search.action": "merge",
                "root_program_id": doc['root_program_id'],
                "content_vector": emb,
                "has_vector": True
            })
        
        try:
            r = requests.post(
                f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}",
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json={'value': actions},
                timeout=60
            )
            
            if r.status_code in (200, 201):
                result = r.json()
                succeeded = len([x for x in result.get('value', []) if x.get('status')])
                print(f"  Uploaded: {succeeded}/{len(actions)}")
                total_processed += succeeded
            else:
                print(f"  Upload error: {r.status_code}")
                print(f"  {r.text[:200]}")
        except Exception as e:
            print(f"  Upload error: {e}")
            continue
        
        # Progress
        elapsed = time.time() - start_time
        rate = total_processed / elapsed if elapsed > 0 else 0
        print(f"  Progress: {total_processed}/{remaining} ({rate:.1f} docs/sec)")
        
        # Small delay between batches
        time.sleep(0.5)
    
    # Final status
    print("\n" + "=" * 70)
    print(f"COMPLETED: Processed {total_processed} documents")
    print("=" * 70)
    
    # Verify
    print("\nVerifying...")
    time.sleep(2)
    r = requests.post(
        f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True, 'filter': 'has_vector eq true'}
    )
    final_count = r.json().get('@odata.count', 0)
    print(f"Documents with vectors: {final_count}/{total}")
    
    if final_count == total:
        print("\n[OK] 100% embedding coverage achieved!")
    else:
        remaining = total - final_count
        print(f"\n[WARN] {remaining} documents still without vectors")
        print("Run script again to process remaining documents")

if __name__ == '__main__':
    main()
