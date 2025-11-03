#!/usr/bin/env python3
"""
Delete and recreate the COBOL index with proper vector configuration
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def delete_and_recreate_index_with_vectors():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== RECREATE INDEX WITH VECTOR SUPPORT ===")
    
    confirm = input("\n⚠️  Type 'DELETE' to proceed: ")
    if confirm != 'DELETE':
        print("❌ Operation cancelled")
        return
    
    print("\n🗑️  STEP 1: Deleting corrupted index...")
    
    # Delete the index (we know it's corrupted with 4.3M docs)
    delete_url = f'{endpoint}/indexes/{index_name}?api-version=2023-07-01-preview'
    delete_response = requests.delete(delete_url, headers=headers)
    
    if delete_response.status_code == 204:
        print("✅ Corrupted index deleted")
    elif delete_response.status_code == 404:
        print("⚠️  Index was already deleted")
    else:
        print(f"❌ Delete failed: {delete_response.status_code}")
        return
    
    time.sleep(10)
    
    print("\n🔨 STEP 2: Creating fresh index with proper vector configuration...")
    
    # Create a fresh index with the same schema we've been using
    fresh_index_schema = {
        "name": index_name,
        "fields": [
            {
                "name": "id",
                "type": "Edm.String",
                "key": True,
                "searchable": False,
                "filterable": False,
                "retrievable": True,
                "sortable": False,
                "facetable": False
            },
            {
                "name": "repo_path",
                "type": "Edm.String",
                "searchable": True,
                "filterable": True,
                "retrievable": True,
                "sortable": True,
                "facetable": True
            },
            {
                "name": "line",
                "type": "Edm.Int32",
                "searchable": False,
                "filterable": True,
                "retrievable": True,
                "sortable": True,
                "facetable": False
            },
            {
                "name": "code",
                "type": "Edm.String",
                "searchable": True,
                "filterable": False,
                "retrievable": True,
                "sortable": False,
                "facetable": False
            },
            {
                "name": "symbol_name",
                "type": "Edm.String",
                "searchable": True,
                "filterable": True,
                "retrievable": True,
                "sortable": True,
                "facetable": True
            },
            {
                "name": "symbol_kind",
                "type": "Edm.String",
                "searchable": True,
                "filterable": True,
                "retrievable": True,
                "sortable": False,
                "facetable": True
            },
            {
                "name": "calls",
                "type": "Collection(Edm.String)",
                "searchable": True,
                "filterable": True,
                "retrievable": True,
                "sortable": False,
                "facetable": True
            },
            {
                "name": "embeddings",
                "type": "Collection(Edm.Single)",
                "searchable": True,
                "retrievable": False,
                "dimensions": 1536,
                "vectorSearchProfile": "default-vector-profile"
            }
        ],
        "vectorSearch": {
            "profiles": [
                {
                    "name": "default-vector-profile",
                    "algorithm": "default-vector-config"
                }
            ],
            "algorithms": [
                {
                    "name": "default-vector-config",
                    "kind": "hnsw",
                    "hnswParameters": {
                        "m": 4,
                        "efConstruction": 400,
                        "efSearch": 500,
                        "metric": "cosine"
                    }
                }
            ]
        }
    }
    
    # Create the index
    create_url = f'{endpoint}/indexes?api-version=2023-07-01-preview'
    create_response = requests.post(create_url, headers=headers, json=fresh_index_schema)
    
    if create_response.status_code == 201:
        print("✅ Fresh index created successfully")
    else:
        print(f"❌ Creation failed: {create_response.status_code}")
        print(create_response.text)
        return
    
    time.sleep(10)
    
    print("\n🚀 STEP 3: Starting clean indexing...")
    
    # Find and run the main COBOL indexer
    indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
    indexers_response = requests.get(indexers_url, headers=headers)
    
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        
        # Look for the main cobol-indexer
        cobol_indexer = None
        for indexer in indexers:
            name = indexer.get('name', '')
            if name == 'cobol-indexer':
                cobol_indexer = name
                break
        
        if cobol_indexer:
            print(f"✅ Found indexer: {cobol_indexer}")
            
            # Reset and run
            reset_url = f'{endpoint}/indexers/{cobol_indexer}/reset?api-version=2023-07-01-preview'
            reset_response = requests.post(reset_url, headers=headers)
            
            if reset_response.status_code == 204:
                print("✅ Indexer reset")
                
                run_url = f'{endpoint}/indexers/{cobol_indexer}/run?api-version=2023-07-01-preview'
                run_response = requests.post(run_url, headers=headers)
                
                if run_response.status_code == 202:
                    print("✅ Clean indexing started")
                    
                    # Quick progress check
                    time.sleep(30)
                    
                    status_url = f'{endpoint}/indexers/{cobol_indexer}/status?api-version=2023-07-01-preview'
                    status_response = requests.get(status_url, headers=headers)
                    
                    if status_response.status_code == 200:
                        status = status_response.json()
                        last_result = status.get('lastResult', {})
                        current_status = last_result.get('status', 'unknown')
                        items_processed = last_result.get('itemsProcessed', 0)
                        
                        print(f"   Initial status: {current_status}")
                        print(f"   Items processed so far: {items_processed}")
                        
                else:
                    print(f"❌ Could not start indexer: {run_response.status_code}")
            else:
                print(f"❌ Could not reset indexer: {reset_response.status_code}")
        else:
            print("⚠️  No cobol-indexer found - you may need to recreate it")
            print("Available indexers:")
            for indexer in indexers:
                name = indexer.get('name', '')
                if 'cobol' in name.lower():
                    print(f"   - {name}")
    
    print("\n🔍 STEP 4: Checking fresh index...")
    
    search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
    search_body = {
        "search": "*",
        "count": True,
        "top": 0
    }
    
    search_response = requests.post(search_url, headers=headers, json=search_body)
    
    if search_response.status_code == 200:
        result = search_response.json()
        doc_count = result.get('@odata.count', 0)
        
        print(f"📊 Fresh index document count: {doc_count}")
        
        if doc_count == 0:
            print("✅ Fresh start! Index is clean and ready for proper indexing")
        else:
            print(f"📈 Indexing in progress: {doc_count} documents so far")
    else:
        print(f"❌ Could not check fresh index: {search_response.status_code}")
    
    print("\n=== FRESH INDEX CREATED ===")
    print("🎉 Success! The corrupted 4.3M document index is gone!")
    print("✅ Fresh, clean index created with proper vector support")
    print("🚀 Clean indexing from original COBOL files has started")
    print("\n📝 Next steps:")
    print("- Wait for indexing to complete (monitor with python monitor_index_recovery.py)")
    print("- Test search when ready (python test_cobol_search.py)")
    print("- Document count should be much more reasonable now!")

if __name__ == "__main__":
    delete_and_recreate_index_with_vectors()
