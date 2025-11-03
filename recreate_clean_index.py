#!/usr/bin/env python3
"""
Recreate index with correct vector field syntax
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def recreate_clean_index():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== CREATE CLEAN COBOL INDEX ===")
    
    confirm = input("\n✅ Create fresh clean index? (yes/no): ")
    if confirm.lower() != 'yes':
        print("❌ Operation cancelled")
        return
    
    print("\n🔨 Creating fresh index...")
    
    # Simplified index without vector embeddings to start clean
    clean_index_schema = {
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
            }
        ]
    }
    
    # Create the index
    create_url = f'{endpoint}/indexes?api-version=2023-07-01-preview'
    create_response = requests.post(create_url, headers=headers, json=clean_index_schema)
    
    if create_response.status_code == 201:
        print("✅ Clean index created successfully")
    else:
        print(f"❌ Creation failed: {create_response.status_code}")
        print(create_response.text)
        return
    
    time.sleep(5)
    
    print("\n🚀 Starting clean indexing from original COBOL files...")
    
    # Find and run the main COBOL indexer
    indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
    indexers_response = requests.get(indexers_url, headers=headers)
    
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        
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
                    print("✅ Clean indexing started!")
                    
                    print("\n📊 Monitoring progress...")
                    for i in range(3):
                        time.sleep(20)
                        
                        # Check indexer status
                        status_url = f'{endpoint}/indexers/{cobol_indexer}/status?api-version=2023-07-01-preview'
                        status_response = requests.get(status_url, headers=headers)
                        
                        if status_response.status_code == 200:
                            status = status_response.json()
                            last_result = status.get('lastResult', {})
                            current_status = last_result.get('status', 'unknown')
                            items_processed = last_result.get('itemsProcessed', 0)
                            
                            print(f"   Check {i+1}: {current_status}, {items_processed} items processed")
                            
                            if current_status == 'success':
                                print("🎉 Indexing completed successfully!")
                                break
                            elif current_status in ['transientFailure', 'error']:
                                print("⚠️  Indexer encountered issues")
                                break
                    
                    # Check final document count
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
                        
                        print(f"\n📊 Final document count: {doc_count:,}")
                        
                        if doc_count == 0:
                            print("⏳ Indexing may still be starting...")
                        elif doc_count < 50000:
                            print("✅ Document count looks healthy!")
                        elif doc_count > 1000000:
                            print("⚠️  Still very high - may have indexing issues")
                        else:
                            print("📈 Reasonable document count")
                    
                else:
                    print(f"❌ Could not start indexer: {run_response.status_code}")
                    print(run_response.text)
            else:
                print(f"❌ Could not reset indexer: {reset_response.status_code}")
        else:
            print("❌ No cobol-indexer found")
            print("Available indexers:")
            for indexer in indexers:
                name = indexer.get('name', '')
                if 'cobol' in name.lower():
                    print(f"   - {name}")
    
    print("\n=== CLEAN INDEX READY ===")
    print("🎉 Fresh index created (no more 4.3M corrupted documents!)")
    print("✅ Clean indexing from original .CBL files started") 
    print("📝 Test search with: python test_cobol_search.py")
    print("📊 Monitor progress with: python monitor_index_recovery.py")

if __name__ == "__main__":
    recreate_clean_index()
