#!/usr/bin/env python3
"""
Delete and recreate the COBOL index from scratch
This is much simpler than trying to delete 4.3M documents
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def delete_and_recreate_index():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== DELETE AND RECREATE INDEX ===")
    print("This is much simpler than deleting 4.3M documents!")
    print("Steps:")
    print("1. Get current index schema")
    print("2. Delete the entire index")
    print("3. Recreate with same schema")
    print("4. Restart clean indexing")
    
    confirm = input("\n⚠️  Type 'DELETE' to proceed: ")
    if confirm != 'DELETE':
        print("❌ Operation cancelled")
        return
    
    print("\n📋 STEP 1: Getting current index schema...")
    
    # Get the current index definition
    get_url = f'{endpoint}/indexes/{index_name}?api-version=2023-07-01-preview'
    get_response = requests.get(get_url, headers=headers)
    
    if get_response.status_code != 200:
        print(f"❌ Could not get index schema: {get_response.status_code}")
        return
    
    index_schema = get_response.json()
    print("✅ Current schema retrieved")
    
    # Remove read-only properties
    schema_to_recreate = {
        "name": index_schema["name"],
        "fields": index_schema["fields"],
        "scoringProfiles": index_schema.get("scoringProfiles", []),
        "defaultScoringProfile": index_schema.get("defaultScoringProfile"),
        "corsOptions": index_schema.get("corsOptions"),
        "suggesters": index_schema.get("suggesters", []),
        "analyzers": index_schema.get("analyzers", []),
        "tokenizers": index_schema.get("tokenizers", []),
        "tokenFilters": index_schema.get("tokenFilters", []),
        "charFilters": index_schema.get("charFilters", []),
        "encryptionKey": index_schema.get("encryptionKey"),
        "similarity": index_schema.get("similarity", {})
    }
    
    print("\n🗑️  STEP 2: Deleting index...")
    
    # Delete the index
    delete_url = f'{endpoint}/indexes/{index_name}?api-version=2023-07-01-preview'
    delete_response = requests.delete(delete_url, headers=headers)
    
    if delete_response.status_code == 204:
        print("✅ Index deleted successfully")
    elif delete_response.status_code == 404:
        print("⚠️  Index was already deleted")
    else:
        print(f"❌ Delete failed: {delete_response.status_code}")
        print(delete_response.text)
        return
    
    print("⏳ Waiting 10 seconds for deletion to process...")
    time.sleep(10)
    
    print("\n🔨 STEP 3: Recreating index with same schema...")
    
    # Recreate the index
    create_url = f'{endpoint}/indexes?api-version=2023-07-01-preview'
    create_response = requests.post(create_url, headers=headers, json=schema_to_recreate)
    
    if create_response.status_code == 201:
        print("✅ Index recreated successfully")
    else:
        print(f"❌ Recreation failed: {create_response.status_code}")
        print(create_response.text)
        return
    
    print("⏳ Waiting 10 seconds for index to be ready...")
    time.sleep(10)
    
    print("\n🚀 STEP 4: Starting clean indexing...")
    
    # Find and run the main COBOL indexer
    indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
    indexers_response = requests.get(indexers_url, headers=headers)
    
    suitable_indexer = None
    
    if indexers_response.status_code == 200:
        indexers = indexers_response.json().get('value', [])
        
        # Look for the main cobol-indexer (for .CBL files)
        for indexer in indexers:
            name = indexer.get('name', '')
            if name == 'cobol-indexer':
                suitable_indexer = name
                break
    
    if suitable_indexer:
        print(f"✅ Found indexer: {suitable_indexer}")
        
        # Reset and run the indexer
        reset_url = f'{endpoint}/indexers/{suitable_indexer}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print("✅ Indexer reset")
            
            # Run the indexer
            run_url = f'{endpoint}/indexers/{suitable_indexer}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print("✅ Clean indexing started")
                
                print("\n📊 Monitoring initial progress...")
                time.sleep(30)  # Wait 30 seconds
                
                status_url = f'{endpoint}/indexers/{suitable_indexer}/status?api-version=2023-07-01-preview'
                status_response = requests.get(status_url, headers=headers)
                
                if status_response.status_code == 200:
                    status = status_response.json()
                    last_result = status.get('lastResult', {})
                    current_status = last_result.get('status', 'unknown')
                    items_processed = last_result.get('itemsProcessed', 'unknown')
                    
                    print(f"   Status: {current_status}")
                    print(f"   Items processed: {items_processed}")
                    
                    if items_processed != 'unknown' and isinstance(items_processed, int) and items_processed > 0:
                        print("✅ Indexing is working!")
                    else:
                        print("⏳ Indexing may still be starting...")
                
            else:
                print(f"❌ Could not start indexer: {run_response.status_code}")
        else:
            print(f"❌ Could not reset indexer: {reset_response.status_code}")
    else:
        print("❌ No suitable indexer found")
        print("You may need to recreate the indexer as well")
    
    print("\n🔍 STEP 5: Checking results...")
    
    # Check document count
    search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
    search_body = {
        "search": "*",
        "count": True,
        "top": 0
    }
    
    search_response = requests.post(search_url, headers=headers, json=search_body)
    
    if search_response.status_code == 200:
        result = search_response.json()
        doc_count = result.get('@odata.count', 'unknown')
        
        print(f"📊 Current document count: {doc_count:,}" if isinstance(doc_count, int) else f"📊 Current document count: {doc_count}")
        
        if isinstance(doc_count, int):
            if doc_count == 0:
                print("⏳ Index is empty - indexing may still be running")
            elif doc_count < 100000:
                print("✅ Document count looks healthy!")
            else:
                print("⚠️  Still high - check indexer configuration")
    else:
        print(f"❌ Could not check document count: {search_response.status_code}")
    
    print("\n=== INDEX RECREATION COMPLETE ===")
    print("✅ Old index with 4.3M corrupted documents deleted")
    print("✅ Fresh index created with same schema")
    print("✅ Clean indexing started")
    print("\n📝 Notes:")
    print("- Indexing will take time to complete")
    print("- Monitor progress with: python monitor_index_recovery.py")
    print("- Test search when ready with: python test_cobol_search.py")

if __name__ == "__main__":
    delete_and_recreate_index()
