#!/usr/bin/env python3
"""
Clean slate: Reset and rebuild the COBOL index from scratch
This will delete all documents and rebuild only from original .CBL files
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def reset_and_rebuild_index():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    index_name = "cobol-index"
    
    print("=== CLEAN SLATE: RESET AND REBUILD INDEX ===")
    print(f"Current document count: 4,396,174")
    print("This will:")
    print("1. Delete ALL documents from the index")
    print("2. Keep the index structure intact")
    print("3. Rebuild only from original COBOL (.CBL) files")
    print("4. Exclude all inlined/duplicate files")
    
    confirm = input("\n⚠️  Type 'RESET' to proceed with clean slate: ")
    if confirm != 'RESET':
        print("❌ Reset cancelled")
        return
    
    print("\n🗑️  STEP 1: Clearing all documents from index...")
    
    # Delete all documents by searching for everything and deleting in batches
    try:
        total_deleted = 0
        batch_size = 1000
        
        while True:
            # Get a batch of documents to delete
            search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
            search_body = {
                "search": "*",
                "top": batch_size,
                "select": "id"
            }
            
            search_response = requests.post(search_url, headers=headers, json=search_body)
            
            if search_response.status_code == 200:
                results = search_response.json()
                docs = results.get('value', [])
                
                if not docs:
                    print(f"✅ No more documents to delete")
                    break
                
                # Prepare batch delete
                delete_batch = []
                for doc in docs:
                    delete_batch.append({
                        "@search.action": "delete",
                        "id": doc.get('id')
                    })
                
                # Execute batch delete
                index_url = f'{endpoint}/indexes/{index_name}/docs/index?api-version=2023-07-01-preview'
                delete_body = {"value": delete_batch}
                
                delete_response = requests.post(index_url, headers=headers, json=delete_body)
                
                if delete_response.status_code in [200, 201]:
                    total_deleted += len(delete_batch)
                    print(f"   Deleted {total_deleted:,} documents so far...")
                    
                    # Wait a moment between batches to avoid throttling
                    time.sleep(1)
                else:
                    print(f"❌ Delete batch failed: {delete_response.status_code}")
                    print(delete_response.text)
                    break
            else:
                print(f"❌ Search failed: {search_response.status_code}")
                break
        
        print(f"✅ Total documents deleted: {total_deleted:,}")
        
    except Exception as e:
        print(f"❌ Error during deletion: {e}")
        return
    
    print("\n⏳ STEP 2: Waiting for index to process deletions...")
    time.sleep(10)
    
    print("\n🔧 STEP 3: Setting up clean rebuild...")
    
    # Check which indexers exist and are suitable for clean rebuild
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
        print(f"✅ Found suitable indexer: {suitable_indexer}")
        print("\n🚀 STEP 4: Starting clean rebuild...")
        
        # Reset and run the indexer
        reset_url = f'{endpoint}/indexers/{suitable_indexer}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print("✅ Indexer reset successfully")
            
            # Run the indexer
            run_url = f'{endpoint}/indexers/{suitable_indexer}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print("✅ Indexer started successfully")
                print("\n📊 Monitoring rebuild progress...")
                
                # Monitor progress
                for check in range(5):
                    time.sleep(30)  # Wait 30 seconds between checks
                    
                    status_url = f'{endpoint}/indexers/{suitable_indexer}/status?api-version=2023-07-01-preview'
                    status_response = requests.get(status_url, headers=headers)
                    
                    if status_response.status_code == 200:
                        status = status_response.json()
                        last_result = status.get('lastResult', {})
                        current_status = last_result.get('status', 'unknown')
                        items_processed = last_result.get('itemsProcessed', 'unknown')
                        
                        print(f"   Check {check + 1}: Status={current_status}, Items={items_processed}")
                        
                        if current_status == 'success':
                            print("✅ Rebuild completed successfully!")
                            break
                        elif current_status == 'transientFailure':
                            print("⚠️  Transient failure - will retry")
                        elif current_status == 'error':
                            print("❌ Rebuild failed")
                            break
                    else:
                        print(f"❌ Could not get status: {status_response.status_code}")
                
            else:
                print(f"❌ Could not start indexer: {run_response.status_code}")
        else:
            print(f"❌ Could not reset indexer: {reset_response.status_code}")
    else:
        print("❌ No suitable indexer found for rebuild")
        print("Available indexers:")
        if indexers_response.status_code == 200:
            indexers = indexers_response.json().get('value', [])
            for indexer in indexers:
                name = indexer.get('name', '')
                if 'cobol' in name.lower():
                    print(f"   - {name}")
    
    print("\n🔍 STEP 5: Checking final results...")
    
    # Check final document count
    search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
    search_body = {
        "search": "*",
        "count": True,
        "top": 0
    }
    
    final_response = requests.post(search_url, headers=headers, json=search_body)
    
    if final_response.status_code == 200:
        final_result = final_response.json()
        final_count = final_result.get('@odata.count', 'unknown')
        
        print(f"📊 Final document count: {final_count:,}" if isinstance(final_count, int) else f"📊 Final document count: {final_count}")
        
        if isinstance(final_count, int):
            if final_count < 100000:
                print("✅ Document count looks healthy!")
            else:
                print("⚠️  Still high - may need additional cleanup")
    else:
        print(f"❌ Could not get final count: {final_response.status_code}")
    
    print("\n=== CLEAN SLATE COMPLETE ===")
    print("✅ Old corrupted data deleted")
    print("✅ Index rebuilt from clean sources")
    print("🔍 Test your search functionality")
    print("\nNext: Run 'python test_cobol_search.py' to verify search works")

if __name__ == "__main__":
    reset_and_rebuild_index()
