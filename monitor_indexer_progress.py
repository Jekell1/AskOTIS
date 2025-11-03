#!/usr/bin/env python3
"""
Monitor COBOL indexer progress and verify improvements
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def monitor_indexer_progress():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== MONITORING COBOL INDEXER PROGRESS ===")
    print("📊 Watching for improved indexing results...\n")
    
    indexer_name = "cobol-indexer"
    index_name = "cobol-index"
    
    for check in range(10):  # Monitor for up to 5 minutes
        print(f"📊 Check {check + 1}/10:")
        
        try:
            # Check indexer status
            status_url = f'{endpoint}/indexers/{indexer_name}/status?api-version=2023-07-01-preview'
            status_response = requests.get(status_url, headers=headers)
            
            if status_response.status_code == 200:
                status = status_response.json()
                last_result = status.get('lastResult', {})
                current_status = last_result.get('status', 'unknown')
                items_processed = last_result.get('itemsProcessed', 0)
                execution_status = status.get('executionHistory', [{}])[0].get('status', 'unknown')
                
                print(f"   Status: {current_status}")
                print(f"   Items processed: {items_processed:,}" if isinstance(items_processed, int) else f"   Items processed: {items_processed}")
                print(f"   Execution status: {execution_status}")
                
                if current_status == 'inProgress':
                    print("   ⏳ Still running - recreation in progress")
                elif current_status == 'success':
                    print("   ✅ Indexing completed successfully!")
                    break
                elif current_status == 'error':
                    print("   ❌ Indexing failed")
                    break
                elif current_status == 'transientFailure':
                    print("   ⚠️  Temporary issue - will retry")
                
            else:
                print(f"   ❌ Could not get indexer status: {status_response.status_code}")
            
            # Check current document count
            search_url = f'{endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview'
            search_body = {"search": "*", "count": True, "top": 0}
            
            search_response = requests.post(search_url, headers=headers, json=search_body)
            
            if search_response.status_code == 200:
                search_result = search_response.json()
                doc_count = search_result.get('@odata.count', 'unknown')
                
                print(f"   Current document count: {doc_count:,}" if isinstance(doc_count, int) else f"   Current document count: {doc_count}")
                
                if isinstance(doc_count, int):
                    if doc_count > 0:
                        print(f"   📈 Documents are being added - recreation is working!")
                    if doc_count > 4000000:
                        print("   ⚠️  Very high count - may still have duplicate data")
                    elif doc_count > 100000:
                        print("   ⚠️  High count - monitor for duplicates")
                    elif doc_count > 1000:
                        print("   ✅ Reasonable count - looking good!")
                
            else:
                print(f"   ❌ Could not get document count: {search_response.status_code}")
                
        except Exception as e:
            print(f"   ❌ Error: {e}")
        
        if check < 9:
            print("   Waiting 30 seconds for next check...\n")
            time.sleep(30)
        else:
            print()
    
    print("=== MONITORING COMPLETE ===")
    print("🔒 No changes were made - indexer left to run naturally")
    print("📋 Final status check:")
    
    # One final status check
    try:
        status_url = f'{endpoint}/indexers/{indexer_name}/status?api-version=2023-07-01-preview'
        status_response = requests.get(status_url, headers=headers)
        
        if status_response.status_code == 200:
            status = status_response.json()
            last_result = status.get('lastResult', {})
            current_status = last_result.get('status', 'unknown')
            items_processed = last_result.get('itemsProcessed', 0)
            
            print(f"   Final status: {current_status}")
            print(f"   Items processed: {items_processed:,}" if isinstance(items_processed, int) else f"   Items processed: {items_processed}")
            
            if current_status == 'success':
                print("   🎉 Recreation completed successfully!")
            elif current_status == 'inProgress':
                print("   ⏳ Still running - let it continue")
            else:
                print("   📝 Check status later if needed")
        
        # Final document count
        search_response = requests.post(search_url, headers=headers, json=search_body)
        if search_response.status_code == 200:
            search_result = search_response.json()
            doc_count = search_result.get('@odata.count', 'unknown')
            print(f"   Final document count: {doc_count:,}" if isinstance(doc_count, int) else f"   Final document count: {doc_count}")
    
    except Exception as e:
        print(f"   ❌ Final check error: {e}")

if __name__ == "__main__":
    monitor_indexer_progress()
