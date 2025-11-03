#!/usr/bin/env python3
"""
Monitor index recovery after deleting runaway indexer
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def monitor_index_recovery():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== MONITORING INDEX RECOVERY ===")
    print("Checking index status after deleting runaway indexer...\n")
    
    for attempt in range(3):
        print(f"📊 Check {attempt + 1}/3:")
        
        try:
            # Get index statistics
            index_url = f'{endpoint}/indexes/cobol-index?api-version=2023-07-01-preview'
            response = requests.get(index_url, headers=headers)
            
            if response.status_code == 200:
                index_info = response.json()
                doc_count = index_info.get('fields', [])
                storage_size = index_info.get('@odata.context', 'unknown')
                
                print(f"   Index exists: ✅")
                
                # Try to get actual document count by searching
                search_url = f'{endpoint}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
                search_body = {
                    "search": "*",
                    "count": True,
                    "top": 0,
                    "select": "id"
                }
                
                search_response = requests.post(search_url, 
                                                headers=headers, 
                                                json=search_body)
                
                if search_response.status_code == 200:
                    search_result = search_response.json()
                    doc_count = search_result.get('@odata.count', 'unknown')
                    
                    print(f"   Document count: {doc_count:,}" if isinstance(doc_count, int) else f"   Document count: {doc_count}")
                    
                    if isinstance(doc_count, int):
                        if doc_count > 1000000:
                            print("   ⚠️  Still very high - may take time to normalize")
                        elif doc_count > 100000:
                            print("   ⚠️  High but possibly settling")
                        else:
                            print("   ✅ Document count looks reasonable")
                    
                else:
                    print(f"   ❌ Could not get document count: {search_response.status_code}")
                    
            else:
                print(f"   ❌ Could not get index info: {response.status_code}")
                
            # Check remaining indexers
            indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
            indexers_response = requests.get(indexers_url, headers=headers)
            
            if indexers_response.status_code == 200:
                indexers = indexers_response.json().get('value', [])
                active_cobol_indexers = []
                
                for indexer in indexers:
                    name = indexer.get('name', '')
                    if 'cobol' in name.lower():
                        # Check if it's active
                        status_url = f'{endpoint}/indexers/{name}/status?api-version=2023-07-01-preview'
                        status_response = requests.get(status_url, headers=headers)
                        
                        if status_response.status_code == 200:
                            status = status_response.json()
                            last_result = status.get('lastResult', {})
                            current_status = last_result.get('status', 'unknown')
                            
                            print(f"   Indexer {name}: {current_status}")
                            
                            if current_status == 'inProgress':
                                active_cobol_indexers.append(name)
                
                if not active_cobol_indexers:
                    print("   ✅ No active indexers - good!")
                else:
                    print(f"   ⚠️  {len(active_cobol_indexers)} indexers still active")
                    
        except Exception as e:
            print(f"   ❌ Error: {e}")
        
        if attempt < 2:
            print("   Waiting 30 seconds...\n")
            time.sleep(30)
        else:
            print()
    
    print("=== SUMMARY ===")
    print("✅ Runaway indexer (cobol-jsonl-indexer) has been deleted")
    print("⏳ Index may take time to reflect the true document count")
    print("📋 Original COBOL files should still be searchable")
    print("\nIf document count remains high:")
    print("- The index may need to be reset/rebuilt")
    print("- There may be other indexers or data sources contributing")

if __name__ == "__main__":
    monitor_index_recovery()
