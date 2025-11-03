#!/usr/bin/env python3
"""
Quick check of indexer status after the fix
"""

import json
import requests

def check_indexer_status():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        LOCAL_SETTINGS = settings.get('Values', {})

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')

    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    
    # Check indexer status
    status_url = f'{SEARCH_ENDPOINT}/indexers/cobol-indexer/status?api-version=2023-07-01-preview'
    
    try:
        response = requests.get(status_url, headers=headers)
        if response.status_code == 200:
            status = response.json()
            
            print("=== INDEXER STATUS ===")
            print(f"Overall Status: {status.get('status', 'unknown')}")
            
            execution_history = status.get('executionHistory', [])
            if execution_history:
                latest = execution_history[0]
                print(f"Latest Run Status: {latest.get('status', 'unknown')}")
                print(f"Items Processed: {latest.get('itemsProcessed', 0)}")
                print(f"Items Failed: {latest.get('itemsFailed', 0)}")
                
                if latest.get('status') == 'success':
                    # Check index document count
                    stats_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/stats?api-version=2023-07-01-preview'
                    stats_response = requests.get(stats_url, headers=headers)
                    
                    if stats_response.status_code == 200:
                        stats = stats_response.json()
                        print(f"\n=== INDEX STATS ===")
                        print(f"Total Documents: {stats.get('documentCount', 0):,}")
                        print(f"Storage Size: {stats.get('storageSize', 0):,} bytes")
                elif latest.get('status') == 'inProgress':
                    print("Indexer is still running...")
                elif latest.get('status') == 'failure':
                    print(f"Error: {latest.get('errorMessage', 'No details')}")
            else:
                print("No execution history found")
                
        else:
            print(f"Failed to get status: {response.status_code}")
            
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    check_indexer_status()
