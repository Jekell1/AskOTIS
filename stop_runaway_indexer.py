#!/usr/bin/env python3
"""
Stop the runaway indexer and analyze the problem
"""

import json
import requests
import time

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def stop_runaway_indexer():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== STOPPING RUNAWAY INDEXER ===")
    
    # Stop the cobol-jsonl-indexer that's processing 673,800+ items
    indexer_name = "cobol-jsonl-indexer"
    stop_url = f'{endpoint}/indexers/{indexer_name}/stop?api-version=2023-07-01-preview'
    
    try:
        print(f"Stopping {indexer_name}...")
        response = requests.post(stop_url, headers=headers)
        
        if response.status_code == 202:
            print("✅ Stop command sent successfully")
        else:
            print(f"❌ Stop failed: {response.status_code}")
            print(response.text)
        
        # Wait a moment and check status
        print("Waiting 10 seconds for indexer to stop...")
        time.sleep(10)
        
        # Check status
        status_url = f'{endpoint}/indexers/{indexer_name}/status?api-version=2023-07-01-preview'
        status_response = requests.get(status_url, headers=headers)
        
        if status_response.status_code == 200:
            status = status_response.json()
            last_result = status.get('lastResult', {})
            current_status = last_result.get('status', 'unknown')
            items_processed = last_result.get('itemsProcessed', 'unknown')
            
            print(f"Current status: {current_status}")
            print(f"Items processed: {items_processed}")
            
            if current_status == 'inProgress':
                print("⚠️  Indexer is still running - may take time to stop")
            elif current_status in ['success', 'transientFailure']:
                print("✅ Indexer has stopped")
        
    except Exception as e:
        print(f"Error stopping indexer: {e}")
    
    # Also check for other active indexers
    print("\n=== CHECKING OTHER ACTIVE INDEXERS ===")
    try:
        indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
        response = requests.get(indexers_url, headers=headers)
        
        if response.status_code == 200:
            indexers = response.json().get('value', [])
            
            active_indexers = []
            for indexer in indexers:
                name = indexer.get('name', 'unknown')
                if 'cobol' in name.lower():
                    # Get status
                    status_url = f'{endpoint}/indexers/{name}/status?api-version=2023-07-01-preview'
                    status_response = requests.get(status_url, headers=headers)
                    
                    if status_response.status_code == 200:
                        status = status_response.json()
                        last_result = status.get('lastResult', {})
                        current_status = last_result.get('status', 'unknown')
                        
                        if current_status == 'inProgress':
                            active_indexers.append(name)
                            print(f"⚠️  {name} is still active")
            
            if active_indexers:
                print(f"\nFound {len(active_indexers)} active indexers")
                
                user_input = input("\nDo you want to stop all active COBOL indexers? (yes/no): ")
                if user_input.lower() in ['yes', 'y']:
                    for indexer_name in active_indexers:
                        print(f"Stopping {indexer_name}...")
                        stop_url = f'{endpoint}/indexers/{indexer_name}/stop?api-version=2023-07-01-preview'
                        stop_response = requests.post(stop_url, headers=headers)
                        
                        if stop_response.status_code == 202:
                            print(f"✅ Stop command sent for {indexer_name}")
                        else:
                            print(f"❌ Failed to stop {indexer_name}")
            else:
                print("✅ No other active indexers found")
                
    except Exception as e:
        print(f"Error checking indexers: {e}")
    
    print("\n=== RECOMMENDATION ===")
    print("The high document count (4.3M+) is caused by runaway indexing.")
    print("Next steps:")
    print("1. Wait for all indexers to fully stop")
    print("2. Check indexer configuration for loops or incorrect data sources")
    print("3. Consider resetting the index if the count doesn't normalize")
    print("4. Restart indexing with proper controls")

if __name__ == "__main__":
    stop_runaway_indexer()
