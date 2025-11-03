#!/usr/bin/env python3
"""
Investigate the runaway indexer configuration
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def investigate_indexer_config():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== INVESTIGATING RUNAWAY INDEXER ===")
    
    indexer_name = "cobol-jsonl-indexer"
    
    # Get indexer configuration
    config_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
    
    try:
        print(f"Getting configuration for {indexer_name}...")
        response = requests.get(config_url, headers=headers)
        
        if response.status_code == 200:
            config = response.json()
            
            print(f"Indexer Name: {config.get('name', 'unknown')}")
            print(f"Data Source: {config.get('dataSourceName', 'unknown')}")
            print(f"Target Index: {config.get('targetIndexName', 'unknown')}")
            print(f"Schedule: {config.get('schedule', 'none')}")
            print(f"Parameters: {config.get('parameters', {})}")
            
            # Check if there's a schedule causing continuous running
            schedule = config.get('schedule')
            if schedule:
                print(f"\n⚠️  SCHEDULE DETECTED:")
                print(f"   Interval: {schedule.get('interval', 'unknown')}")
                print(f"   Start Time: {schedule.get('startTime', 'unknown')}")
                print("   This may be causing continuous reindexing!")
            
            # Check parameters for any problematic settings
            params = config.get('parameters', {})
            if params:
                print(f"\nParameters:")
                for key, value in params.items():
                    print(f"   {key}: {value}")
                    
                # Check for batch size or other settings that might cause issues
                batch_size = params.get('batchSize')
                if batch_size:
                    print(f"\n   Batch size: {batch_size}")
                    
                max_failed_items = params.get('maxFailedItems')
                if max_failed_items:
                    print(f"   Max failed items: {max_failed_items}")
            
            # Check the data source configuration
            data_source_name = config.get('dataSourceName')
            if data_source_name:
                print(f"\n=== CHECKING DATA SOURCE: {data_source_name} ===")
                ds_url = f'{endpoint}/datasources/{data_source_name}?api-version=2023-07-01-preview'
                ds_response = requests.get(ds_url, headers=headers)
                
                if ds_response.status_code == 200:
                    ds_config = ds_response.json()
                    print(f"Data Source Type: {ds_config.get('type', 'unknown')}")
                    
                    container = ds_config.get('container', {})
                    if container:
                        print(f"Container Name: {container.get('name', 'unknown')}")
                        print(f"Query: {container.get('query', 'none')}")
                        
                        # Check if the query is selecting too much data
                        query = container.get('query', '')
                        if '*' in query or not query:
                            print("⚠️  Query may be selecting all files, causing massive reprocessing!")
                else:
                    print(f"❌ Could not get data source config: {ds_response.status_code}")
        else:
            print(f"❌ Could not get indexer config: {response.status_code}")
            print(response.text)
            
    except Exception as e:
        print(f"Error investigating indexer: {e}")
    
    print("\n=== URGENT ACTION NEEDED ===")
    print("The indexer cannot be stopped normally. Options:")
    print("1. Delete the indexer entirely (drastic but effective)")
    print("2. Reset the index and start fresh")
    print("3. Contact Azure support for stuck indexer")
    print("4. Try alternative stop methods")

if __name__ == "__main__":
    investigate_indexer_config()
