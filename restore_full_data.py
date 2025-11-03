#!/usr/bin/env python3
"""
RESTORE FULL DATA TO RECREATED INDEXES
Gets all the original data back into the recreated indexes.
"""

import requests
import os
import json
import time
from typing import Dict, List
from secrets_loader import load_secrets

def load_config():
    """Load configuration."""
    load_secrets()
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    }

def get_all_records_from_backup_source(config: Dict, index_name: str) -> List[Dict]:
    """Get all records from the original data source (since we lost the full backup)."""
    print(f"🔍 FINDING ORIGINAL DATA SOURCE FOR {index_name}")
    print("-" * 60)
    
    # Check if there's a datasource for this index
    datasources_url = f"{config['search_endpoint']}/datasources?api-version=2023-11-01"
    
    try:
        response = requests.get(
            datasources_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"❌ Error fetching datasources: {response.status_code}")
            return []
            
        datasources = response.json().get('value', [])
        
        # Look for matching datasource
        matching_datasource = None
        for ds in datasources:
            if index_name.replace('new_cobol_', '') in ds['name'].lower():
                matching_datasource = ds
                break
        
        if not matching_datasource:
            print(f"❌ No matching datasource found for {index_name}")
            print(f"Available datasources: {[ds['name'] for ds in datasources]}")
            return []
        
        print(f"✅ Found datasource: {matching_datasource['name']}")
        return get_records_from_datasource(config, matching_datasource)
        
    except Exception as e:
        print(f"❌ Exception finding datasource: {e}")
        return []

def get_records_from_datasource(config: Dict, datasource: Dict) -> List[Dict]:
    """Get records from the original datasource."""
    print(f"📥 GETTING RECORDS FROM DATASOURCE: {datasource['name']}")
    print("-" * 60)
    
    # For JSONL datasources, we need to read the blob directly
    if datasource.get('type') == 'azureblob':
        container_name = datasource['container']['name']
        connection_string = datasource['credentials']['connectionString']
        
        print(f"Container: {container_name}")
        print(f"⚠️  Note: Direct blob access would require Azure Storage SDK")
        print(f"📋 Alternative: Let's check if there are indexers that can help")
        
        return check_indexer_for_data(config, datasource['name'])
    
    return []

def check_indexer_for_data(config: Dict, datasource_name: str) -> List[Dict]:
    """Check if there's an indexer that processes this datasource."""
    print(f"🔍 CHECKING INDEXERS FOR {datasource_name}")
    print("-" * 50)
    
    indexers_url = f"{config['search_endpoint']}/indexers?api-version=2023-11-01"
    
    try:
        response = requests.get(
            indexers_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"❌ Error fetching indexers: {response.status_code}")
            return []
            
        indexers = response.json().get('value', [])
        
        for indexer in indexers:
            if indexer.get('dataSourceName') == datasource_name:
                print(f"✅ Found indexer: {indexer['name']}")
                print(f"   Target index: {indexer.get('targetIndexName')}")
                print(f"   Status: {indexer.get('status', {}).get('lastResult', {}).get('status', 'unknown')}")
                
                # Trigger indexer to repopulate
                return trigger_indexer_run(config, indexer['name'])
        
        print(f"❌ No indexer found for datasource {datasource_name}")
        return []
        
    except Exception as e:
        print(f"❌ Exception checking indexers: {e}")
        return []

def trigger_indexer_run(config: Dict, indexer_name: str):
    """Trigger an indexer run to repopulate the index."""
    print(f"🚀 TRIGGERING INDEXER RUN: {indexer_name}")
    print("-" * 50)
    
    run_url = f"{config['search_endpoint']}/indexers/{indexer_name}/run?api-version=2023-11-01"
    
    try:
        response = requests.post(
            run_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code in [200, 202]:
            print(f"✅ Indexer {indexer_name} triggered successfully")
            print(f"⏳ Waiting for indexer to complete...")
            
            # Monitor indexer status
            return monitor_indexer_progress(config, indexer_name)
        else:
            print(f"❌ Error triggering indexer: {response.status_code} - {response.text}")
            return []
            
    except Exception as e:
        print(f"❌ Exception triggering indexer: {e}")
        return []

def monitor_indexer_progress(config: Dict, indexer_name: str, max_wait_minutes: int = 15):
    """Monitor indexer progress."""
    print(f"⏳ MONITORING {indexer_name} PROGRESS")
    print("-" * 50)
    
    status_url = f"{config['search_endpoint']}/indexers/{indexer_name}/status?api-version=2023-11-01"
    
    start_time = time.time()
    max_wait_seconds = max_wait_minutes * 60
    
    while time.time() - start_time < max_wait_seconds:
        try:
            response = requests.get(
                status_url,
                headers={'api-key': config['search_key']},
                timeout=30
            )
            
            if response.status_code != 200:
                print(f"❌ Error checking status: {response.status_code}")
                break
                
            status = response.json()
            last_result = status.get('lastResult', {})
            current_status = last_result.get('status', 'unknown')
            
            if current_status == 'success':
                items_processed = last_result.get('itemsProcessed', 0)
                print(f"✅ Indexer completed successfully")
                print(f"   Items processed: {items_processed}")
                return True
            elif current_status == 'inProgress':
                items_processed = last_result.get('itemsProcessed', 0)
                print(f"   ⏳ In progress... {items_processed} items processed")
                time.sleep(30)  # Wait 30 seconds
            elif current_status in ['transientFailure', 'persistentFailure']:
                print(f"❌ Indexer failed: {current_status}")
                error_details = last_result.get('errors', [])
                if error_details:
                    print(f"   Errors: {json.dumps(error_details[:3], indent=2)}")
                break
            else:
                print(f"   Status: {current_status}")
                time.sleep(10)
                
        except Exception as e:
            print(f"❌ Exception monitoring: {e}")
            break
    
    print(f"⏰ Monitoring timeout after {max_wait_minutes} minutes")
    return False

def restore_full_data():
    """Restore full data to the recreated indexes."""
    config = load_config()
    
    print("📥 RESTORING FULL DATA TO RECREATED INDEXES")
    print("=" * 70)
    
    indexes_to_restore = [
        'new_cobol_screen_nodes',
        'new_cobol_ui_paths'
    ]
    
    for index_name in indexes_to_restore:
        print(f"\n🔄 RESTORING {index_name}")
        print("=" * 50)
        
        # Try to trigger the indexer to repopulate
        records = get_all_records_from_backup_source(config, index_name)
        
        # Check final document count
        count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        try:
            time.sleep(5)  # Wait for indexing to settle
            count_response = requests.get(
                count_url,
                headers={'api-key': config['search_key']},
                timeout=30
            )
            if count_response.status_code == 200:
                doc_count = int(count_response.text)
                print(f"✅ Final document count for {index_name}: {doc_count:,}")
                
                if doc_count > 5000:  # Expected reasonable count
                    print(f"   ✅ Restoration appears successful")
                else:
                    print(f"   ⚠️  Document count seems low, may need manual intervention")
            else:
                print(f"❌ Cannot verify document count: {count_response.status_code}")
        except Exception as e:
            print(f"❌ Error checking final count: {e}")
    
    print(f"\n" + "=" * 70)
    print("🎯 DATA RESTORATION COMPLETE")
    print("=" * 70)
    print("Check document counts above to verify successful restoration")

if __name__ == '__main__':
    restore_full_data()