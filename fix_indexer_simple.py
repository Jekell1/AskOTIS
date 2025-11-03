#!/usr/bin/env python3
"""
Simple fix for COBOL indexer - directly point to COBOL files instead of JSONL
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def fix_indexer_simple():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== SIMPLE COBOL INDEXER FIX ===")
    print("🎯 Root cause: Indexer is parsing JSONL files instead of COBOL files")
    
    # Step 1: Check current data source
    ds_name = "cobol-ds"
    ds_url = f'{endpoint}/datasources/{ds_name}?api-version=2023-07-01-preview'
    
    print(f"\n📋 Checking data source '{ds_name}':")
    
    try:
        ds_response = requests.get(ds_url, headers=headers)
        
        if ds_response.status_code == 200:
            ds_config = ds_response.json()
            container_name = ds_config.get('container', {}).get('name', 'unknown')
            query = ds_config.get('container', {}).get('query', 'none')
            
            print(f"   Container: {container_name}")
            print(f"   Query/Pattern: {query}")
            
            # Check if it's pointing to JSONL files
            if 'jsonl' in str(query).lower():
                print("   ❌ PROBLEM: Data source is filtering for JSONL files!")
                
                # Update data source to target COBOL files
                print(f"\n🔧 Updating data source to target COBOL files...")
                
                updated_ds = ds_config.copy()
                
                # Remove system fields
                for key in ['@odata.context', '@odata.etag']:
                    updated_ds.pop(key, None)
                
                # Update container query to target COBOL files
                if 'container' in updated_ds:
                    updated_ds['container']['query'] = "*.cbl OR *.CBL OR *.cobol OR *.COBOL"
                
                # Update the data source
                ds_update_response = requests.put(ds_url, headers=headers, json=updated_ds)
                
                if ds_update_response.status_code in [200, 201]:
                    print("   ✅ Data source updated to target COBOL files")
                else:
                    print(f"   ❌ Failed to update data source: {ds_update_response.status_code}")
                    print(ds_update_response.text)
                    return
            else:
                print("   ✅ Data source query looks correct")
        
        else:
            print(f"❌ Could not get data source: {ds_response.status_code}")
            return
    
    except Exception as e:
        print(f"❌ Error checking data source: {e}")
        return
    
    # Step 2: Update indexer configuration for COBOL files
    print(f"\n🔧 Updating indexer for COBOL file processing...")
    
    indexer_name = "cobol-indexer"
    indexer_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
    
    try:
        indexer_response = requests.get(indexer_url, headers=headers)
        
        if indexer_response.status_code == 200:
            indexer_config = indexer_response.json()
            
            # Remove system fields
            for key in ['@odata.context', '@odata.etag']:
                indexer_config.pop(key, None)
            
            # Update parameters for COBOL processing
            indexer_config['parameters'] = {
                "batchSize": 10,        # Smaller batches
                "maxFailedItems": 5,
                "maxFailedItemsPerBatch": 2,
                "configuration": {
                    "parsingMode": "text",  # Changed from "json" to "text"
                    "dataToExtract": "contentAndMetadata",
                    "failOnUnsupportedContentType": False,
                    "indexedFileNameExtensions": ".cbl,.CBL,.cobol,.COBOL",  # Changed from ".jsonl"
                    "excludedFileNameExtensions": ".jsonl,.log,.tmp"  # Exclude JSONL files
                }
            }
            
            # Simple field mappings for text files
            indexer_config['fieldMappings'] = [
                {
                    "sourceFieldName": "metadata_storage_path",
                    "targetFieldName": "id",
                    "mappingFunction": {
                        "name": "base64Encode"
                    }
                },
                {
                    "sourceFieldName": "metadata_storage_path",
                    "targetFieldName": "repo_path"
                },
                {
                    "sourceFieldName": "content",
                    "targetFieldName": "code"
                }
            ]
            
            # Remove skillset temporarily to simplify
            indexer_config.pop('skillsetName', None)
            indexer_config.pop('outputFieldMappings', None)
            
            # Update indexer
            indexer_update_response = requests.put(indexer_url, headers=headers, json=indexer_config)
            
            if indexer_update_response.status_code in [200, 201]:
                print("   ✅ Indexer updated for COBOL text processing")
            else:
                print(f"   ❌ Failed to update indexer: {indexer_update_response.status_code}")
                print(indexer_update_response.text)
                return
        
        else:
            print(f"❌ Could not get indexer config: {indexer_response.status_code}")
            return
    
    except Exception as e:
        print(f"❌ Error updating indexer: {e}")
        return
    
    # Step 3: Test the fix
    print(f"\n🧪 Ready to test the fix...")
    
    confirm = input("Do you want to reset and run the indexer now? (yes/no): ")
    if confirm.lower() in ['yes', 'y']:
        
        # Reset indexer
        reset_url = f'{endpoint}/indexers/{indexer_name}/reset?api-version=2023-07-01-preview'
        reset_response = requests.post(reset_url, headers=headers)
        
        if reset_response.status_code == 204:
            print("✅ Indexer reset successful")
            
            # Run the indexer
            run_url = f'{endpoint}/indexers/{indexer_name}/run?api-version=2023-07-01-preview'
            run_response = requests.post(run_url, headers=headers)
            
            if run_response.status_code == 202:
                print("✅ Indexer started!")
                print("\n📋 What this should fix:")
                print("   • Switch from JSONL parsing to direct COBOL file parsing")
                print("   • Index full COBOL file content instead of pre-processed lines")
                print("   • Capture complete file content in the 'code' field")
                print("\n📊 Monitor progress with: python monitor_indexer_progress.py")
                
            else:
                print(f"❌ Failed to run indexer: {run_response.status_code}")
                print(run_response.text)
        else:
            print(f"❌ Failed to reset indexer: {reset_response.status_code}")
            print(reset_response.text)
    else:
        print("⏳ Indexer updated but not run. Use the monitor script when ready.")

if __name__ == "__main__":
    fix_indexer_simple()
