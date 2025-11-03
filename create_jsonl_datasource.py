import json
import requests

# Load configuration
with open('local.settings.json', 'r') as f:
    config = json.load(f)

search_endpoint = config['Values']['SEARCH_ENDPOINT']
search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
search_admin_key = config['Values']['SEARCH_KEY']

headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

print("🔧 CREATING NEW DATA SOURCE WITH CORRECT JSONL PARSING")
print("=" * 60)

# Let's create a new data source specifically for JSONL line parsing
new_datasource_name = "cobol-jsonl-ds"
datasource_url = f"https://{search_service_name}.search.windows.net/datasources/{new_datasource_name}"

# Configuration for parsing JSONL files line by line
new_datasource_config = {
    "name": new_datasource_name,
    "type": "azureblob",
    "credentials": {
        "connectionString": config['Values']['AzureWebJobsStorage']
    },
    "container": {
        "name": "aisearch",
        "query": "S35-Source/S35JSON/"  # Only target JSONL files
    },
    "dataChangeDetectionPolicy": {
        "@odata.type": "#Microsoft.Azure.Search.HighWaterMarkChangeDetectionPolicy",
        "highWaterMarkColumnName": "metadata_storage_last_modified"
    },
    "dataDeletionDetectionPolicy": {
        "@odata.type": "#Microsoft.Azure.Search.NativeBlobSoftDeleteDeletionDetectionPolicy"
    }
}

print("1. CREATING NEW DATA SOURCE FOR JSONL PARSING...")
print(f"📄 Configuration:")
print(json.dumps(new_datasource_config, indent=2))

try:
    response = requests.put(datasource_url, headers=headers, params=params, json=new_datasource_config)
    
    if response.status_code in [200, 201]:
        print("✅ New data source created successfully!")
        
        # Now we need to create a new indexer that uses this data source
        # and configures the parsing for JSONL
        
        print("\n2. CREATING NEW INDEXER WITH JSONL PARSING...")
        
        new_indexer_name = "cobol-jsonl-indexer"
        indexer_url = f"https://{search_service_name}.search.windows.net/indexers/{new_indexer_name}"
        
        # First, let's get the existing skillset and index names
        existing_indexer_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
        existing_response = requests.get(existing_indexer_url, headers=headers, params=params)
        
        if existing_response.status_code == 200:
            existing_indexer = existing_response.json()
            skillset_name = existing_indexer.get('skillsetName')
            target_index_name = existing_indexer.get('targetIndexName')
            
            # Create new indexer with JSONL parsing configuration
            new_indexer_config = {
                "name": new_indexer_name,
                "dataSourceName": new_datasource_name,
                "targetIndexName": target_index_name,
                "skillsetName": skillset_name,
                "schedule": {
                    "interval": "PT2H"  # Run every 2 hours
                },
                "parameters": {
                    "batchSize": 50,
                    "maxFailedItems": 10,
                    "maxFailedItemsPerBatch": 5,
                    "configuration": {
                        "dataToExtract": "contentAndMetadata",
                        "parsingMode": "jsonLines",  # This is the key setting!
                        "indexedFileNameExtensions": ".jsonl"
                    }
                },
                "fieldMappings": existing_indexer.get('fieldMappings', []),
                "outputFieldMappings": existing_indexer.get('outputFieldMappings', [])
            }
            
            print(f"📄 New indexer configuration:")
            print(f"✅ parsingMode: 'jsonLines'")
            print(f"✅ Data source: {new_datasource_name}")
            print(f"✅ Target index: {target_index_name}")
            print(f"✅ Skillset: {skillset_name}")
            
            indexer_response = requests.put(indexer_url, headers=headers, params=params, json=new_indexer_config)
            
            if indexer_response.status_code in [200, 201]:
                print("✅ New JSONL indexer created successfully!")
                
                # Run the new indexer
                run_url = f"https://{search_service_name}.search.windows.net/indexers/{new_indexer_name}/run"
                run_response = requests.post(run_url, headers=headers, params=params)
                
                if run_response.status_code in [200, 202]:
                    print("✅ JSONL indexer started successfully!")
                    print("\n🎉 MAJOR BREAKTHROUGH!")
                    print("=" * 50)
                    print("✅ Created new data source with proper JSONL parsing")
                    print("✅ Created new indexer with parsingMode: 'jsonLines'")
                    print("✅ Now processing ALL lines in JSONL files!")
                    print("📈 Expected: 200,000+ documents from ~300,000 COBOL lines")
                    print("\n⏳ PROCESSING IN PROGRESS...")
                    print("Monitor with: python monitor_indexer.py")
                    print("Or check new indexer: cobol-jsonl-indexer")
                    print("=" * 50)
                else:
                    print(f"⚠️  Indexer run issue: {run_response.status_code} - {run_response.text}")
                    
            else:
                print(f"❌ Failed to create indexer: {indexer_response.status_code}")
                print(f"Error: {indexer_response.text}")
                
        else:
            print(f"❌ Could not get existing indexer config: {existing_response.status_code}")
            
    else:
        print(f"❌ Failed to create data source: {response.status_code}")
        print(f"Error: {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

print("\n" + "=" * 60)
print("🔍 NEXT STEPS:")
print("-" * 30)
print("1. Monitor the new indexer: cobol-jsonl-indexer")
print("2. Check document count growth with monitor_indexer.py")
print("3. Once complete, you'll have 200,000+ searchable COBOL lines!")
print("4. Can then disable/delete the old indexer if desired")
print("=" * 60)
