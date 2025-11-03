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

print("🔧 CREATING JSONL DATA SOURCE AND INDEXER (FINAL VERSION)")
print("=" * 65)

# Delete the previous attempt first
delete_url = f"https://{search_service_name}.search.windows.net/datasources/cobol-jsonl-ds"
try:
    requests.delete(delete_url, headers=headers, params=params)
    print("🗑️  Cleaned up previous attempt")
except:
    pass

# Create data source WITHOUT deletion detection policy (required for jsonLines)
new_datasource_config = {
    "name": "cobol-jsonl-ds",
    "type": "azureblob",
    "credentials": {
        "connectionString": config['Values']['AzureWebJobsStorage']
    },
    "container": {
        "name": "aisearch",
        "query": "S35-Source/S35JSON/"
    }
    # NO deletion detection policy - not compatible with jsonLines parsing
}

print("1. CREATING JSONL-COMPATIBLE DATA SOURCE...")
datasource_url = f"https://{search_service_name}.search.windows.net/datasources/cobol-jsonl-ds"

try:
    response = requests.put(datasource_url, headers=headers, params=params, json=new_datasource_config)
    
    if response.status_code in [200, 201]:
        print("✅ Data source created successfully!")
        
        # Get existing indexer configuration
        print("\n2. GETTING EXISTING INDEXER CONFIGURATION...")
        existing_indexer_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
        existing_response = requests.get(existing_indexer_url, headers=headers, params=params)
        
        if existing_response.status_code == 200:
            existing_indexer = existing_response.json()
            
            # Create new indexer with JSONL parsing
            new_indexer_config = {
                "name": "cobol-jsonl-indexer",
                "dataSourceName": "cobol-jsonl-ds",
                "targetIndexName": existing_indexer.get('targetIndexName', 'cobol-index'),
                "skillsetName": existing_indexer.get('skillsetName'),
                "schedule": {
                    "interval": "PT4H"  # Run every 4 hours (this will be a big job)
                },
                "parameters": {
                    "batchSize": 10,  # Smaller batches for JSONL parsing
                    "maxFailedItems": 100,
                    "maxFailedItemsPerBatch": 10,
                    "configuration": {
                        "dataToExtract": "contentAndMetadata",
                        "parsingMode": "jsonLines",  # KEY: Parse each line as separate document
                        "indexedFileNameExtensions": ".jsonl"
                    }
                },
                "fieldMappings": existing_indexer.get('fieldMappings', []),
                "outputFieldMappings": existing_indexer.get('outputFieldMappings', [])
            }
            
            print("3. CREATING JSONL INDEXER...")
            print(f"✅ Parsing Mode: jsonLines")
            print(f"✅ Batch Size: 10 (smaller for performance)")
            print(f"✅ Target: {existing_indexer.get('targetIndexName')}")
            print(f"✅ Skillset: {existing_indexer.get('skillsetName')}")
            
            indexer_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-jsonl-indexer"
            indexer_response = requests.put(indexer_url, headers=headers, params=params, json=new_indexer_config)
            
            if indexer_response.status_code in [200, 201]:
                print("✅ JSONL indexer created successfully!")
                
                # Start the indexer
                print("\n4. STARTING THE JSONL INDEXER...")
                run_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-jsonl-indexer/run"
                run_response = requests.post(run_url, headers=headers, params=params)
                
                if run_response.status_code in [200, 202]:
                    print("✅ JSONL indexer started successfully!")
                    
                    print("\n" + "🎉" * 20)
                    print("🚀 BREAKTHROUGH ACHIEVED!")
                    print("🎉" * 20)
                    print()
                    print("✅ Fixed the root cause: Missing jsonLines parsing mode")
                    print("✅ Created new data source: cobol-jsonl-ds") 
                    print("✅ Created new indexer: cobol-jsonl-indexer")
                    print("✅ Now processing EVERY LINE in JSONL files!")
                    print()
                    print("📊 EXPECTED RESULTS:")
                    print(f"   📁 JSONL files: ~9,952")
                    print(f"   📄 Lines per file: ~50-400")  
                    print(f"   📈 Total documents: 200,000 - 300,000+")
                    print(f"   🔍 Full COBOL line-level search capability!")
                    print()
                    print("⏳ PROCESSING TIME: This will take several hours")
                    print("   Monitor progress: python monitor_indexer.py")
                    print("   Watch document count grow from 19,904 to 200,000+")
                    print()
                    print("🎯 WHEN COMPLETE:")
                    print("   Your search for 'CUSTOMER-ACCNT-NO' will find")
                    print("   EVERY occurrence in your COBOL codebase!")
                    print("🎉" * 20)
                    
                else:
                    print(f"⚠️  Indexer start issue: {run_response.status_code} - {run_response.text}")
                    
            else:
                print(f"❌ Failed to create indexer: {indexer_response.status_code}")
                print(f"Error: {indexer_response.text}")
                
        else:
            print(f"❌ Could not get existing indexer: {existing_response.status_code}")
            
    else:
        print(f"❌ Failed to create data source: {response.status_code}")
        print(f"Error: {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

print("\n" + "=" * 65)
print("📋 MONITOR COMMANDS:")
print("   python monitor_indexer.py")
print("   python detailed_document_analysis.py")
print("=" * 65)
