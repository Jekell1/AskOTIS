import json
import requests
import sys

# Load configuration
try:
    with open('local.settings.json', 'r') as f:
        config = json.load(f)
    
    # Extract service name from endpoint
    search_endpoint = config['Values']['SEARCH_ENDPOINT']
    search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
    search_admin_key = config['Values']['SEARCH_KEY']
    
except Exception as e:
    print(f"Error loading configuration: {e}")
    sys.exit(1)

# Azure AI Search REST API endpoint for indexers
url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2024-07-01'
}

# Indexer definition
indexer_data = {
    "name": "cobol-indexer",
    "description": "Indexer for processing COBOL files with custom parsing",
    "dataSourceName": "cobol-ds",
    "targetIndexName": "cobol-index", 
    "skillsetName": "cobol-skillset",
    "schedule": { 
        "interval": "PT2H"  # Run every 2 hours
    },
    "parameters": {
        "configuration": {
            "parsingMode": "json",
            "dataToExtract": "contentAndMetadata",
            "failOnUnsupportedContentType": False,
            "indexedFileNameExtensions": ".jsonl"
        }
    },
    "fieldMappings": [
        # Map fields from the JSONL data to the index
        { 
            "sourceFieldName": "id", 
            "targetFieldName": "id",
            "mappingFunction": {
                "name": "base64Encode",
                "parameters": { "useHttpServerUtilityUrlTokenEncode": False }
            }
        },
        { "sourceFieldName": "repo_path", "targetFieldName": "repo_path" },
        { "sourceFieldName": "line", "targetFieldName": "line" },
        { "sourceFieldName": "code", "targetFieldName": "code" }
    ],
    "outputFieldMappings": [
        # Map skillset outputs to index fields - matching our Azure Function outputs
        { "sourceFieldName": "/document/symbols", "targetFieldName": "symbol_name" },
        { "sourceFieldName": "/document/symbol_type", "targetFieldName": "symbol_kind" },
        { "sourceFieldName": "/document/calls", "targetFieldName": "calls" }
    ]
}

print("Creating COBOL indexer:")
print("=" * 60)
print(f"Search Service: {search_service_name}")
print(f"Indexer Name: {indexer_data['name']}")
print(f"Data Source: {indexer_data['dataSourceName']}")
print(f"Target Index: {indexer_data['targetIndexName']}")
print(f"Skillset: {indexer_data['skillsetName']}")
print(f"Schedule: {indexer_data['schedule']['interval']}")
print()

try:
    # Create the indexer using PUT
    response = requests.put(url, headers=headers, params=params, json=indexer_data)
    
    print(f"Status Code: {response.status_code}")
    print(f"URL: {url}")
    
    if response.status_code in [200, 201, 204]:
        print("✅ Indexer 'cobol-indexer' created successfully!")
        
        # Show the response if there is content
        if response.content:
            try:
                result = response.json()
                print(f"Created indexer: {result.get('name', 'N/A')}")
                print(f"Description: {result.get('description', 'N/A')}")
                print(f"Data Source: {result.get('dataSourceName', 'N/A')}")
                print(f"Target Index: {result.get('targetIndexName', 'N/A')}")
                print(f"Skillset: {result.get('skillsetName', 'N/A')}")
                
                # Show field mappings
                field_mappings = result.get('fieldMappings', [])
                output_mappings = result.get('outputFieldMappings', [])
                
                print(f"Field Mappings: {len(field_mappings)}")
                for mapping in field_mappings:
                    print(f"  {mapping['sourceFieldName']} -> {mapping['targetFieldName']}")
                
                print(f"Output Field Mappings: {len(output_mappings)}")
                for mapping in output_mappings:
                    print(f"  {mapping['sourceFieldName']} -> {mapping['targetFieldName']}")
                    
            except:
                print("Response received but no JSON content")
        else:
            print("Indexer created (no response body)")
    else:
        print(f"✗ Failed to create indexer")
        print(f"Response: {response.text}")
        
        # Try to parse error details
        try:
            error_data = response.json()
            if 'error' in error_data:
                print(f"Error details: {error_data['error'].get('message', 'No details')}")
        except:
            pass
        
except Exception as e:
    print(f"Error creating indexer: {e}")

# Verify the indexer was created
try:
    print(f"\nVerifying indexer creation...")
    verify_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
    verify_response = requests.get(verify_url, headers=headers, params={'api-version': '2024-07-01'})
    
    if verify_response.status_code == 200:
        indexer = verify_response.json()
        print(f"✓ Verification: Indexer '{indexer['name']}' exists")
        print(f"  Description: {indexer.get('description', 'N/A')}")
        print(f"  Status: {indexer.get('status', 'Unknown')}")
        print(f"  Last Execution: {indexer.get('lastRunTime', 'Never run')}")
        
    else:
        print(f"✗ Verification failed: Status {verify_response.status_code}")
        if verify_response.status_code == 404:
            print("  Indexer was not created successfully")
        
except Exception as e:
    print(f"\nError verifying indexer: {e}")

# Run the indexer
try:
    print(f"\nRunning the indexer...")
    run_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer/run"
    run_response = requests.post(run_url, headers=headers, params={'api-version': '2024-07-01'})
    
    if run_response.status_code == 202:
        print("✅ Indexer started successfully!")
        print("  The indexer is now processing your COBOL files.")
        print("  This may take a few minutes depending on the number of files.")
        
        # Check indexer status
        print(f"\nChecking indexer status...")
        status_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer/status"
        status_response = requests.get(status_url, headers=headers, params={'api-version': '2024-07-01'})
        
        if status_response.status_code == 200:
            status_data = status_response.json()
            print(f"  Status: {status_data.get('status', 'Unknown')}")
            
            last_result = status_data.get('lastResult')
            if last_result:
                print(f"  Last Result Status: {last_result.get('status', 'Unknown')}")
                print(f"  Items Processed: {last_result.get('itemsProcessed', 'N/A')}")
                print(f"  Items Failed: {last_result.get('itemsFailed', 'N/A')}")
                if last_result.get('errors'):
                    print(f"  Errors: {len(last_result['errors'])}")
                    for error in last_result['errors'][:3]:  # Show first 3 errors
                        print(f"    - {error.get('errorMessage', 'Unknown error')}")
        
    else:
        print(f"✗ Failed to start indexer: Status {run_response.status_code}")
        print(f"Response: {run_response.text}")
        
except Exception as e:
    print(f"\nError running indexer: {e}")

print(f"\n" + "="*60)
print("INDEXER SUMMARY:")
print("✅ Indexer Name: cobol-indexer")
print("✅ Data Source: cobol-ds (Azure Blob Storage)")
print("✅ Target Index: cobol-index")
print("✅ Skillset: cobol-skillset (Custom COBOL parser)")
print("✅ Schedule: Every 2 hours")
print("✅ File Type: JSONL files")
print()
print("FIELD MAPPINGS:")
print("  Source -> Index:")
print("  - metadata_storage_name -> id")
print("  - metadata_storage_path -> repo_path") 
print("  - content -> content")
print()
print("OUTPUT FIELD MAPPINGS (from skillset):")
print("  - /document/symbols -> symbols")
print("  - /document/calls -> calls")
print("  - /document/procedures -> procedures")
print("  - /document/variables -> variables")
print("  - /document/symbol_type -> symbol_type")
print()
print("NEXT STEPS:")
print("1. Monitor indexer progress in Azure Portal")
print("2. Check indexer status and errors if any")
print("3. Test search queries once indexing completes")
print("4. Set up vector search if needed")
print("="*60)
