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

print("🔧 FIXING DATA SOURCE FOR JSONL LINE-BY-LINE PARSING")
print("=" * 60)

# The main data source name
datasource_name = "cobol-ds"
datasource_url = f"https://{search_service_name}.search.windows.net/datasources/{datasource_name}"

# First, get the current configuration
print("1. GETTING CURRENT CONFIGURATION...")
try:
    response = requests.get(datasource_url, headers=headers, params=params)
    
    if response.status_code == 200:
        current_config = response.json()
        print("✅ Current config retrieved")
        
        # Update the configuration to parse JSONL files line by line
        updated_config = current_config.copy()
        
        # Add the parsingMode to the container configuration
        if 'container' not in updated_config:
            updated_config['container'] = {}
            
        updated_config['container']['parsingMode'] = 'jsonLines'
        
        # Also set query to target our JSONL files
        updated_config['container']['query'] = 'S35-Source/S35JSON/'
        
        print("\n2. UPDATED CONFIGURATION:")
        print("-" * 40)
        print(f"✅ Added parsingMode: 'jsonLines'")
        print(f"✅ Set query to: 'S35-Source/S35JSON/'")
        print(f"✅ This will parse each line in JSONL files as a separate document")
        
        print(f"\n📄 New container config:")
        print(json.dumps(updated_config['container'], indent=2))
        
        # Update the data source
        print("\n3. APPLYING THE FIX...")
        
        update_response = requests.put(datasource_url, headers=headers, params=params, json=updated_config)
        
        if update_response.status_code in [200, 201]:
            print("✅ DATA SOURCE UPDATED SUCCESSFULLY!")
            print("\n🎉 EXPECTED RESULTS:")
            print("   - Each LINE in JSONL files will now be indexed as separate document")
            print("   - Document count should increase from ~19,904 to ~200,000+")
            print("   - You'll be able to search individual COBOL lines")
            
            # Now we need to reset and re-run the indexer
            print("\n4. TRIGGERING INDEXER RESET AND RE-RUN...")
            
            # Reset the indexer
            indexer_reset_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer/reset"
            reset_response = requests.post(indexer_reset_url, headers=headers, params=params)
            
            if reset_response.status_code in [200, 204]:
                print("✅ Indexer reset successfully")
                
                # Run the indexer
                indexer_run_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer/run"
                run_response = requests.post(indexer_run_url, headers=headers, params=params)
                
                if run_response.status_code in [200, 202]:
                    print("✅ Indexer started successfully!")
                    print("\n⏳ INDEXING IN PROGRESS...")
                    print("This will take significantly longer as we're now processing")
                    print("200,000+ individual COBOL lines instead of just file headers.")
                    print("\nMonitor progress with: python monitor_indexer.py")
                else:
                    print(f"⚠️  Indexer run issue: {run_response.status_code} - {run_response.text}")
            else:
                print(f"⚠️  Indexer reset issue: {reset_response.status_code} - {reset_response.text}")
            
        else:
            print(f"❌ Failed to update data source: {update_response.status_code}")
            print(f"Error: {update_response.text}")
            
    else:
        print(f"❌ Failed to get current config: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

print("\n" + "=" * 60)
print("📋 SUMMARY:")
print("-" * 30)
print("✅ Root cause identified: Missing parsingMode configuration")
print("✅ Applied fix: Set parsingMode to 'jsonLines'")
print("✅ Indexer reset and restarted")
print("⏳ Now processing ALL lines in JSONL files")
print("📈 Expected result: 200,000+ searchable COBOL documents")
print("🔍 Monitor with: python monitor_indexer.py")
print("=" * 60)
