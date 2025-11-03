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

print("🔍 FINDING THE CORRECT DATA SOURCE")
print("=" * 50)

# List all data sources
datasources_url = f"https://{search_service_name}.search.windows.net/datasources"

try:
    response = requests.get(datasources_url, headers=headers, params=params)
    
    if response.status_code == 200:
        datasources_list = response.json()
        datasources = datasources_list.get('value', [])
        
        print(f"📋 FOUND {len(datasources)} DATA SOURCES:")
        print("-" * 50)
        
        for i, ds in enumerate(datasources, 1):
            name = ds.get('name')
            ds_type = ds.get('type')
            print(f"{i}. {name} (type: {ds_type})")
        
        # Now examine each data source
        for ds in datasources:
            name = ds.get('name')
            print(f"\n🔍 EXAMINING: {name}")
            print("-" * 30)
            
            container = ds.get('container', {})
            container_name = container.get('name')
            query = container.get('query')
            
            print(f"Container: {container_name}")
            print(f"Query: {query}")
            
            # Check for parsing mode - THIS IS KEY!
            parsing_mode = container.get('parsingMode')
            print(f"⚠️  Parsing Mode: {parsing_mode}")
            
            if 'cobol' in name.lower() or 'aisearch' in container_name:
                print(f"🎯 THIS LOOKS LIKE OUR COBOL DATA SOURCE!")
                print(f"📄 FULL CONFIG:")
                print(json.dumps(ds, indent=2))
        
    else:
        print(f"❌ Error listing data sources: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

print("\n" + "=" * 50)
print("🚨 CRITICAL FINDING:")
print("If parsingMode is NULL or not 'jsonLines',")
print("that's why we're only getting 1 document per JSONL file!")
print("=" * 50)
