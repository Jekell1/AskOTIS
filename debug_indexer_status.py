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

print("🔍 DEBUGGING: Why Document Counts Are Not Increasing")
print("=" * 60)

# Check both indexers
indexers = ["cobol-indexer", "cobol-jsonl-indexer"]

for indexer_name in indexers:
    print(f"\n📊 CHECKING INDEXER: {indexer_name}")
    print("-" * 40)
    
    indexer_url = f"https://{search_service_name}.search.windows.net/indexers/{indexer_name}"
    
    try:
        response = requests.get(indexer_url, headers=headers, params=params)
        
        if response.status_code == 200:
            indexer = response.json()
            
            status = indexer.get('status', 'unknown')
            last_result = indexer.get('lastResult', {})
            
            print(f"✅ Status: {status}")
            print(f"📊 Last Result Status: {last_result.get('status', 'N/A')}")
            print(f"📈 Items Processed: {last_result.get('itemsProcessed', 0)}")
            print(f"❌ Items Failed: {last_result.get('itemsFailed', 0)}")
            print(f"⚠️  Warnings: {len(last_result.get('warnings', []))}")
            print(f"❌ Errors: {len(last_result.get('errors', []))}")
            
            start_time = last_result.get('startTime')
            end_time = last_result.get('endTime')
            
            if start_time:
                print(f"🕐 Start Time: {start_time}")
            if end_time:
                print(f"🕐 End Time: {end_time}")
            
            # Show errors if any
            errors = last_result.get('errors', [])
            if errors:
                print(f"\n❌ ERRORS ({len(errors)}):")
                for i, error in enumerate(errors[:3], 1):  # Show first 3 errors
                    print(f"   {i}. {error.get('errorMessage', 'No message')}")
                    print(f"      Key: {error.get('key', 'N/A')}")
            
            # Show warnings if any
            warnings = last_result.get('warnings', [])
            if warnings:
                print(f"\n⚠️  WARNINGS ({len(warnings)}):")
                for i, warning in enumerate(warnings[:3], 1):  # Show first 3 warnings
                    print(f"   {i}. {warning.get('message', 'No message')}")
            
        else:
            print(f"❌ Error getting indexer: {response.status_code} - {response.text}")
            
    except Exception as e:
        print(f"❌ Exception: {e}")

# Check current document count
print(f"\n📄 CURRENT DOCUMENT COUNT:")
print("-" * 30)

search_url = f"https://{search_service_name}.search.windows.net/indexes/cobol-index/docs/search"

count_search = {
    "search": "*",
    "count": True,
    "top": 0
}

try:
    response = requests.post(search_url, headers=headers, params=params, json=count_search)
    
    if response.status_code == 200:
        result = response.json()
        doc_count = result.get('@odata.count', 0)
        print(f"📊 Total documents in index: {doc_count:,}")
        
        if doc_count == 19904:
            print("⚠️  Document count has not changed!")
            print("   This suggests the new indexer isn't working properly.")
        elif doc_count > 19904:
            print(f"✅ Document count increased by {doc_count - 19904:,}")
        
    else:
        print(f"❌ Error getting document count: {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

# Check if the new data source is accessible
print(f"\n🔍 CHECKING DATA SOURCE: cobol-jsonl-ds")
print("-" * 40)

datasource_url = f"https://{search_service_name}.search.windows.net/datasources/cobol-jsonl-ds"

try:
    response = requests.get(datasource_url, headers=headers, params=params)
    
    if response.status_code == 200:
        datasource = response.json()
        print(f"✅ Data source exists")
        print(f"📁 Container: {datasource.get('container', {}).get('name')}")
        print(f"🔍 Query: {datasource.get('container', {}).get('query')}")
        
        # Test if we can access the blob container
        container_info = datasource.get('container', {})
        container_name = container_info.get('name')
        query = container_info.get('query')
        
        print(f"📂 Looking for JSONL files in: {container_name}/{query}")
        
    else:
        print(f"❌ Data source not found: {response.status_code} - {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")

# Check indexer history for more details
print(f"\n📜 INDEXER EXECUTION HISTORY:")
print("-" * 40)

for indexer_name in ["cobol-jsonl-indexer"]:  # Focus on new indexer
    history_url = f"https://{search_service_name}.search.windows.net/indexers/{indexer_name}/status"
    
    try:
        response = requests.get(history_url, headers=headers, params=params)
        
        if response.status_code == 200:
            status_info = response.json()
            execution_history = status_info.get('executionHistory', [])
            
            print(f"\n🔍 {indexer_name} - Last 3 executions:")
            
            for i, execution in enumerate(execution_history[:3], 1):
                status = execution.get('status', 'unknown')
                start_time = execution.get('startTime', 'N/A')
                end_time = execution.get('endTime', 'N/A')
                items_processed = execution.get('itemsProcessed', 0)
                items_failed = execution.get('itemsFailed', 0)
                
                print(f"   {i}. Status: {status}")
                print(f"      Start: {start_time}")
                print(f"      End: {end_time}")
                print(f"      Processed: {items_processed}")
                print(f"      Failed: {items_failed}")
                
                if execution.get('errorMessage'):
                    print(f"      Error: {execution.get('errorMessage')}")
        
        else:
            print(f"❌ Could not get indexer status: {response.status_code}")
            
    except Exception as e:
        print(f"❌ Error: {e}")

print(f"\n" + "=" * 60)
print("🧐 POSSIBLE ISSUES:")
print("1. New indexer may have errors or configuration issues")
print("2. Data source path might be incorrect")
print("3. Indexer might not be running automatically")
print("4. Permissions or connection issues")
print("5. JSONL parsing configuration might be invalid")
print("=" * 60)
