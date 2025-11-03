import json
import requests
import time
import sys
from datetime import datetime

# Load configuration
try:
    with open('local.settings.json', 'r') as f:
        config = json.load(f)
    
    search_endpoint = config['Values']['SEARCH_ENDPOINT']
    search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
    search_admin_key = config['Values']['SEARCH_KEY']
    
    # Azure Storage account info from connection string
    connection_string = config['Values']['AzureWebJobsStorage']
    # Extract account name and key from connection string
    storage_account_name = 'waazuse1aistorage'
    storage_account_key = 'CC+qIlIhGidHQycBZ/MZ19KN2kmKSiMkLgU8kXr16//yE3aGbWK85kEfTpE7K4In7vKi0xn5MzZI+AStxHZ/sw=='
    container_name = 'aisearch'
    
except Exception as e:
    print(f"Error loading configuration: {e}")
    sys.exit(1)

# Azure AI Search REST API endpoints
indexer_url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
search_url = f"https://{search_service_name}.search.windows.net/indexes/cobol-index/docs/search"

headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}

params = {
    'api-version': '2024-07-01'
}

def get_blob_count():
    """Get the total number of JSONL files in blob storage"""
    try:
        from azure.storage.blob import BlobServiceClient
        
        blob_service_client = BlobServiceClient(
            account_url=f"https://{storage_account_name}.blob.core.windows.net",
            credential=storage_account_key
        )
        
        container_client = blob_service_client.get_container_client(container_name)
        
        # Count JSONL files in the S35-Source/S35JSON folder
        jsonl_count = 0
        for blob in container_client.list_blobs(name_starts_with="S35-Source/S35JSON/"):
            if blob.name.endswith('.jsonl'):
                jsonl_count += 1
                
        return jsonl_count
        
    except Exception as e:
        print(f"Error counting blobs: {e}")
        return None

def get_indexer_status():
    """Get current indexer status"""
    try:
        response = requests.get(indexer_url, headers=headers, params=params)
        
        if response.status_code == 200:
            return response.json()
        else:
            print(f"Error getting indexer status: {response.status_code} - {response.text}")
            return None
            
    except Exception as e:
        print(f"Error: {e}")
        return None

def get_document_count():
    """Get current count of documents in the search index"""
    try:
        search_query = {
            "search": "*",
            "count": True,
            "top": 0  # Don't return documents, just count
        }
        
        response = requests.post(search_url, headers=headers, params=params, json=search_query)
        
        if response.status_code == 200:
            result = response.json()
            return result.get('@odata.count', 0)
        else:
            print(f"Error getting document count: {response.status_code}")
            return None
            
    except Exception as e:
        print(f"Error: {e}")
        return None

def format_timestamp(timestamp_str):
    """Format timestamp for display"""
    if not timestamp_str:
        return "N/A"
    try:
        dt = datetime.fromisoformat(timestamp_str.replace('Z', '+00:00'))
        return dt.strftime('%Y-%m-%d %H:%M:%S UTC')
    except:
        return timestamp_str

print("🔍 COBOL INDEXING PROGRESS MONITOR")
print("=" * 60)

# First, let's see how many files we expect to index
print("📊 GETTING BASELINE COUNTS...")
expected_files = get_blob_count()
if expected_files:
    print(f"📁 JSONL files in blob storage: {expected_files}")
else:
    print("⚠️  Could not count blob files (using Azure Storage SDK)")

print("-" * 60)

# Monitor indexing progress
check_count = 0
max_checks = 20  # Run for up to 20 checks
check_interval = 30  # Check every 30 seconds

while check_count < max_checks:
    check_count += 1
    print(f"\n🔍 CHECK #{check_count} - {datetime.now().strftime('%H:%M:%S')}")
    
    # Get indexer status
    indexer_status = get_indexer_status()
    current_doc_count = get_document_count()
    
    if not indexer_status or current_doc_count is None:
        print("❌ Error getting status")
        continue
    
    status = indexer_status.get('status', 'unknown')
    last_result = indexer_status.get('lastResult', {})
    
    execution_history = indexer_status.get('executionHistory', [])
    latest_execution = execution_history[0] if execution_history else {}
    
    items_processed = latest_execution.get('itemsProcessed', 0)
    items_failed = latest_execution.get('itemsFailed', 0)
    start_time = latest_execution.get('startTime')
    end_time = latest_execution.get('endTime')
    
    print(f"📊 Status: {status}")
    print(f"📈 Documents indexed: {current_doc_count}")
    
    if expected_files:
        percentage = (current_doc_count / expected_files) * 100 if expected_files > 0 else 0
        print(f"📊 Progress: {current_doc_count}/{expected_files} ({percentage:.1f}%)")
        
        if current_doc_count >= expected_files:
            print("🎉 ALL FILES APPEAR TO BE INDEXED!")
    
    print(f"⚡ Items processed (last run): {items_processed}")
    print(f"❌ Items failed (last run): {items_failed}")
    print(f"🕐 Start time: {format_timestamp(start_time)}")
    print(f"🕐 End time: {format_timestamp(end_time)}")
    
    # Check if indexer is done
    if status.lower() in ['idle', 'completed']:
        print("✅ INDEXER IS COMPLETE!")
        break
    elif status.lower() == 'error':
        print("❌ INDEXER HAS ERRORS!")
        break
    elif status.lower() == 'running':
        print("🔄 Indexer is still running...")
        
        if check_count < max_checks:
            print(f"⏳ Waiting {check_interval} seconds for next check...")
            time.sleep(check_interval)
    else:
        print(f"ℹ️  Indexer status: {status}")
        
        if check_count < max_checks:
            time.sleep(check_interval)

print(f"\n" + "=" * 60)
print("📋 FINAL SUMMARY:")
print(f"🔍 Total checks performed: {check_count}")

final_doc_count = get_document_count()
final_status = get_indexer_status()

if final_doc_count is not None:
    print(f"📈 Final document count: {final_doc_count}")
    
if expected_files:
    print(f"📁 Expected JSONL files: {expected_files}")
    if final_doc_count and final_doc_count >= expected_files:
        print("🎉 INDEXING APPEARS COMPLETE!")
    else:
        print("⏳ Indexing may still be in progress...")

if final_status:
    status = final_status.get('status', 'unknown')
    print(f"📊 Final indexer status: {status}")

print("\n💡 TO KNOW WHEN INDEXING IS COMPLETE:")
print("✅ Status changes from 'running' to 'idle' or 'completed'")
print("✅ Document count stops increasing")
print("✅ Document count matches (or exceeds) the number of JSONL files")
print("✅ Check the Azure Portal: https://portal.azure.com")
print("   -> Azure AI Search -> az-use1-ai-search -> Indexers -> cobol-indexer")

print("=" * 60)
