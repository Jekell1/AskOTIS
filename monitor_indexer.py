import os, json, time, sys, requests

"""Poll an Azure AI Search indexer until it finishes or times out.

Usage:
  pwsh> python monitor_indexer.py --name idx-files --interval 15 --timeout 1800

Logic:
 - Calls /indexers/{name}/status every interval seconds
 - Prints items processed/failed for last result if available
 - Exits 0 on success, non-zero on failure or timeout
"""

API_VERSION = "2024-07-01"

def load_settings():
    path = os.path.join(os.getcwd(), 'local.settings.json')
    if os.path.exists(path):
        try:
            data = json.load(open(path,'r',encoding='utf-8'))
            return data.get('Values', {})
        except Exception:
            pass
    return {}

def get_endpoint_key():
    vals = load_settings()
    ep = (os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT') or
          vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT'))
    key = (os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY') or
           vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY'))
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def fetch_status(ep, key, name):
    url = f"{ep}/indexers/{name}/status?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        return None, f"HTTP {r.status_code}: {r.text[:120]}"
    data = r.json()
    last = data.get('lastResult') or {}
    status = last.get('status')
    items_processed = last.get('itemsProcessed')
    items_failed = last.get('itemsFailed')
    err = last.get('errorMessage')
    return {
        'status': status,
        'processed': items_processed,
        'failed': items_failed,
        'error': err
    }, None

def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument('--name', required=True, help='Indexer name, e.g. idx-files')
    ap.add_argument('--interval', type=int, default=20, help='Polling interval seconds')
    ap.add_argument('--timeout', type=int, default=1800, help='Timeout seconds (overall)')
    args = ap.parse_args()

    ep, key = get_endpoint_key()
    print(f"Monitoring indexer {args.name} @ {ep}")
    start = time.time()
    last_status = None
    while True:
        if time.time() - start > args.timeout:
            print('Timeout reached.')
            sys.exit(3)
        stat, err = fetch_status(ep, key, args.name)
        if err:
            print('Error fetching status:', err)
        else:
            if stat['status'] != last_status:
                print(f"Status: {stat['status']}")
                last_status = stat['status']
            print(f"  processed={stat['processed']} failed={stat['failed']}")
            if stat['error']:
                print(f"  error: {stat['error'][:200]}")
            if stat['status'] in ('success','transientFailure','persistentFailure','reset','inProgress', None):
                if stat['status'] == 'success':
                    print('Indexer completed successfully.')
                    sys.exit(0)
                if stat['status'] in ('transientFailure','persistentFailure'):
                    print('Indexer finished with failure state.')
                    sys.exit(4)
        time.sleep(args.interval)

if __name__ == '__main__':
    main()
import json
import requests
import time

# Load configuration
with open('local.settings.json', 'r') as f:
    config = json.load(f)

search_endpoint = config['Values']['SEARCH_ENDPOINT']
search_admin_key = config['Values']['SEARCH_KEY']

def check_indexer_status():
    url = f"{search_endpoint}/indexers/cobol-indexer/status"
    headers = {'api-key': search_admin_key}
    params = {'api-version': '2024-07-01'}
    
    response = requests.get(url, headers=headers, params=params)
    
    if response.status_code == 200:
        return response.json()
    else:
        print(f"Error getting status: {response.status_code}")
        return None

def get_document_count():
    url = f"{search_endpoint}/indexes/cobol-index/stats"
    headers = {'api-key': search_admin_key}
    params = {'api-version': '2024-07-01'}
    
    response = requests.get(url, headers=headers, params=params)
    
    if response.status_code == 200:
        stats = response.json()
        return stats.get('documentCount', 0)
    return 0

print("Monitoring COBOL Indexer Progress")
print("=" * 50)

# Check status a few times
for i in range(5):
    status_data = check_indexer_status()
    doc_count = get_document_count()
    
    if status_data:
        status = status_data.get('status', 'Unknown')
        print(f"Check {i+1}: Status = {status}, Documents in index = {doc_count}")
        
        last_result = status_data.get('lastResult')
        if last_result:
            result_status = last_result.get('status', 'Unknown')
            items_processed = last_result.get('itemsProcessed', 0)
            items_failed = last_result.get('itemsFailed', 0)
            start_time = last_result.get('startTime', 'Unknown')
            end_time = last_result.get('endTime', 'Still running')
            
            print(f"  Last Result: {result_status}")
            print(f"  Items Processed: {items_processed}")
            print(f"  Items Failed: {items_failed}")
            print(f"  Start Time: {start_time}")
            print(f"  End Time: {end_time}")
            
            if last_result.get('errors'):
                print(f"  Errors: {len(last_result['errors'])}")
                for j, error in enumerate(last_result['errors'][:3]):
                    print(f"    Error {j+1}: {error.get('errorMessage', 'Unknown')}")
            
            if last_result.get('warnings'):
                print(f"  Warnings: {len(last_result['warnings'])}")
        
        if status == 'running':
            print("  Indexer is still processing files...")
        elif status == 'success':
            print("  ✅ Indexer completed successfully!")
            break
        elif status == 'error':
            print("  ❌ Indexer encountered errors")
            break
    else:
        print(f"Check {i+1}: Could not get status")
    
    print("-" * 40)
    
    if i < 4:  # Don't sleep on the last iteration
        time.sleep(10)  # Wait 10 seconds between checks

print("\nFinal Status:")
print("=" * 30)

final_status = check_indexer_status()
final_doc_count = get_document_count()

if final_status:
    status = final_status.get('status', 'Unknown')
    print(f"Indexer Status: {status}")
    print(f"Documents in Index: {final_doc_count}")
    
    if final_doc_count > 0:
        print(f"\n🎉 Success! {final_doc_count} documents have been indexed!")
        print("Your COBOL files are now searchable in Azure AI Search.")
        print("\nNext steps:")
        print("1. Test search queries")
        print("2. Set up a search application")
        print("3. Configure vector search if needed")
    else:
        print("\n⚠️  No documents were indexed. Check for errors above.")

print(f"\nTo view more details, visit the Azure Portal:")
print(f"https://portal.azure.com -> Azure AI Search -> {search_endpoint.split('//')[1].split('.')[0]} -> Indexers")
