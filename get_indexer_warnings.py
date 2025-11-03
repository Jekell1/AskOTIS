import json
import requests

# Load configuration
with open('local.settings.json', 'r') as f:
    config = json.load(f)

search_endpoint = config['Values']['SEARCH_ENDPOINT']
search_admin_key = config['Values']['SEARCH_KEY']

def get_detailed_status():
    url = f"{search_endpoint}/indexers/cobol-indexer/status"
    headers = {'api-key': search_admin_key}
    params = {'api-version': '2024-07-01'}
    
    response = requests.get(url, headers=headers, params=params)
    
    if response.status_code == 200:
        return response.json()
    else:
        print(f"Error getting status: {response.status_code}")
        return None

print("Detailed Indexer Status and Warnings")
print("=" * 50)

status_data = get_detailed_status()

if status_data:
    print(f"Status: {status_data.get('status', 'Unknown')}")
    
    last_result = status_data.get('lastResult')
    if last_result:
        print(f"Items Processed: {last_result.get('itemsProcessed', 0)}")
        print(f"Items Failed: {last_result.get('itemsFailed', 0)}")
        
        # Show warnings
        warnings = last_result.get('warnings', [])
        if warnings:
            print(f"\nWarnings ({len(warnings)} total):")
            print("-" * 30)
            
            # Show first 10 warnings to understand the pattern
            for i, warning in enumerate(warnings[:10]):
                print(f"Warning {i+1}:")
                print(f"  Key: {warning.get('key', 'Unknown')}")
                print(f"  Message: {warning.get('message', 'Unknown')}")
                print(f"  Name: {warning.get('name', 'Unknown')}")
                print()
            
            if len(warnings) > 10:
                print(f"... and {len(warnings) - 10} more warnings")
        
        # Show errors
        errors = last_result.get('errors', [])
        if errors:
            print(f"\nErrors ({len(errors)} total):")
            print("-" * 30)
            for i, error in enumerate(errors[:5]):
                print(f"Error {i+1}:")
                print(f"  Key: {error.get('key', 'Unknown')}")
                print(f"  Message: {error.get('errorMessage', 'Unknown')}")
                print()

print("\nAnalysis:")
print("- If there are many 'field not found' warnings, the JSONL structure might not match expectations")
print("- If there are skillset errors, the Azure Function might be having issues")
print("- Check the Azure Function logs for more details if needed")
