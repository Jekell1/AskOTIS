import json
import requests

# Load configuration
with open('local.settings.json', 'r') as f:
    config = json.load(f)

search_endpoint = config['Values']['SEARCH_ENDPOINT']
search_service_name = search_endpoint.replace('https://', '').replace('.search.windows.net', '')
search_admin_key = config['Values']['SEARCH_KEY']

# Delete the existing indexer
url = f"https://{search_service_name}.search.windows.net/indexers/cobol-indexer"
headers = {'api-key': search_admin_key}
params = {'api-version': '2024-07-01'}

print("Deleting existing indexer...")
response = requests.delete(url, headers=headers, params=params)

if response.status_code == 204:
    print("✅ Existing indexer deleted successfully")
elif response.status_code == 404:
    print("ℹ️  No existing indexer to delete")
else:
    print(f"⚠️  Delete response: {response.status_code}")

print("Now run: python create_cobol_indexer.py")
