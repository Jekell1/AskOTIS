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

# Azure AI Search REST API endpoint for skillsets
base_url = f"https://{search_service_name}.search.windows.net"
headers = {
    'Content-Type': 'application/json',
    'api-key': search_admin_key
}
params = {
    'api-version': '2023-11-01'
}

def list_skillsets():
    """Get all skillsets"""
    url = f"{base_url}/skillsets"
    response = requests.get(url, headers=headers, params=params)
    if response.status_code == 200:
        return response.json()['value']
    return []

def delete_skillset(skillset_name):
    """Delete a specific skillset"""
    url = f"{base_url}/skillsets/{skillset_name}"
    response = requests.delete(url, headers=headers, params=params)
    return response.status_code

# Get all skillsets
skillsets = list_skillsets()
print(f"Found {len(skillsets)} total skillsets")

# Filter for COBOL-related skillsets
cobol_patterns = ['coboldata-ss-', 'mycoboldata-ss-']
cobol_skillsets = []

for skillset in skillsets:
    name = skillset.get('name', '')
    for pattern in cobol_patterns:
        if name.startswith(pattern):
            cobol_skillsets.append(name)
            break

print(f"Found {len(cobol_skillsets)} COBOL-related skillsets to delete:")
for name in cobol_skillsets:
    print(f"  - {name}")

if cobol_skillsets:
    print(f"\nDeleting {len(cobol_skillsets)} COBOL skillsets...")
    
    deleted_count = 0
    failed_count = 0
    
    for skillset_name in cobol_skillsets:
        try:
            status_code = delete_skillset(skillset_name)
            if status_code == 204:  # Success
                print(f"✓ Deleted: {skillset_name}")
                deleted_count += 1
            else:
                print(f"✗ Failed to delete {skillset_name}: Status {status_code}")
                failed_count += 1
        except Exception as e:
            print(f"✗ Error deleting {skillset_name}: {e}")
            failed_count += 1
    
    print(f"\nSummary:")
    print(f"  Deleted: {deleted_count}")
    print(f"  Failed: {failed_count}")
    
    # List remaining skillsets
    remaining_skillsets = list_skillsets()
    remaining_cobol = [s['name'] for s in remaining_skillsets if any(s['name'].startswith(p) for p in cobol_patterns)]
    
    if remaining_cobol:
        print(f"\nRemaining COBOL skillsets: {len(remaining_cobol)}")
        for name in remaining_cobol:
            print(f"  - {name}")
    else:
        print(f"\nAll COBOL skillsets deleted successfully!")
        
    print(f"\nTotal remaining skillsets: {len(remaining_skillsets)}")
else:
    print("No COBOL skillsets found to delete.")
