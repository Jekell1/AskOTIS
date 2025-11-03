#!/usr/bin/env python3
"""
Delete coboldata-ds-* data sources from Azure AI Search
"""
import json
import requests
from pathlib import Path

# Load configuration
config_path = Path(__file__).parent / "local.settings.json"
with open(config_path, 'r') as f:
    config = json.load(f)

values = config["Values"]
search_endpoint = values["SEARCH_ENDPOINT"]
search_key = values["SEARCH_KEY"]

print(f"Deleting mycoboldata-ds-* data sources from: {search_endpoint}")

# Headers
headers = {
    "api-key": search_key,
    "Content-Type": "application/json"
}

# List all data sources first
url = f"{search_endpoint}/datasources?api-version=2024-07-01"

try:
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        data = response.json()
        datasources = data.get('value', [])
        
        # Filter for mycoboldata-ds-* pattern
        cobol_datasources = [ds for ds in datasources if ds['name'].startswith('mycoboldata-ds-')]
        
        print(f"Found {len(cobol_datasources)} mycoboldata-ds-* data sources to delete:")
        
        if not cobol_datasources:
            print("No mycoboldata-ds-* data sources found.")
            exit(0)
        
        # Show what will be deleted
        for ds in cobol_datasources[:5]:  # Show first 5
            print(f"  - {ds['name']}")
        if len(cobol_datasources) > 5:
            print(f"  ... and {len(cobol_datasources) - 5} more")
        
        confirm = input(f"\nDelete all {len(cobol_datasources)} mycoboldata-ds-* data sources? (y/N): ")
        
        if not confirm.lower().startswith('y'):
            print("Cancelled.")
            exit(0)
        
        # Delete them
        deleted_count = 0
        failed_count = 0
        
        for ds in cobol_datasources:
            ds_name = ds['name']
            delete_url = f"{search_endpoint}/datasources/{ds_name}?api-version=2024-07-01"
            
            del_response = requests.delete(delete_url, headers=headers)
            
            if del_response.status_code in [200, 204]:
                deleted_count += 1
                print(f"✅ Deleted: {ds_name}")
            else:
                failed_count += 1
                print(f"❌ Failed to delete: {ds_name} (Status: {del_response.status_code})")
        
        print(f"\n=== Summary ===")
        print(f"Successfully deleted: {deleted_count}")
        print(f"Failed to delete: {failed_count}")
        print(f"Data sources freed up: {deleted_count}")
        
        if deleted_count > 0:
            print(f"\n✅ You now have space to create new data sources!")
        
    else:
        print(f"❌ Failed to list data sources. Status: {response.status_code}")
        if response.content:
            print(f"Response: {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")
