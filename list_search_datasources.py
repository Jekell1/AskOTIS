#!/usr/bin/env python3
"""
List and optionally delete Azure AI Search data sources
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

print(f"Listing data sources from: {search_endpoint}")

# Headers
headers = {
    "api-key": search_key,
    "Content-Type": "application/json"
}

# List all data sources
url = f"{search_endpoint}/datasources?api-version=2024-07-01"

try:
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        data = response.json()
        datasources = data.get('value', [])
        
        print(f"\nFound {len(datasources)} data sources:")
        print("=" * 50)
        
        # Group by name patterns to identify bulk-created ones
        patterns = {}
        for ds in datasources:
            name = ds['name']
            # Look for patterns like LIBUT__FILENAME, etc.
            if '__' in name:
                prefix = name.split('__')[0]
                if prefix not in patterns:
                    patterns[prefix] = []
                patterns[prefix].append(name)
            else:
                if 'individual' not in patterns:
                    patterns['individual'] = []
                patterns['individual'].append(name)
        
        # Show summary by pattern
        for pattern, names in patterns.items():
            print(f"\n{pattern}: {len(names)} data sources")
            if len(names) <= 5:
                for name in names:
                    print(f"  - {name}")
            else:
                print(f"  - {names[0]}")
                print(f"  - {names[1]}")
                print(f"  - ...")
                print(f"  - {names[-2]}")
                print(f"  - {names[-1]}")
        
        # Ask if user wants to delete bulk-created ones
        print(f"\nTotal: {len(datasources)} data sources (limit is 50)")
        
        # Offer to delete data sources matching COBOL patterns
        cobol_patterns = [k for k in patterns.keys() if k in ['LIBUT', 'LIBWI', 'MN', 'SP', 'UP', 'UT', 'WI']]
        if cobol_patterns:
            print(f"\nFound COBOL-related data sources with patterns: {cobol_patterns}")
            total_cobol = sum(len(patterns[p]) for p in cobol_patterns)
            print(f"This accounts for {total_cobol} data sources.")
            
            response = input(f"\nDelete all COBOL-related data sources? This will free up space. (y/N): ")
            if response.lower().startswith('y'):
                deleted_count = 0
                for pattern in cobol_patterns:
                    for ds_name in patterns[pattern]:
                        delete_url = f"{search_endpoint}/datasources/{ds_name}?api-version=2024-07-01"
                        del_response = requests.delete(delete_url, headers=headers)
                        if del_response.status_code in [200, 204]:
                            deleted_count += 1
                            print(f"  ✅ Deleted: {ds_name}")
                        else:
                            print(f"  ❌ Failed to delete: {ds_name} (Status: {del_response.status_code})")
                
                print(f"\nDeleted {deleted_count} data sources. You can now create new ones!")
        
    else:
        print(f"❌ Failed to list data sources. Status: {response.status_code}")
        if response.content:
            print(f"Response: {response.text}")
        
except Exception as e:
    print(f"❌ Error: {e}")
