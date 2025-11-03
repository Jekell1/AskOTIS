#!/usr/bin/env python3
"""Search ui_paths for PGMENU -> REGPAY connection."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_ui_paths',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("="*100)
print("SEARCHING UI_PATHS FOR PGMENU -> REGPAY")
print("="*100)

# Get schema first
from azure.search.documents.indexes import SearchIndexClient
index_client = SearchIndexClient(endpoint=config['SEARCH_ENDPOINT'], 
                                  credential=AzureKeyCredential(config['SEARCH_KEY']))
index = index_client.get_index('new_cobol_ui_paths')
fields = [f.name for f in index.fields]
print(f"\nAvailable fields: {', '.join(sorted(fields))}\n")

# Search for REGPAY in ui_paths
results = list(search_client.search(
    'REGPAY',
    top=20
))

print(f"Found {len(results)} UI paths mentioning REGPAY\n")

for i, r in enumerate(results[:10], 1):
    print(f"\n{i}. UI Path Record:")
    # Print all fields
    for key, value in sorted(r.items()):
        if key.startswith('_') or key == '@search.score':
            continue
        if isinstance(value, list):
            print(f"   {key}: {value[:10]}")  # First 10 items
        elif isinstance(value, str) and len(value) > 200:
            print(f"   {key}: {value[:200]}...")
        else:
            print(f"   {key}: {value}")

# Search specifically for PGMENU
print("\n" + "="*100)
print("SEARCHING FOR PGMENU")
print("="*100)

results = list(search_client.search(
    'PGMENU',
    top=10
))

print(f"\nFound {len(results)} UI paths mentioning PGMENU\n")

for i, r in enumerate(results[:5], 1):
    print(f"\n{i}. UI Path Record:")
    for key, value in sorted(r.items()):
        if key.startswith('_') or key == '@search.score':
            continue
        if isinstance(value, list):
            if 'REGPAY' in str(value):
                print(f"   {key}: {value} ✓ CONTAINS REGPAY")
            else:
                print(f"   {key}: {value[:10]}")
        elif isinstance(value, str) and len(value) > 200:
            print(f"   {key}: {value[:200]}...")
        else:
            print(f"   {key}: {value}")

print("\n" + "="*100)
