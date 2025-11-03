#!/usr/bin/env python3
"""Get PGMENU tree details."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('', filter="root_program_id eq 'PGMENU'", top=1))

if results:
    r = results[0]
    print("PGMENU Tree Record:")
    print("="*100)
    
    for key, value in r.items():
        if key == 'tree_json':
            print(f"\n{key}:")
            print(f"  Type: {type(value)}")
            if isinstance(value, str):
                print(f"  Length: {len(value)}")
                print(f"  First 2000 chars: {value[:2000]}")
                try:
                    parsed = json.loads(value)
                    print(f"\n  Parsed type: {type(parsed)}")
                    if isinstance(parsed, dict):
                        print(f"  Keys: {list(parsed.keys())}")
                        if 'nodes' in parsed:
                            print(f"  Nodes count: {len(parsed['nodes'])}")
                            print(f"  First 10 nodes: {parsed['nodes'][:10]}")
                except:
                    print("  Could not parse as JSON")
            else:
                print(f"  Value: {value}")
        else:
            print(f"{key}: {value}")
else:
    print("No PGMENU tree found")
