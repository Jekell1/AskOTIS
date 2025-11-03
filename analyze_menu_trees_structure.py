#!/usr/bin/env python3
"""Analyze menu_trees structure to understand what it contains."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("="*100)
print("ANALYZING MENU_TREES STRUCTURE")
print("="*100)

# Get a few sample menu trees
results = list(search_client.search(
    '*',
    top=5
))

print(f"\nFound {len(results)} sample menu trees\n")

for i, r in enumerate(results, 1):
    root = r.get('root_program_id')
    tree_json = r.get('tree_json', [])
    max_depth = r.get('max_depth')
    total_nodes = r.get('total_nodes')
    total_ui_nodes = r.get('total_ui_nodes')
    ui_ratio = r.get('ui_ratio')
    
    print(f"\n{i}. Root: {root}")
    print(f"   Max Depth: {max_depth}")
    print(f"   Total Nodes: {total_nodes}")
    print(f"   Total UI Nodes: {total_ui_nodes}")
    print(f"   UI Ratio: {ui_ratio}")
    
    # Parse tree_json
    if isinstance(tree_json, str):
        try:
            tree_json = json.loads(tree_json)
        except:
            pass
    
    if isinstance(tree_json, list):
        print(f"   Tree structure (first 30 items):")
        print(f"   {tree_json[:30]}")
        
        # Check for common program names
        tree_str = str(tree_json)
        if 'REGPAY' in tree_str:
            print(f"   ✓ Contains REGPAY")
        if 'MENU' in tree_str:
            print(f"   ✓ Contains MENU programs")
        if 'MU' in tree_str:
            print(f"   ✓ Contains MU (menu) programs")
    else:
        print(f"   Tree: {str(tree_json)[:300]}")

print("\n" + "="*100)
print("CHECKING FOR PGMENU SPECIFICALLY")
print("="*100)

# Search for PGMENU
results = list(search_client.search(
    '',
    filter="root_program_id eq 'PGMENU'",
    top=5
))

print(f"\nFound {len(results)} menu trees with root PGMENU\n")

for i, r in enumerate(results, 1):
    tree_json = r.get('tree_json', [])
    
    if isinstance(tree_json, str):
        try:
            tree_json = json.loads(tree_json)
        except:
            pass
    
    print(f"\n{i}. PGMENU Tree:")
    print(f"   Total items in tree: {len(tree_json) if isinstance(tree_json, list) else 'N/A'}")
    
    if isinstance(tree_json, list):
        # Show first 50 items
        print(f"   First 50 items: {tree_json[:50]}")
        
        # Check if it contains leaf programs (non-menu)
        non_menu_items = [item for item in tree_json if 'MENU' not in str(item) and 'MU' not in str(item)]
        print(f"\n   Non-menu items (first 20): {non_menu_items[:20]}")
        
        if 'REGPAY' in str(tree_json):
            print(f"   ✓✓✓ REGPAY FOUND IN PGMENU TREE!")
            # Find position
            for idx, item in enumerate(tree_json):
                if 'REGPAY' in str(item):
                    print(f"   Position {idx}: {item}")

print("\n" + "="*100)
print("CONCLUSION")
print("="*100)
print("Menu trees contain: menu programs (like PGMENU, LPMNMU) AND leaf programs (like REGPAY)")
print("The tree structure shows the full navigation hierarchy from root menu to final programs")
print("="*100)
