#!/usr/bin/env python3
"""Search all menu trees for REGPAY."""

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

# Get all menu trees
results = list(client.search('*', top=10000))

print(f"Total menu trees: {len(results)}")
print("\nSearching for REGPAY in tree_json fields...")

found_trees = []
for r in results:
    root = r.get('root_program_id')
    tree_json = r.get('tree_json', '')
    
    # Check if REGPAY is in the tree
    if 'REGPAY' in str(tree_json):
        found_trees.append(root)
        print(f"\n✓ Found REGPAY in tree rooted at: {root}")
        
        # Parse and show context
        if isinstance(tree_json, str):
            try:
                tree = json.loads(tree_json)
                # Find REGPAY in the tree structure
                def find_regpay(node, path=[]):
                    current_path = path + [node.get('program_id', '?')]
                    if node.get('program_id') == 'REGPAY':
                        print(f"  Path to REGPAY: {' → '.join(current_path)}")
                    for child in node.get('children', []):
                        find_regpay(child, current_path)
                
                find_regpay(tree)
            except:
                pass

if not found_trees:
    print("\n❌ REGPAY NOT FOUND in any menu tree")
    print("\nThis means REGPAY is likely:")
    print("  1. Invoked directly by transaction code (not through menus)")
    print("  2. In a menu structure that wasn't indexed")
    print("  3. Accessed through a different navigation mechanism")
else:
    print(f"\n\nSummary: REGPAY found in {len(found_trees)} menu tree(s): {', '.join(found_trees)}")
