"""Check if tree_json has actual data despite 0 node count"""
import os
import json
import requests

# Load settings
try:
    vals = json.load(open('local.settings.json', 'r')).get('Values', {})
    for k in ['SEARCH_ENDPOINT', 'SEARCH_KEY', 'AZURE_SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY']:
        if k in vals and k not in os.environ:
            os.environ[k] = vals[k]
except Exception:
    pass

ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_menu_trees'

# Check a few sample trees
sample_roots = [
    'WIMENU',  # 21 nodes claimed
    'SPMENU',  # 14 nodes claimed
    'LPMENU',  # 10 nodes claimed
    'CHLOAN',  # 2 nodes claimed
]

for root in sample_roots:
    print(f"\n{'='*80}")
    print(f"Checking: {root}")
    print('='*80)
    
    get_url = f"{ep}/indexes/{INDEX}/docs('{root}')?api-version={API_VERSION}"
    r = requests.get(get_url, headers={'api-key': key})
    
    if r.status_code == 200:
        doc = r.json()
        tree_json_str = doc.get('tree_json', '')
        
        print(f"Root Program ID: {doc.get('root_program_id')}")
        print(f"Total Nodes (field): {doc.get('total_nodes', 0)}")
        print(f"UI Nodes (field): {doc.get('total_ui_nodes', 0)}")
        print(f"Max Depth (field): {doc.get('max_depth', 0)}")
        print(f"Tree JSON length: {len(tree_json_str)} characters")
        
        if tree_json_str:
            try:
                tree = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
                print(f"Tree JSON parsed successfully")
                print(f"Tree type: {type(tree)}")
                
                if isinstance(tree, list):
                    print(f"Tree is list with {len(tree)} items")
                    if tree:
                        print(f"First item keys: {tree[0].keys() if tree else 'empty'}")
                        if tree[0]:
                            print(f"First item sample: {json.dumps(tree[0], indent=2)[:500]}")
                elif isinstance(tree, dict):
                    print(f"Tree is dict with keys: {tree.keys()}")
                    print(f"Sample: {json.dumps(tree, indent=2)[:500]}")
                else:
                    print(f"Tree is unexpected type: {type(tree)}")
                    
            except Exception as e:
                print(f"❌ Error parsing tree_json: {e}")
                print(f"First 200 chars: {tree_json_str[:200]}")
        else:
            print("❌ No tree_json data")
    else:
        print(f"❌ Error: {r.status_code}")
