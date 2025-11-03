"""Probe menu_trees index for COLLECTION MAINTENANCE MENU"""
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

if not ep or not key:
    print('Missing endpoint/key')
    exit(1)

API_VERSION = '2025-08-01-preview'
INDEX = 'new_cobol_menu_trees'

# Search for COLLECTION MAINTENANCE related menu trees
url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
headers = {'api-key': key, 'Content-Type': 'application/json'}

search_terms = [
    'COLLECTION',
    'COLLECT',
    'COLL'
]

print("=" * 80)
print("SEARCHING MENU_TREES INDEX FOR COLLECTION MAINTENANCE MENU")
print("=" * 80)

for term in search_terms:
    print(f"\n🔍 Searching for: {term}")
    print("-" * 80)
    
    payload = {
        'search': term,
        'top': 10,
        'queryType': 'simple',
        'select': 'root_program_id,total_nodes,total_ui_nodes,max_depth'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code != 200:
        print(f"❌ Error: {r.status_code} - {r.text[:200]}")
        continue
    
    results = r.json().get('value', [])
    print(f"Found {len(results)} results:")
    
    for i, doc in enumerate(results, 1):
        root = doc.get('root_program_id', 'Unknown')
        nodes = doc.get('total_nodes', 0)
        ui_nodes = doc.get('total_ui_nodes', 0)
        depth = doc.get('max_depth', 0)
        print(f"  {i}. {root}")
        print(f"     Nodes: {nodes}, UI: {ui_nodes}, Depth: {depth}")

# Now get a specific tree if we find one
print("\n" + "=" * 80)
print("CHECKING FOR SPECIFIC PROGRAM IDS")
print("=" * 80)

candidates = [
    'COLLMENU',
    'COLLMAINT',
    'COLLECTION_MENU',
    'COLLECTION_MAINT',
    'COLMENU',
    'COLL_MENU',
    'COLMAINT'
]

for prog_id in candidates:
    print(f"\n🔍 Checking: {prog_id}")
    
    # Try exact key lookup
    get_url = f"{ep}/indexes/{INDEX}/docs('{prog_id}')?api-version={API_VERSION}"
    r = requests.get(get_url, headers={'api-key': key})
    
    if r.status_code == 200:
        doc = r.json()
        print(f"✅ FOUND: {prog_id}")
        print(f"   Total nodes: {doc.get('total_nodes', 0)}")
        print(f"   UI nodes: {doc.get('total_ui_nodes', 0)}")
        print(f"   Max depth: {doc.get('max_depth', 0)}")
        
        # Show tree structure
        tree_json = doc.get('tree_json', '[]')
        try:
            tree = json.loads(tree_json) if isinstance(tree_json, str) else tree_json
            print(f"\n   Tree structure preview:")
            
            def show_tree(node, depth=0, max_depth=3):
                if depth > max_depth:
                    return
                prog = node.get('program_id', 'Unknown')
                is_ui = node.get('ui', False)
                ui_mark = '🖥️ ' if is_ui else ''
                indent = '  ' * depth
                print(f"   {indent}{ui_mark}{prog}")
                
                for child in node.get('children', [])[:5]:  # Show first 5 children
                    show_tree(child, depth + 1, max_depth)
                
                if len(node.get('children', [])) > 5:
                    print(f"   {indent}  ... ({len(node.get('children', [])) - 5} more)")
            
            if isinstance(tree, list):
                for t in tree[:1]:  # Just first root
                    show_tree(t)
            else:
                show_tree(tree)
                
        except Exception as e:
            print(f"   ⚠️ Could not parse tree: {e}")
    else:
        print(f"   Not found (status: {r.status_code})")

print("\n" + "=" * 80)
print("DONE")
print("=" * 80)
