"""List all menu trees in the index"""
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

url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Get all documents
all_docs = []
skip = 0
batch_size = 100

while True:
    payload = {
        'search': '*',
        'top': batch_size,
        'skip': skip,
        'queryType': 'simple',
        'select': 'root_program_id,total_nodes,total_ui_nodes,max_depth'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    if r.status_code != 200:
        print(f"Error: {r.status_code}")
        break
    
    batch = r.json().get('value', [])
    if not batch:
        break
    
    all_docs.extend(batch)
    skip += batch_size
    
    if len(batch) < batch_size:
        break

print("=" * 80)
print(f"ALL MENU TREES IN INDEX ({len(all_docs)} total)")
print("=" * 80)

# Sort by total nodes descending
all_docs.sort(key=lambda x: x.get('total_nodes', 0), reverse=True)

print(f"\n{'Root Program ID':<40} {'Nodes':<8} {'UI':<6} {'Depth':<6}")
print("-" * 80)

for doc in all_docs:
    root = doc.get('root_program_id', 'Unknown')
    nodes = doc.get('total_nodes', 0)
    ui = doc.get('total_ui_nodes', 0)
    depth = doc.get('max_depth', 0)
    print(f"{root:<40} {nodes:<8} {ui:<6} {depth:<6}")

# Look for anything with COLLECTION or LOAN in the name
print("\n" + "=" * 80)
print("TREES WITH 'LOAN' IN NAME")
print("=" * 80)
loan_trees = [d for d in all_docs if 'LOAN' in d.get('root_program_id', '').upper()]
for doc in loan_trees:
    root = doc.get('root_program_id', 'Unknown')
    nodes = doc.get('total_nodes', 0)
    ui = doc.get('total_ui_nodes', 0)
    print(f"  {root} - {nodes} nodes, {ui} UI")

print("\n" + "=" * 80)
print("TREES WITH 'MENU' IN NAME")
print("=" * 80)
menu_trees = [d for d in all_docs if 'MENU' in d.get('root_program_id', '').upper()]
for doc in menu_trees:
    root = doc.get('root_program_id', 'Unknown')
    nodes = doc.get('total_nodes', 0)
    ui = doc.get('total_ui_nodes', 0)
    print(f"  {root} - {nodes} nodes, {ui} UI")
