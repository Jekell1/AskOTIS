"""Find the program ID for COLLECTION MAINTENANCE MENU"""
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

# Search in program_meta for COLLECTION MAINTENANCE
print("=" * 80)
print("SEARCHING PROGRAM_META FOR COLLECTION MAINTENANCE")
print("=" * 80)

url = f"{ep}/indexes/new_cobol_program_meta/docs/search?api-version={API_VERSION}"
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': 'COLLECTION MAINTENANCE MENU',
    'top': 20,
    'queryType': 'simple',
    'select': 'program_id,program_role,ui_flag,program_summary'
}

r = requests.post(url, headers=headers, json=payload)
if r.status_code == 200:
    results = r.json().get('value', [])
    print(f"\nFound {len(results)} programs:")
    for i, doc in enumerate(results, 1):
        prog_id = doc.get('program_id', 'Unknown')
        role = doc.get('program_role', 'Unknown')
        ui_flag = doc.get('ui_flag', False)
        summary = (doc.get('program_summary') or '')[:100]
        print(f"\n{i}. {prog_id}")
        print(f"   Role: {role}, UI: {ui_flag}")
        print(f"   Summary: {summary}")
else:
    print(f"Error: {r.status_code}")

# Also search for screen nodes
print("\n" + "=" * 80)
print("SEARCHING SCREEN_NODES FOR COLLECTION MAINTENANCE MENU")
print("=" * 80)

url = f"{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version={API_VERSION}"
payload = {
    'search': '"COLLECTION MAINTENANCE MENU"',
    'top': 10,
    'queryType': 'simple',
    'select': 'id,screen_id,program_id,summary_text'
}

r = requests.post(url, headers=headers, json=payload)
if r.status_code == 200:
    results = r.json().get('value', [])
    print(f"\nFound {len(results)} screen nodes:")
    for i, doc in enumerate(results, 1):
        screen_id = doc.get('screen_id', 'Unknown')
        prog_id = doc.get('program_id', 'Unknown')
        summary = (doc.get('summary_text') or '')[:200]
        print(f"\n{i}. Screen ID: {screen_id}")
        print(f"   Program ID: {prog_id}")
        print(f"   Summary: {summary}")
else:
    print(f"Error: {r.status_code}")

# Check if any of these program IDs exist in menu_trees
print("\n" + "=" * 80)
print("CHECKING IF THESE PROGRAMS HAVE MENU TREES")
print("=" * 80)

# Collect candidate program IDs
candidates = set()

# Re-search to collect IDs
url = f"{ep}/indexes/new_cobol_program_meta/docs/search?api-version={API_VERSION}"
payload = {
    'search': 'COLLECTION MAINTENANCE',
    'top': 20,
    'queryType': 'simple',
    'select': 'program_id'
}
r = requests.post(url, headers=headers, json=payload)
if r.status_code == 200:
    for doc in r.json().get('value', []):
        candidates.add(doc.get('program_id', ''))

url = f"{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version={API_VERSION}"
payload = {
    'search': '"COLLECTION MAINTENANCE"',
    'top': 10,
    'select': 'program_id'
}
r = requests.post(url, headers=headers, json=payload)
if r.status_code == 200:
    for doc in r.json().get('value', []):
        candidates.add(doc.get('program_id', ''))

candidates.discard('')
candidates.discard(None)

print(f"\nChecking {len(candidates)} candidate program IDs...")
for prog_id in sorted(candidates):
    get_url = f"{ep}/indexes/new_cobol_menu_trees/docs('{prog_id}')?api-version={API_VERSION}"
    r = requests.get(get_url, headers={'api-key': key})
    
    if r.status_code == 200:
        doc = r.json()
        nodes = doc.get('total_nodes', 0)
        ui = doc.get('total_ui_nodes', 0)
        depth = doc.get('max_depth', 0)
        print(f"  ✅ {prog_id} - HAS TREE: {nodes} nodes, {ui} UI, depth {depth}")
    else:
        print(f"  ❌ {prog_id} - No tree")
