"""
Adds summary_vector_3072 field to new_cobol_screen_nodes (3072 dims to match other indexes).
Keeps existing summary_vector (1536) for compatibility.
"""
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
INDEX = 'new_cobol_screen_nodes'
NEW_VECTOR_FIELD = 'summary_vector_3072'
DIM = 3072

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

load_settings()
ep, key = resolve()

base_url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
headers = {'api-key': key, 'Content-Type': 'application/json'}

print(f"Fetching index definition for {INDEX}...")
r = requests.get(base_url, headers=headers)
if r.status_code != 200:
    print(f"ERROR: {r.status_code} {r.text}")
    sys.exit(1)

idx_def = r.json()

# Check if field already exists
if any(f['name'] == NEW_VECTOR_FIELD for f in idx_def['fields']):
    print(f"{NEW_VECTOR_FIELD} already exists. No changes needed.")
    sys.exit(0)

# Add new vector field
print(f"Adding {NEW_VECTOR_FIELD} ({DIM} dimensions)...")
idx_def['fields'].append({
    'name': NEW_VECTOR_FIELD,
    'type': 'Collection(Edm.Single)',
    'searchable': True,
    'filterable': False,
    'retrievable': False,
    'sortable': False,
    'facetable': False,
    'dimensions': DIM,
    'vectorSearchProfile': 'vprofile'
})

r = requests.put(base_url, headers=headers, json=idx_def)
if r.status_code not in [200, 201, 204]:
    print(f"ERROR: {r.status_code} {r.text}")
    sys.exit(1)

print(f"✓ {NEW_VECTOR_FIELD} added with {DIM} dimensions")
print("Next: Update backfill_screen_node_embeddings.py to use this field.")
