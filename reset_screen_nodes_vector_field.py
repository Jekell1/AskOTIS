"""
Removes summary_vector + has_vector from new_cobol_screen_nodes, then re-adds with 3072 dims.
Safe because no embeddings have been generated yet.
"""
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
INDEX = 'new_cobol_screen_nodes'
VECTOR_FIELD = 'summary_vector'
HAS_FIELD = 'has_vector'
NEW_DIM = 3072  # Match other indexes

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

# 1. Get current index definition
print(f"Fetching index definition for {INDEX}...")
r = requests.get(base_url, headers=headers)
if r.status_code != 200:
    print(f"ERROR: Failed to fetch index: {r.status_code} {r.text}")
    sys.exit(1)

idx_def = r.json()
original_field_count = len(idx_def['fields'])

# 2. Remove the vector fields
print(f"Removing {VECTOR_FIELD} and {HAS_FIELD} fields...")
idx_def['fields'] = [f for f in idx_def['fields'] if f['name'] not in [VECTOR_FIELD, HAS_FIELD]]
after_removal_count = len(idx_def['fields'])
print(f"  Fields: {original_field_count} → {after_removal_count}")

# 3. Update index (remove fields)
r = requests.put(base_url, headers=headers, json=idx_def)
if r.status_code not in [200, 201, 204]:
    print(f"ERROR: Failed to update index (removal): {r.status_code} {r.text}")
    sys.exit(1)
print("  ✓ Fields removed")

# 4. Re-fetch to ensure clean state
print("Fetching updated index definition...")
r = requests.get(base_url, headers=headers)
if r.status_code != 200:
    print(f"ERROR: Failed to re-fetch index: {r.status_code}")
    sys.exit(1)
idx_def = r.json()

# 5. Add fields back with 3072 dimensions
print(f"Adding {VECTOR_FIELD} (3072 dims) and {HAS_FIELD}...")

# Add has_vector field
idx_def['fields'].append({
    'name': HAS_FIELD,
    'type': 'Edm.Boolean',
    'searchable': False,
    'filterable': True,
    'retrievable': True,
    'sortable': False,
    'facetable': False
})

# Add summary_vector field with 3072 dimensions
idx_def['fields'].append({
    'name': VECTOR_FIELD,
    'type': 'Collection(Edm.Single)',
    'searchable': True,
    'filterable': False,
    'retrievable': False,
    'sortable': False,
    'facetable': False,
    'dimensions': NEW_DIM,
    'vectorSearchProfile': 'vprofile'
})

# 6. Update index (add fields)
r = requests.put(base_url, headers=headers, json=idx_def)
if r.status_code not in [200, 201, 204]:
    print(f"ERROR: Failed to update index (addition): {r.status_code} {r.text}")
    sys.exit(1)

final_field_count = len(idx_def['fields'])
print(f"  ✓ Fields added")
print(f"  Final field count: {final_field_count}")
print(f"\nDone! {VECTOR_FIELD} now has {NEW_DIM} dimensions (matching other indexes).")
print("Run backfill_screen_node_embeddings.py next.")
