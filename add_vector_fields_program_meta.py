"""Add vector field program_summary_vector to existing new_cobol_program_meta index.

Uses Update (PUT) with new field appended; assumes index exists already.
Safe if field not present; if present it will no-op.
Includes post-update verification of actual stored dimension.
"""
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_meta'
FIELD_NAME = 'program_summary_vector'
VECTOR_DIM = int(os.getenv('PROGRAM_META_VECTOR_DIM','1536'))


def load_settings():
    try:
        vals = json.load(open('local.settings.json')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_index(ep, key):
    r = requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()


def field_exists(data):
    for f in data.get('fields',[]):
        if f['name']==FIELD_NAME:
            return True
    return False

def existing_field_dimension(data):
    for f in data.get('fields',[]):
        if f['name']==FIELD_NAME:
            return f.get('dimensions')
    return None


def ensure_vector_search(data):
    # Azure may return 'vectorSearch': None for legacy indexes; normalize
    if not isinstance(data.get('vectorSearch'), dict):
        data['vectorSearch'] = {}
    data['vectorSearch'].setdefault('algorithms', [])
    if not any(a.get('name')=='hnsw-alg' for a in data['vectorSearch']['algorithms']):
        data['vectorSearch']['algorithms'].append({'name':'hnsw-alg','kind':'hnsw'})
    data['vectorSearch'].setdefault('profiles', [])
    if not any(p.get('name')=='vprofile' for p in data['vectorSearch']['profiles']):
        data['vectorSearch']['profiles'].append({'name':'vprofile','algorithm':'hnsw-alg'})


def update_index(ep, key, data):
    ensure_vector_search(data)
    data['fields'].append({
        'name': FIELD_NAME,
        'type': 'Collection(Edm.Single)',
        'searchable': True,
        'dimensions': VECTOR_DIM,
        'vectorSearchProfile':'vprofile',
        'retrievable': False
    })
    url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key':key,'Content-Type':'application/json'}, json=data)
    if r.status_code not in (200,201,204):
        print('Update failed', r.status_code, r.text[:600]); sys.exit(1)
    print(f'Added field {FIELD_NAME} (requested dim {VECTOR_DIM}) - HTTP {r.status_code}')
    # Re-fetch to verify dimension
    refetch = fetch_index(ep,key)
    actual = existing_field_dimension(refetch)
    if actual is not None:
        if actual != VECTOR_DIM:
            print(f'WARNING: Field stored dimension {actual} != requested {VECTOR_DIM}. You may need to recreate index for dimension change.')
        else:
            print(f'Verified field dimension {actual}.')

if __name__=='__main__':
    load_settings(); ep, key = resolve(); data = fetch_index(ep,key)
    if field_exists(data):
        dim = existing_field_dimension(data)
        print(f'Field already exists; dimension={dim}. If mismatch with embedding model, recreate index.')
        sys.exit(0)
    print(f'Adding vector field with requested dimension {VECTOR_DIM}')
    update_index(ep,key,data)
