"""Add (or patch) vector field for program_sequence_json embeddings on new_cobol_ui_paths.

Hardening:
* Dimension configurable via UI_PATH_VECTOR_DIM (default 3072) aligning with text-embedding-3-large
* Initializes vectorSearch.algorithms / profiles if absent (idempotent)
* Accepts 200/201/204 as success
* Field name override via UI_PATH_VECTOR_FIELD (default sequence_vector)
"""
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_ui_paths'
FIELD = os.getenv('UI_PATH_VECTOR_FIELD','sequence_vector')
VECTOR_DIM = int(os.getenv('UI_PATH_VECTOR_DIM','3072'))
ALGO_NAME = os.getenv('UI_PATH_VECTOR_ALGO','hnsw-alg')
PROFILE_NAME = os.getenv('UI_PATH_VECTOR_PROFILE','vprofile')

def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals=json.load(f).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:  # noqa: BLE001
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_index(ep,key):
    r = requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()

def has_field(data):
    return any(f.get('name')==FIELD for f in data.get('fields',[]))

def update(ep,key,data):
    raw_vs = data.get('vectorSearch')
    if raw_vs is None or not isinstance(raw_vs, dict):
        data['vectorSearch'] = {}
    vs = data['vectorSearch']
    algos = vs.setdefault('algorithms', [])
    if not any(a.get('name')==ALGO_NAME for a in algos):
        algos.append({'name':ALGO_NAME,'kind':'hnsw'})
    profiles = vs.setdefault('profiles', [])
    if not any(p.get('name')==PROFILE_NAME for p in profiles):
        profiles.append({'name':PROFILE_NAME,'algorithm':ALGO_NAME})

    # Append new field definition
    data.setdefault('fields', []).append({
        'name': FIELD,
        'type': 'Collection(Edm.Single)',
        'searchable': True,
        'dimensions': VECTOR_DIM,
        'vectorSearchProfile': PROFILE_NAME,
        'retrievable': False
    })
    url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key':key,'Content-Type':'application/json'}, json=data)
    if r.status_code not in (200,201,204):
        print('Update failed', r.status_code, r.text[:500]); sys.exit(1)
    print(f"Added field {FIELD} (dim={VECTOR_DIM}) with profile={PROFILE_NAME}")

if __name__=='__main__':
    load_settings(); ep,key = resolve(); data = fetch_index(ep,key)
    if has_field(data):
        print(f'Field {FIELD} already exists'); sys.exit(0)
    update(ep,key,data)
