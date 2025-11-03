import os, json, sys, requests
API='2024-07-01'
INDEX='cobol-copybooks'
VECTOR_FIELD='copybook_vector'
FLAG_FIELD='has_vector'
PROFILE='vec-default'

# This script adds a vector field + boolean flag to the existing copybooks index if absent.
# If vectorSearch profile missing, inject minimal config.

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception:
        vals={}
    for k,v in vals.items():
        os.environ.setdefault(k,v)
    ep=os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key=os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY'); sys.exit(2)
    return ep.rstrip('/'), key

def get_index(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key})
    if r.status_code!=200:
        print('Failed to fetch index:', r.status_code, r.text[:200]); sys.exit(2)
    return r.json()

def update_index(ep,key,definition):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=definition)
    if r.status_code not in (200,201,204):
        print('Update failed:', r.status_code, r.text[:300]); sys.exit(2)
    print('Index updated successfully (status', r.status_code, ').')

def ensure_vector_search(defn):
    vs=defn.get('vectorSearch')
    if not vs:
        defn['vectorSearch']={
            'algorithms':[{'name':'hnsw','kind':'hnsw'}],
            'profiles':[{'name':PROFILE,'algorithm':'hnsw'}]
        }
        print('Added vectorSearch configuration.')
        return True
    # ensure profile present
    profs={p.get('name') for p in vs.get('profiles',[])}
    if PROFILE not in profs:
        vs.setdefault('profiles',[]).append({'name':PROFILE,'algorithm':'hnsw'})
        print('Added missing vectorSearch profile', PROFILE)
        return True
    return False

def main():
    ep,key=load_settings()
    idx=get_index(ep,key)
    fields=idx.get('fields',[])
    names={f['name'] for f in fields}
    changed=False
    if VECTOR_FIELD not in names:
        fields.append({
            'name': VECTOR_FIELD,
            'type': 'Collection(Edm.Single)',
            'searchable': True,
            'filterable': False,
            'facetable': False,
            'sortable': False,
            'dimensions': 1536,
            'vectorSearchProfile': PROFILE
        })
        changed=True
        print('Added vector field', VECTOR_FIELD)
    if FLAG_FIELD not in names:
        fields.append({
            'name': FLAG_FIELD,
            'type': 'Edm.Boolean',
            'filterable': True,
            'facetable': True
        })
        changed=True
        print('Added flag field', FLAG_FIELD)
    vs_changed=ensure_vector_search(idx)
    if not changed and not vs_changed:
        print('No changes needed; fields and vectorSearch already present.')
        return
    idx['fields']=fields
    update_index(ep,key,idx)

if __name__=='__main__':
    main()
