"""Create the new_cobol_copybook_meta index (canonical copybook meta docs).

Spec Key Changes vs legacy create_copybook_meta_index.py:
  * Key is canonical copybook_name (one doc per logical copybook, not per file)
  * Includes fields: copybook_name (key), file_paths_json, summary, summary_vector (3072), include_count,
    updated_at, has_vector.

Usage:
  python search/indexes/create_copybook_meta_index.py --overwrite

Environment:
  Uses AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY or SEARCH_ENDPOINT / SEARCH_KEY.
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_copybook_meta'
VECTOR_DIM = 3072

FIELDS = [
    {'name':'copybook_name','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
    {'name':'file_paths_json','type':'Edm.String','searchable':False,'filterable':False,'facetable':False,'sortable':False,'retrievable':True},
    {'name':'summary','type':'Edm.String','searchable':True,'filterable':False,'facetable':False,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
    {'name':'include_count','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
    {'name':'updated_at','type':'Edm.String','searchable':False,'filterable':True,'facetable':False,'sortable':False,'retrievable':True},
    {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
    {'name':'summary_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile','retrievable':False},
]

VECTOR_SEARCH = {
    'algorithms': [{'name':'hnsw-alg','kind':'hnsw'}],
    'profiles': [{'name':'vprofile','algorithm':'hnsw-alg'}]
}

SEMANTIC = {
    'configurations': [
        {
            'name':'semantic-default',
            'prioritizedFields': {
                'titleField': {'fieldName':'copybook_name'},
                'prioritizedContentFields': [{'fieldName':'summary'}]
            }
        }
    ]
}

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def index_exists(ep,key):
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key':key})
    return r.status_code == 200

def delete_index(ep,key):
    if index_exists(ep,key):
        r = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key':key})
        if r.status_code not in (200,204):
            print(f"[WARN] Delete failed {r.status_code}: {r.text[:300]}")
        else:
            print('Deleted existing index.')

def create_index(ep,key,overwrite: bool):
    if index_exists(ep,key):
        if not overwrite:
            print('Index already exists (use --overwrite).'); return
        delete_index(ep,key)
    schema = {
        'name': INDEX_NAME,
        'fields': FIELDS,
        'vectorSearch': VECTOR_SEARCH,
        'semantic': SEMANTIC
    }
    r = requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:800]}"); sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create canonical copybook meta index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--overwrite', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep,key = resolve(args)
    create_index(ep,key,args.overwrite)

if __name__ == '__main__':
    main()
