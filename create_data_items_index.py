"""Create the new_cobol_data_items index for COBOL data division items.

Schema rationale:
  - Each document is a data item (group or elementary) with hierarchical path.
  - Vector field allows semantic lookup of field meaning or structure references.

Usage:
  python create_data_items_index.py --endpoint https://<search>.search.windows.net --key <key>
Or rely on local.settings.json Values for AZURE_SEARCH_ENDPOINT / KEY.
"""
from __future__ import annotations
import os, json, argparse, sys, requests, hashlib, time

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
INDEX_NAME = 'new_cobol_data_items'
VECTOR_DIM = 3072


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint or key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def index_exists(ep: str, key: str) -> bool:
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200


def delete_index(ep: str, key: str):
    if index_exists(ep, key):
        r = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
        if r.status_code not in (200,204):
            print(f"[WARN] Delete failed {r.status_code}: {r.text[:200]}")
        else:
            print('Deleted existing index.')


def create_index(ep: str, key: str, force: bool):
    if index_exists(ep, key):
        if not force:
            print('Index already exists (use --force to recreate).')
            return
        delete_index(ep, key)
    schema = {
        'name': INDEX_NAME,
        'fields': [
            {'name':'item_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'program_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'file_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'file_path','type':'Edm.String','searchable':False,'filterable':False,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'level','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'item_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'pic','type':'Edm.String','searchable':True,'filterable':False,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'occurs','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'redefines','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'usage','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'full_clause','type':'Edm.String','searchable':True,'filterable':False,'facetable':False,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'value_clause','type':'Edm.String','searchable':True,'filterable':False,'facetable':False,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'parent_item','type':'Edm.String','searchable':False,'filterable':True,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'path','type':'Edm.String','searchable':True,'filterable':True,'facetable':False,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'line_start','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':False,'sortable':True,'retrievable':True},
            {'name':'line_end','type':'Edm.Int32','searchable':False,'filterable':False,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'length_bytes','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'is_group','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'ingested_at','type':'Edm.DateTimeOffset','searchable':False,'filterable':True,'facetable':False,'sortable':True,'retrievable':True},
            # Vector field: use vectorSearchProfile referencing profile defined below
            {'name':'vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vector-profile','retrievable':False}
        ],
        'vectorSearch': {
            'algorithms': [ {'name':'hnsw-alg','kind':'hnsw'} ],
            'profiles': [ {'name':'vector-profile','algorithm':'hnsw-alg'} ]
        },
        'semantic': {
            'configurations': [
                {
                    'name':'semantic-default',
                    'prioritizedFields': {
                        'titleField': {'fieldName':'item_name'},
                        'prioritizedContentFields': [
                            {'fieldName':'full_clause'},
                            {'fieldName':'path'}
                        ]
                    }
                }
            ]
        }
    }
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:500]}")
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_data_items index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--force', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_index(ep, key, args.force)

if __name__ == '__main__':
    main()
