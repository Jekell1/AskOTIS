"""Create the new_cobol_copybook_meta index.

Purpose:
  Summarize each COPYBOOK (.cpy) file with structural / usage info enabling:
    * "Where is COPYBOOK X used?" (paired with a future dependency index or on-demand analysis)
    * Semantic retrieval of copybook purpose (working-storage layouts, linkage sections, etc.)
    * Rapid scoping for data structure exploration prior to drilling into data items.

Document granularity: one document per copybook file.

Key fields:
  copybook_id (key) - stable hash of normalized path
  copybook_name - base name (upper)
  file_path - original relative path
  program_like - heuristic flag if shaped like a full program (has PROCEDURE DIVISION)  
  lines_total / lines_non_comment
  data_division_present / linkage_items / working_storage_items
  redefines_count / occurs_count / level01_count
  approximate_item_count - total parsed data items (if quick scan performed)
  has_vector - semantic summary vector presence
  ingested_at - timestamp
  summary - short textual summary used for semantic embedding

Vector field `summary_vector` (3072 dims, non-retrievable) supports semantic lookups.

Usage:
  python create_copybook_meta_index.py --force
  (Relies on local.settings.json for AZURE_SEARCH_ENDPOINT / KEY if not passed.)
"""
from __future__ import annotations
import os, sys, json, argparse, requests, hashlib

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_copybook_meta'
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
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def index_exists(ep: str, key: str) -> bool:
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200

def delete_index(ep: str, key: str):
    if index_exists(ep, key):
        r = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key})
        if r.status_code not in (200,204):
            print(f"[WARN] Delete failed {r.status_code}: {r.text[:300]}")
        else:
            print('Deleted existing index.')

def create_index(ep: str, key: str, force: bool):
    if index_exists(ep, key):
        if not force:
            print('Index already exists (use --force to recreate).'); return
        delete_index(ep, key)
    schema = {
        'name': INDEX_NAME,
        'fields': [
            {'name':'copybook_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'copybook_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'file_path','type':'Edm.String','searchable':False,'filterable':False,'facetable':False,'sortable':False,'retrievable':True},
            {'name':'program_like','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'lines_total','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'lines_non_comment','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'data_division_present','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'working_storage_items','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'linkage_items','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'redefines_count','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'occurs_count','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'level01_count','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'approximate_item_count','type':'Edm.Int32','searchable':False,'filterable':True,'facetable':True,'sortable':True,'retrievable':True},
            {'name':'summary','type':'Edm.String','searchable':True,'filterable':False,'facetable':False,'sortable':False,'retrievable':True,'analyzer':'standard.lucene'},
            {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True},
            {'name':'ingested_at','type':'Edm.DateTimeOffset','searchable':False,'filterable':True,'facetable':False,'sortable':True,'retrievable':True},
            {'name':'summary_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vector-profile','retrievable':False}
        ],
        'vectorSearch': {
            'algorithms': [{'name':'hnsw-alg','kind':'hnsw'}],
            'profiles': [{'name':'vector-profile','algorithm':'hnsw-alg'}]
        },
        'semantic': {
            'configurations': [
                {
                    'name':'semantic-default',
                    'prioritizedFields': {
                        'titleField': {'fieldName':'copybook_name'},
                        'prioritizedContentFields': [
                            {'fieldName':'summary'}
                        ]
                    }
                }
            ]
        }
    }
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:800]}")
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")

def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_copybook_meta index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--force', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_index(ep, key, args.force)

if __name__ == '__main__':
    main()
