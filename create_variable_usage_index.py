"""Create the new_cobol_variable_usage index.

Purpose:
  Aggregated variable (data item / symbol) usage statistics across programs.
  Enables queries like:
    - Which programs read/write variable X most?
    - Top variables by write frequency
    - Variables only written (never read) or only read

Document model (one doc per variable name, uppercased canonical):
  variable_id (key)  -> canonical variable name (uppercased)
  read_count         -> total read xrefs
  write_count        -> total write xrefs
  param_in_count     -> parameter in occurrences
  param_out_count    -> parameter out occurrences
  total_refs         -> sum of above (except param counts may overlap with read/write, stored separately)
  program_readers    -> collection of program_ids that read it (truncated <= 1000)
  program_writers    -> collection of program_ids that write it (truncated <= 1000)
  program_params_in  -> collection of program_ids referencing as param_in (<= 500)
  program_params_out -> collection of program_ids referencing as param_out (<= 500)
  sample_refs_json   -> small JSON sample of reference snippets with program + direction
  ingested_at        -> timestamp

Later extension: add vector field for semantic variable meaning (future if needed).

Usage:
  python create_variable_usage_index.py [--overwrite]
"""
from __future__ import annotations
import os, sys, json, argparse, requests
from typing import Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_variable_usage'
VECTOR_DIM = 3072  # reserved if we later add embeddings


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
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

def create_index(ep: str, key: str, overwrite: bool):
    if index_exists(ep, key):
        if not overwrite:
            print('Index exists (use --overwrite to recreate).')
            return
        delete_index(ep, key)
    schema: Dict[str, Any] = {
        'name': INDEX_NAME,
        'fields': [
            {'name':'variable_id','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True,'sortable':False},
            {'name':'symbol_id_global','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'symbol_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'read_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'write_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'param_in_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'param_out_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'total_refs','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'program_readers','type':'Collection(Edm.String)','searchable':False,'filterable':True,'facetable':False},
            {'name':'program_writers','type':'Collection(Edm.String)','searchable':False,'filterable':True,'facetable':False},
            {'name':'program_params_in','type':'Collection(Edm.String)','searchable':False,'filterable':True,'facetable':False},
            {'name':'program_params_out','type':'Collection(Edm.String)','searchable':False,'filterable':True,'facetable':False},
            {'name':'sample_refs_json','type':'Edm.String','searchable':False,'filterable':False,'facetable':False,'sortable':False},
            # New enrichment fields
            {'name':'usage_role','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'first_write_location','type':'Edm.String','searchable':False,'filterable':False,'facetable':False,'sortable':False},
            {'name':'first_ref_program_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'first_ref_line_number','type':'Edm.Int32','searchable':False,'filterable':True,'sortable':True},
            {'name':'last_ref_program_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'last_ref_line_number','type':'Edm.Int32','searchable':False,'filterable':True,'sortable':True},
            {'name':'activity_count','type':'Edm.Int32','searchable':False,'filterable':True,'sortable':True},
            {'name':'last_embedded_usage_role','type':'Edm.String','searchable':False,'filterable':True,'sortable':False,'facetable':True},
            {'name':'has_vector','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'sortable':False},
            {'name':'usage_summary_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'var-usage-vp'},
            {'name':'ingested_at','type':'Edm.String','searchable':False,'filterable':True,'sortable':True}
        ],
        'vectorSearch':{
            'algorithms':[{'name':'var-hnsw','kind':'hnsw'}],
            'profiles':[{'name':'var-usage-vp','algorithm':'var-hnsw'}]
        },
        'semantic': {
            'configurations':[{
                'name':'semantic-default',
                'prioritizedFields':{
                    'titleField': {'fieldName':'variable_id'},
                    'prioritizedContentFields': []
                }
            }]
        }
    }
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:400]}")
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_variable_usage index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--overwrite', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_index(ep, key, args.overwrite)

if __name__ == '__main__':
    main()
