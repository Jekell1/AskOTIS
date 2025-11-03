"""Create the new_cobol_ui_paths index for canonical user screen/navigation flows.

Each document represents one enumerated navigation path from a root (menu/dispatch/UI) program
through a sequence of UI-relevant programs to a leaf.

We store sequences as JSON strings for compactness and deterministic retrieval.

Initial heuristic generation (no vectors yet). We can add embeddings later over a
concatenated textual path representation if semantic similarity queries become needed.

Usage:
  python create_ui_paths_index.py --overwrite
"""
from __future__ import annotations
import os, json, sys, argparse, requests
from typing import Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_ui_paths'


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
            {'name':'path_id','type':'Edm.String','key':True,'searchable':False,'filterable':True},
            {'name':'root_program_id','type':'Edm.String','filterable':True,'searchable':True},
            {'name':'leaf_program_id','type':'Edm.String','filterable':True,'searchable':True},
            # New explicit start/end aliases (root/leaf preserved for backward compatibility)
            {'name':'start_program_id','type':'Edm.String','filterable':True,'searchable':True},
            {'name':'end_program_id','type':'Edm.String','filterable':True,'searchable':True},
            {'name':'program_sequence_json','type':'Edm.String','searchable':True},
            {'name':'screen_sequence_json','type':'Edm.String'},
            # Unified path JSON (program + screens + metadata) for downstream summarization
            {'name':'path_json','type':'Edm.String'},
            {'name':'length','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'ui_program_count','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'branching_events_json','type':'Edm.String'},
            {'name':'guards_json','type':'Edm.String'},
            {'name':'guard_summary','type':'Edm.String','searchable':True},
            {'name':'edge_freqs_json','type':'Edm.String'},
            {'name':'avg_edge_freq','type':'Edm.Double','filterable':True,'sortable':True},
            {'name':'min_edge_freq','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'screen_ids_json','type':'Edm.String'},
            {'name':'frequency_score','type':'Edm.Double','filterable':True,'sortable':True},
            {'name':'path_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':1536,'vectorSearchProfile':'vprofile','retrievable':False},
            {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
            {'name':'loop_collapsed','type':'Edm.Boolean','filterable':True,'facetable':True},
            {'name':'score','type':'Edm.Double','filterable':True,'sortable':True},
            {'name':'generated_at','type':'Edm.String','filterable':True,'sortable':True},
            {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True},
            {'name':'notes','type':'Edm.String','searchable':True}
        ],
        'vectorSearch': {
            'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],
            'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]
        },
        'semantic': {
            'configurations':[{'name':'semantic-default','prioritizedFields':{
                'titleField': None,
                'prioritizedContentFields':[{'fieldName':'program_sequence_json'},{'fieldName':'notes'}],
                'prioritizedKeywordsFields':[]
            }}]
        }
    }
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print(f"[FATAL] Create failed {r.status_code}: {r.text[:400]}")
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_ui_paths index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--overwrite', action='store_true')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    create_index(ep, key, args.overwrite)

if __name__ == '__main__':
    main()
