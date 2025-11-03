"""Create or recreate the `new_cobol_ui_paths` index (semantic UI navigation paths).

Minimal required fields (per spec):
  path_id (key)
  start_program_id, end_program_id (filterable)
  path_json (ordered JSON array of node ids program/screen ...)
  guard_summary (searchable)
  frequency_score (double)
  hop_count (int)
  updated_at (string)

We retain the richer legacy/schema fields already used elsewhere (program_sequence_json,
score, notes, path_vector, etc.) for backward compatibility. Adding `hop_count` if missing.

Usage:
  python search/indexes/create_ui_paths_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_ui_paths'

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def get_index(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code==200:
        return r.json()
    return None

def delete(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204,404):
        print('[FATAL] Delete failed',r.status_code,r.text[:300]); sys.exit(1)

BASE_FIELDS=[
    {'name':'path_id','type':'Edm.String','key':True,'searchable':False,'filterable':True},
    {'name':'start_program_id','type':'Edm.String','filterable':True,'searchable':True},
    {'name':'end_program_id','type':'Edm.String','filterable':True,'searchable':True},
    {'name':'path_json','type':'Edm.String','searchable':True},
    {'name':'guard_summary','type':'Edm.String','searchable':True},
    {'name':'frequency_score','type':'Edm.Double','filterable':True,'sortable':True},
    {'name':'frequency_score_norm','type':'Edm.Double','filterable':True,'sortable':True},
    {'name':'hop_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True},
]

# Legacy / extended fields kept (will be merged in if not duplicate)
EXTENDED_FIELDS=[
    {'name':'root_program_id','type':'Edm.String','filterable':True,'searchable':True},
    {'name':'leaf_program_id','type':'Edm.String','filterable':True,'searchable':True},
    {'name':'program_sequence_json','type':'Edm.String','searchable':True},
    {'name':'screen_sequence_json','type':'Edm.String'},
    {'name':'screen_names_json','type':'Edm.String'},
    {'name':'length','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'ui_program_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'branching_events_json','type':'Edm.String'},
    {'name':'guards_json','type':'Edm.String'},
    {'name':'edge_freqs_json','type':'Edm.String'},
    {'name':'avg_edge_freq','type':'Edm.Double','filterable':True,'sortable':True},
    {'name':'min_edge_freq','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'screen_ids_json','type':'Edm.String'},
    {'name':'path_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':1536,'vectorSearchProfile':'vprofile','retrievable':False},
    {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
    {'name':'loop_collapsed','type':'Edm.Boolean','filterable':True,'facetable':True},
    {'name':'score','type':'Edm.Double','filterable':True,'sortable':True},
    {'name':'generated_at','type':'Edm.String','filterable':True,'sortable':True},
    {'name':'notes','type':'Edm.String','searchable':True},
    {'name':'is_placeholder','type':'Edm.Boolean','filterable':True,'facetable':True},
    {'name':'path_type','type':'Edm.String','filterable':True,'searchable':True,'facetable':True},
    {'name':'generation_pass','type':'Edm.Int32','filterable':True,'sortable':True}
    ,{'name':'edge_origins_json','type':'Edm.String'}
    ,{'name':'transition_edge_count','type':'Edm.Int32','filterable':True,'sortable':True}
    ,{'name':'call_edge_count','type':'Edm.Int32','filterable':True,'sortable':True}
    ,{'name':'deepening_pass','type':'Edm.Int32','filterable':True,'sortable':True}
]

def build_fields():
    # ensure no duplicates (first occurrence kept)
    out=[]; seen=set()
    for lst in (BASE_FIELDS, EXTENDED_FIELDS):
        for f in lst:
            if f['name'] not in seen:
                out.append(f); seen.add(f['name'])
    return out

VECTOR_SEARCH={
  'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],
  'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]
}

SEMANTIC={
  'configurations':[{
    'name':'semantic-default',
    'prioritizedFields':{
      'titleField':None,
      'prioritizedContentFields':[{'fieldName':'program_sequence_json'},{'fieldName':'notes'}],
      'prioritizedKeywordsFields':[]
    }
  }]
}

def create(ep,key,overwrite:bool):
    existing=get_index(ep,key)
    if existing and not overwrite:
        print('Index exists (use --overwrite).'); return
    if existing and overwrite:
        delete(ep,key)
    schema={
        'name':INDEX_NAME,
        'fields':build_fields(),
        'vectorSearch':VECTOR_SEARCH,
        'semantic':SEMANTIC,
        'corsOptions':{'allowedOrigins':['*']}
    }
    r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('[FATAL] Create failed',r.status_code,r.text[:400]); sys.exit(1)
    print('Created index',INDEX_NAME)

def main():
    ap=argparse.ArgumentParser(description='Create/overwrite new_cobol_ui_paths index')
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
    args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
    main()
