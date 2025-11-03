"""Create the `new_cobol_screen_nodes` index (screen inventory / SCREEN SECTION extraction).

Schema (spec):
  screen_id (key)
  program_id (filterable, facetable)
  screen_name (filterable)
  fields_json (names/types/lengths)
  actions_json (user actions / keys)
  transitions_json (next program/screen candidates)
  raw_span_text (searchable)
  summary_text (searchable)
  summary_vector (Collection(Edm.Single), dims=3072, vectorSearchProfile=vprofile)
  field_count, action_count, transition_count (ints)
  generated_at (string)
  has_vector (bool)

Usage:
  python search/indexes/create_screen_nodes_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_screen_nodes'
VECTOR_DIM=3072

FIELDS=[
  {'name':'screen_id','type':'Edm.String','key':True,'searchable':False,'filterable':True},
    {'name':'original_screen_id','type':'Edm.String','filterable':True,'searchable':False},
  {'name':'program_id','type':'Edm.String','filterable':True,'facetable':True,'searchable':True},
  {'name':'screen_name','type':'Edm.String','filterable':True,'searchable':True},
  {'name':'fields_json','type':'Edm.String'},
  {'name':'actions_json','type':'Edm.String'},
  {'name':'transitions_json','type':'Edm.String'},
  {'name':'raw_span_text','type':'Edm.String','searchable':True},
  {'name':'summary_text','type':'Edm.String','searchable':True},
  {'name':'summary_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile'},
  {'name':'field_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'action_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'transition_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'generated_at','type':'Edm.String','filterable':True,'sortable':True},
  {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True}
]

VECTOR_SEARCH={'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}], 'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]}
SEMANTIC={'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[{'fieldName':'summary_text'}],'prioritizedKeywordsFields':[]}}]}

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def exists(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    return r.status_code==200

def delete(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204,404):
        print('[FATAL] Delete failed',r.status_code,r.text[:200]); sys.exit(1)

def create(ep,key,overwrite:bool):
    if exists(ep,key):
        if not overwrite:
            print('Index exists (use --overwrite)'); return
        delete(ep,key)
    schema={'name':INDEX_NAME,'fields':FIELDS,'vectorSearch':VECTOR_SEARCH,'semantic':SEMANTIC,'corsOptions':{'allowedOrigins':['*']}}
    r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('[FATAL] Create failed',r.status_code,r.text[:300]); sys.exit(1)
    print('Created index',INDEX_NAME)

def main():
    ap=argparse.ArgumentParser(description='Create screen nodes index')
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
    args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
    main()
