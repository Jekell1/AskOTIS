"""Create the new_cobol_screen_nodes index capturing concrete SCREEN SECTION artifacts.

Each document represents one extracted screen (logical UI surface) with:
  - screen_id (key)
  - program_id (owning program)
  - screen_name (derived or synthetic)
  - fields_json (array of field definitions: name, pic, usage, line, attributes)
  - actions_json (array of detected actionable elements: function keys, buttons, verbs)
  - transitions_json (array of potential next programs / paragraphs inferred heuristically)
  - raw_span_text (optional snippet of SCREEN SECTION text region)
  - metrics: field_count, action_count, transition_count
  - embedding-ready summary (deferred vector field add step)

Extraction will be performed by a companion script scanning parsed source or pre-indexed code chunks.
"""
from __future__ import annotations
import os, sys, json, argparse, requests
from typing import Any, Dict

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_screen_nodes'

BASIC_FIELDS=[
    {'name':'screen_id','type':'Edm.String','key':True,'searchable':False,'filterable':True},
    {'name':'program_id','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
    {'name':'screen_name','type':'Edm.String','searchable':True,'filterable':True},
    {'name':'field_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'action_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'transition_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'fields_json','type':'Edm.String'},
    {'name':'actions_json','type':'Edm.String'},
    {'name':'transitions_json','type':'Edm.String'},
    {'name':'display_literals_json','type':'Edm.String','searchable':True},
    {'name':'value_clauses_json','type':'Edm.String','searchable':True},
    {'name':'label_literals_json','type':'Edm.String','searchable':True},
    {'name':'raw_span_text','type':'Edm.String','searchable':True},
    {'name':'summary_text','type':'Edm.String','searchable':True},
    {'name':'generated_at','type':'Edm.String','filterable':True,'sortable':True}
]

SEMANTIC_CFG={
    'configurations':[{'name':'semantic-default','prioritizedFields':{
        'titleField': {'fieldName':'screen_name'},
        'prioritizedContentFields':[{'fieldName':'summary_text'},{'fieldName':'raw_span_text'}],
        'prioritizedKeywordsFields':[]
    }}]
}

def load_local_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def index_exists(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    return r.status_code==200

def delete(ep,key):
    if index_exists(ep,key):
        r=requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
        if r.status_code not in (200,204):
            print('[WARN] delete failed',r.status_code,r.text[:200])
        else:
            print('Deleted existing screen_nodes index')

def create(ep,key,overwrite):
    if index_exists(ep,key):
        if not overwrite:
            print('Index exists (use --overwrite)'); return
        delete(ep,key)
    schema: Dict[str,Any]={
        'name': INDEX_NAME,
        'fields': BASIC_FIELDS,
        'semantic': SEMANTIC_CFG,
        # Pre-provision vectorSearch container (vector field appended later by add_vector_field_screen_nodes.py)
        'vectorSearch': {
            'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],
            'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]
        }
    }
    r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('[FATAL] create failed',r.status_code,r.text[:400]); sys.exit(1)
    print('Created index', INDEX_NAME)

if __name__=='__main__':
    ap=argparse.ArgumentParser(description='Create screen nodes index')
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
    args=ap.parse_args(); load_local_settings(); ep,key=resolve(args); create(ep,key,args.overwrite)
