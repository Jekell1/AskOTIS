"""Create the new_cobol_program_deps index.

Materialized per-program dependency document enabling a single retrieval to
answer questions like:
  - What external programs does APIPAY call?
  - What dependencies does LONPF2 have?

Schema (initial):
  program_id              (key)
  calls_out_json          (JSON list of unique outbound static call targets)
  calls_in_json           (JSON list of unique inbound static callers)
  external_programs_json  (Outbound calls with no local source match)
  copybooks_used_json     (Unique copybooks referenced)
  dependency_blob         (Lexical narrative summary)
  dependency_blob_vector  (Embedding of narrative, dim=3072)
  outgoing_count          (int)
  incoming_count          (int)
  external_count          (int)
  copybook_count          (int)
  has_vector              (bool)
  updated_at              (timestamp string)

Vector profile name: vprofile

Usage:
  python search/indexes/create_program_deps_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests, datetime

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_program_deps'
VECTOR_DIM=3072

FIELDS=[
  {'name':'program_id','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True},
  {'name':'calls_out_json','type':'Edm.String','searchable':False},
  {'name':'calls_in_json','type':'Edm.String','searchable':False},
  {'name':'external_programs_json','type':'Edm.String','searchable':False},
  {'name':'copybooks_used_json','type':'Edm.String','searchable':False},
  {'name':'dependency_blob','type':'Edm.String','searchable':True},
  {'name':'dependency_blob_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile'},
  {'name':'outgoing_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'incoming_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'external_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'copybook_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
  {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True}
]

VECTOR_SEARCH={
  'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],
  'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]
}

SEMANTIC={
  'configurations':[{
    'name':'semantic-default',
    'prioritizedFields':{
      'titleField':None,
      'prioritizedContentFields':[{'fieldName':'dependency_blob'}],
      'prioritizedKeywordsFields':[]
    }
  }]
}

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
    print('[FATAL] delete failed',r.status_code,r.text[:200]); sys.exit(1)

def create(ep,key,overwrite:bool):
  if exists(ep,key):
    if not overwrite:
      print('Index exists (use --overwrite)'); return
    delete(ep,key)
  schema={
    'name':INDEX_NAME,
    'fields':FIELDS,
    'vectorSearch':VECTOR_SEARCH,
    'semantic':SEMANTIC,
    'corsOptions':{'allowedOrigins':['*']}
  }
  r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
  if r.status_code not in (200,201):
    print('[FATAL] create failed',r.status_code,r.text[:400]); sys.exit(1)
  print('Created index',INDEX_NAME)

def main():
  ap=argparse.ArgumentParser(description='Create program dependencies index')
  ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
  args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
  main()
