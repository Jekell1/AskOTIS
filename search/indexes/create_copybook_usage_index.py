"""Create the new_cobol_copybook_usage index.

Documents representing a single usage site of a copybook within a program/context.

Fields:
  usage_id (key)
  copybook_name
  program_id
  paragraph_name
  line_number
  file_path
  context_text
  context_vector (3072)
  has_vector (bool)
  updated_at

Usage:
  python search/indexes/create_copybook_usage_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_copybook_usage'
VECTOR_DIM=3072

FIELDS=[
  {'name':'usage_id','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True},
  {'name':'copybook_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
  {'name':'program_id','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
  {'name':'paragraph_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
  {'name':'line_number','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'file_path','type':'Edm.String','searchable':False,'filterable':True},
  {'name':'context_text','type':'Edm.String','searchable':True},
  {'name':'context_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile'},
  {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
  {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True}
]

VECTOR_SEARCH={'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]}
SEMANTIC={'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':{'fieldName':'copybook_name'},'prioritizedContentFields':[{'fieldName':'context_text'}],'prioritizedKeywordsFields':[]}}]}

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
  if exists(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204):
      print('[WARN] delete failed',r.status_code,r.text[:200])
    else:
      print('Deleted existing index.')

def create(ep,key,overwrite:bool):
  if exists(ep,key):
    if not overwrite:
      print('Index exists (use --overwrite)'); return
    delete(ep,key)
  schema={'name':INDEX_NAME,'fields':FIELDS,'vectorSearch':VECTOR_SEARCH,'semantic':SEMANTIC,'corsOptions':{'allowedOrigins':['*']}}
  r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
  if r.status_code not in (200,201):
    print('[FATAL] create failed',r.status_code,r.text[:400]); sys.exit(1)
    
  print('Created index',INDEX_NAME)

def main():
  ap=argparse.ArgumentParser(description='Create copybook usage index')
  ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
  args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
  main()
