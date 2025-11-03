"""Create the new_cobol_variable_usage index.

Aggregated per-variable global usage metrics.

Fields:
  symbol_id_global (key)   # composite unique id (e.g., PROGRAM::SYMBOL)
  program_id
  symbol_name
  data_type
  first_write_location_path
  first_write_location_line
  first_write_program
  first_write_paragraph
  read_count
  write_count
  param_in_count
  param_out_count
  param_inout_count
  total_refs
  has_first_write (bool)
  updated_at

Usage:
  python search/indexes/create_variable_usage_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_variable_usage'

FIELDS=[
  {'name':'symbol_id_global','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True},
  {'name':'program_id','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
  {'name':'symbol_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True,'analyzer':'standard.lucene'},
  {'name':'data_type','type':'Edm.String','searchable':False,'filterable':True,'facetable':True},
  {'name':'first_write_location','type':'Edm.String','searchable':False,'filterable':True},
  {'name':'first_write_location_path','type':'Edm.String','searchable':False,'filterable':True},
  {'name':'first_write_location_line','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'first_write_program','type':'Edm.String','searchable':False,'filterable':True},
  {'name':'first_write_paragraph','type':'Edm.String','searchable':False,'filterable':True},
  {'name':'read_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'write_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'param_in_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'param_out_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'param_inout_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'total_refs','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'has_first_write','type':'Edm.Boolean','filterable':True,'facetable':True},
  {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True}
]

VECTOR_SEARCH=None
SEMANTIC={'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':{'fieldName':'symbol_name'},'prioritizedContentFields':[],'prioritizedKeywordsFields':[]}}]}

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
  import requests
  r=requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
  return r.status_code==200

def delete(ep,key):
  import requests
  if exists(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204):
      print('[WARN] delete failed',r.status_code,r.text[:200])
    else:
      print('Deleted existing index.')

def create(ep,key,overwrite:bool):
  import requests
  if exists(ep,key):
    if not overwrite:
      print('Index exists (use --overwrite)'); return
    delete(ep,key)
  schema={'name':INDEX_NAME,'fields':FIELDS,'semantic':SEMANTIC,'corsOptions':{'allowedOrigins':['*']}}
  r=requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
  if r.status_code not in (200,201):
    print('[FATAL] create failed',r.status_code,r.text[:400]); sys.exit(1)
  print('Created index',INDEX_NAME)

def main():
  ap=argparse.ArgumentParser(description='Create variable usage index')
  ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
  args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
  main()
