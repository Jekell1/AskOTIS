"""Create the new_cobol_program_flows index.

Per-program flow graph summary documents.

Fields:
  program_id (key)
  condensed_mermaid (string)
  flow_summary (string)
  flow_vector (3072)
  has_vector (bool)
  node_count (int, optional)
  edge_count (int, optional)
  updated_at (string)

Usage:
  python search/indexes/create_program_flows_index.py --overwrite
"""
from __future__ import annotations
import os, sys, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME='new_cobol_program_flows'
VECTOR_DIM=3072

FIELDS=[
  {'name':'program_id','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':True},
  # Core textual + visualization artifacts
  {'name':'mermaid_flow','type':'Edm.String','searchable':True},
  {'name':'condensed_mermaid','type':'Edm.String','searchable':True},
  {'name':'flow_summary','type':'Edm.String','searchable':True},
  # Embeddings
  {'name':'flow_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile'},
  {'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True},
  # Graph metrics
  {'name':'node_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'edge_count','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'max_depth','type':'Edm.Int32','filterable':True,'sortable':True},
  {'name':'has_cycles','type':'Edm.Boolean','filterable':True,'facetable':True},
  {'name':'risk_score','type':'Edm.Double','filterable':True,'sortable':True},
  # JSON serialized collections (stored as strings)
  {'name':'flow_nodes_json','type':'Edm.String','searchable':False},
  {'name':'flow_edges_json','type':'Edm.String','searchable':False},
  {'name':'entry_nodes_json','type':'Edm.String','searchable':False},
  {'name':'exit_nodes_json','type':'Edm.String','searchable':False},
  {'name':'high_fanout_nodes_json','type':'Edm.String','searchable':False},
  {'name':'path_samples_json','type':'Edm.String','searchable':False},
  # Housekeeping
  {'name':'updated_at','type':'Edm.String','filterable':True,'sortable':True}
]

VECTOR_SEARCH={'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]}
SEMANTIC={'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':{'fieldName':'program_id'},'prioritizedContentFields':[{'fieldName':'flow_summary'}],'prioritizedKeywordsFields':[]}}]}

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
  ap=argparse.ArgumentParser(description='Create program flows index')
  ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--overwrite',action='store_true')
  args=ap.parse_args(); load_local(); ep,key=resolve(args); create(ep,key,args.overwrite)

if __name__=='__main__':
  main()
