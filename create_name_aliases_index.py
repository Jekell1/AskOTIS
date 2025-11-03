"""Create the new_cobol_name_aliases index for cross-entity name normalization.

Doc model (one per alias variant):
  alias_id (key)
  canonical_name (filterable, facetable)
  alias (searchable)
  variant_type (classification of how alias generated)
  kind (PROGRAM|COPYBOOK|PARAGRAPH|UNKNOWN)
  source_hint (optional short provenance e.g. filename or 'COPY stmt')
  ingested_at

Search use cases:
  * Expand query tokens through alias variants to improve recall across code / copybooks / paragraph labels.
"""
from __future__ import annotations
import os, json, argparse, requests, sys, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_name_aliases'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def exists(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    return r.status_code==200

def delete(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code not in (200,204,404):
        print('Delete failed', r.status_code, r.text[:200]); sys.exit(1)

def create(ep,key,force: bool):
    if exists(ep,key):
        if not force:
            print('Index exists (use --force)'); return
        delete(ep,key)
    schema={
        'name': INDEX,
        'fields': [
            {'name':'alias_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'retrievable':True},
            {'name':'canonical_name','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'alias','type':'Edm.String','searchable':True,'filterable':False,'retrievable':True},
            {'name':'variant_type','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'kind','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True},
            {'name':'source_hint','type':'Edm.String','searchable':False,'filterable':False,'retrievable':True},
            {'name':'ingested_at','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True}
        ],
        'corsOptions': {'allowedOrigins':['*']},
        'semantic': {'configurations':[{'name':'semantic-default','prioritizedFields':{'titleField':None,'prioritizedContentFields':[{'fieldName':'alias'}]}}]}
    }
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('Create failed', r.status_code, r.text[:300]); sys.exit(1)
    print('Created index', INDEX)

def main():
    ap=argparse.ArgumentParser(description='Create name aliases index')
    ap.add_argument('--endpoint'); ap.add_argument('--key'); ap.add_argument('--force',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve(args); create(ep,key,args.force)

if __name__=='__main__':
    main()
