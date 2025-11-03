"""Create index new_cobol_program_copybook_edges capturing program->copybook distinct relationships.

Fields:
  edge_id (key) sha1(program_id|copybook_name_plain)
  program_id (filter/facet)
  copybook_name_plain (filter/facet)
  first_line (int) first observed line number
  occurrence_count (int)
  ingested_at
"""
from __future__ import annotations
import os,json,requests,hashlib,sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_copybook_edges'
SCHEMA={
  'name':INDEX,
  'fields':[
    {'name':'edge_id','type':'Edm.String','key':True,'filterable':True,'searchable':False},
    {'name':'program_id','type':'Edm.String','filterable':True,'facetable':True,'searchable':False},
    {'name':'copybook_name_plain','type':'Edm.String','filterable':True,'facetable':True,'searchable':False},
    {'name':'first_line','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'occurrence_count','type':'Edm.Int32','filterable':True,'sortable':True},
    {'name':'ingested_at','type':'Edm.DateTimeOffset','filterable':True,'sortable':True}
  ],
  'corsOptions':{'allowedOrigins':['*'],'maxAgeInSeconds':60}
}

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def delete(ep,key):
    requests.delete(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})

def create(ep,key):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=SCHEMA)
    if r.status_code not in (200,201):
        print('Create failed',r.status_code,r.text[:400]); sys.exit(1)
    print('[OK] Created',INDEX)

if __name__=='__main__':
    load(); ep,key=resolve(); delete(ep,key); create(ep,key)
