"""Add origin_kind field to multiple Azure Search indexes if missing.

Supported indexes (edit INDEXES list as needed):
  - new_cobol_calls (key: call_id)
  - cobol-symbols (key: item_id)
  - cobol-xrefs (key: xref_id)
  - new_code_chunks (key: chunk_id)
  - new_cobol_flow_edges_v2 (key: edge_id)  (if exists)
  - new_cobol_flow_edges (legacy) (key: edge_id) (optional)

The script:
  1. Fetches the index schema (GET /indexes/{name})
  2. Checks if a field named origin_kind exists
  3. If absent and --apply provided, PUTs updated schema with appended field:
       {
         "name": "origin_kind",
         "type": "Edm.String",
         "filterable": true,
         "facetable": true,
         "sortable": false,
         "searchable": false
       }
  4. Prints a summary table.

NOTE: Azure AI Search permits adding new simple fields via PUT. This operation may briefly impact indexing; pass --allow-downtime to add allowIndexDowntime=true query parameter if desired.

Dry run:
  python add_origin_kind_field.py
Apply:
  python add_origin_kind_field.py --apply
With allow downtime:
  python add_origin_kind_field.py --apply --allow-downtime
"""
from __future__ import annotations
import os, json, argparse, requests

INDEXES = [
    'new_cobol_calls',
    'cobol-symbols',
    'cobol-xrefs',
    'new_code_chunks',
    'new_cobol_flow_edges_v2',
    'new_cobol_flow_edges'
]

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

FIELD_DEF = {
    'name': 'origin_kind',
    'type': 'Edm.String',
    'filterable': True,
    'facetable': True,
    'sortable': False,
    'searchable': False
}

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key environment variables.')
    return ep.rstrip('/'), key

def fetch_index(ep,key,name:str):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}",headers={'api-key':key})
    return r.status_code, (r.json() if r.status_code==200 else r.text[:300])

def update_index(ep,key,name:str,schema:dict, allow_downtime:bool):
    q=f"?api-version={API_VERSION}"
    if allow_downtime:
        q+="&allowIndexDowntime=true"
    r=requests.put(f"{ep}/indexes/{name}{q}",headers={'api-key':key,'Content-Type':'application/json'},json=schema,timeout=120)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Failed updating index {name}: {r.status_code} {r.text[:300]}")


def ensure_field(ep,key,index,apply=False, allow_downtime=False):
    code,data=fetch_index(ep,key,index)
    if code==404:
        return {'index':index,'status':'missing','action':'skip'}
    if code!=200:
        return {'index':index,'status':'error','detail':data,'action':'error'}
    schema=data
    fields=schema.get('fields',[])
    if any(f.get('name')=='origin_kind' for f in fields):
        return {'index':index,'status':'present','action':'none'}
    # Add field
    schema['fields'].append(FIELD_DEF.copy())
    if not apply:
        return {'index':index,'status':'absent','action':'would-add'}
    update_index(ep,key,index,schema,allow_downtime)
    return {'index':index,'status':'added','action':'updated'}


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--apply',action='store_true',help='Perform schema updates (default dry-run)')
    ap.add_argument('--allow-downtime',action='store_true',help='Set allowIndexDowntime=true for PUT')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    rows=[ensure_field(ep,key,i,args.apply,args.allow_downtime) for i in INDEXES]
    print(json.dumps({'results':rows},indent=2))
    if not args.apply:
        print('Dry-run complete. Use --apply to persist changes.')

if __name__=='__main__':
    main()
