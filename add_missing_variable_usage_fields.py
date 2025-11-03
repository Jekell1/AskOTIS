"""Add missing first/last ref fields to existing new_cobol_variable_usage index.

Azure Search allows adding new fields (cannot remove). This script PATCHes the schema with new fields:
  first_ref_program_id, first_ref_line_number, last_ref_program_id, last_ref_line_number

Usage:
  python add_missing_variable_usage_fields.py
"""
from __future__ import annotations
import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'

FIELDS_TO_ADD=[
    {'name':'first_ref_program_id','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True,'facetable':True,'sortable':False},
    {'name':'first_ref_line_number','type':'Edm.Int32','searchable':False,'filterable':True,'retrievable':True,'facetable':True,'sortable':True},
    {'name':'last_ref_program_id','type':'Edm.String','searchable':False,'filterable':True,'retrievable':True,'facetable':True,'sortable':False},
    {'name':'last_ref_line_number','type':'Edm.Int32','searchable':False,'filterable':True,'retrievable':True,'facetable':True,'sortable':True},
]

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,str(v))
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def current_fields(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
    if r.status_code!=200: raise SystemExit(f"Fetch index failed {r.status_code}: {r.text[:200]}")
    data=r.json(); existing={f['name'] for f in data.get('fields',[])}
    return existing

def patch(ep,key,to_add):
    if not to_add:
        print('No missing fields to add.'); return
    url=f"{ep}/indexes/{INDEX}?api-version={API}"
    # PATCH with merged fields; need full index payload? We fetch existing then append.
    base=requests.get(url,headers={'api-key':key}).json()
    base_fields=base.get('fields',[])
    base_fields.extend(to_add)
    base['fields']=base_fields
    r=requests.put(url,headers={'api-key':key,'Content-Type':'application/json'},json=base)
    if r.status_code not in (200,201,204):
        print('Update failed',r.status_code,r.text[:400]); sys.exit(1)
    print('Added fields:',[f['name'] for f in to_add])

def main():
    load(); ep,key=resolve(); existing=current_fields(ep,key)
    to_add=[f for f in FIELDS_TO_ADD if f['name'] not in existing]
    patch(ep,key,to_add)

if __name__=='__main__':
    main()
