"""Append symbol_id field to new_cobol_data_items index schema if missing."""
from __future__ import annotations
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_data_items'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def main():
    load_settings(); ep,key=resolve()
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code!=200:
        print('Fetch schema failed', r.status_code, r.text[:300]); sys.exit(1)
    schema=r.json()
    names={f['name'] for f in schema.get('fields',[])}
    if 'symbol_id' in names:
        print('symbol_id already present')
        return
    schema['fields'].append({'name':'symbol_id','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'sortable':False,'retrievable':True})
    put=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if put.status_code not in (200,201):
        print('Update failed', put.status_code, put.text[:400]); sys.exit(1)
    print('symbol_id field added to schema.')

if __name__=='__main__':
    main()
