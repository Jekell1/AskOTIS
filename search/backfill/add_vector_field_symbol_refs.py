"""Ensure symbol refs index has excerpt_vector & related fields (idempotent schema patch).

Usage:
  python search/backfill/add_vector_field_symbol_refs.py

Actions:
  - Fetch index schema new_cobol_symbol_refs
  - If excerpt_vector missing or dimension mismatch vs SYMBOL_REF_VECTOR_DIM env, print guidance (requires recreate for dimension change)
  - If is_first_write not present, PATCH to add (NOTE: adding vector fields post-creation unsupported; recreate if absent)
"""
from __future__ import annotations
import os, json, requests, sys

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
VECTOR_DIM_ENV=int(os.getenv('SYMBOL_REF_VECTOR_DIM','3072'))


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass


def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def main():
    load_settings(); ep,key=resolve()
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code!=200:
        print('Index fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    schema=r.json()
    fields=schema.get('fields',[])
    name_map={f['name']:f for f in fields}
    changed=False
    # Check vector field
    if 'excerpt_vector' not in name_map:
        print('[WARN] excerpt_vector missing. You must recreate index (cannot add vector field via PATCH).')
    else:
        dim=name_map['excerpt_vector'].get('dimensions')
        if dim!=VECTOR_DIM_ENV:
            print(f"[WARN] excerpt_vector dimension {dim} != env {VECTOR_DIM_ENV}. Recreate index with desired dim.")
    if 'is_first_write' not in name_map:
        print('[INFO] Adding is_first_write boolean field via index update.')
        fields.append({'name':'is_first_write','type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True})
        # Full index update (PUT) required with same schema + added field
        put=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'name':INDEX,'fields':fields,'vectorSearch':schema.get('vectorSearch'), 'semantic':schema.get('semantic')})
        if put.status_code not in (200,201):
            print('[ERROR] Failed to update index with is_first_write', put.status_code, put.text[:300])
        else:
            print('[OK] Added is_first_write field.')
            changed=True
    else:
        print('[OK] is_first_write already present.')
    if not changed:
        print('No schema changes applied.')

if __name__=='__main__':
    main()
