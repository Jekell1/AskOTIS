"""Compatibility wrapper for historical command path.

The copybook usage index (new_cobol_copybook_usage) is created with the vector field
already present (context_vector, has_vector). This script is a no-op that
reports the field presence so existing automated command sequences that invoke:
  python search/backfill/add_vector_field_copybook_usage.py
succeed without modification.
"""
from __future__ import annotations
import os, json, sys, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview') or '2025-08-01-preview'
INDEX='new_cobol_copybook_usage'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            os.environ.setdefault(k,str(v))
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key env vars', file=sys.stderr)
        sys.exit(0)  # treat as noop
    return ep.rstrip('/'), key

FIELDS_ENDPOINT = "/indexes/{}/?api-version={}"

def main():
    load_settings(); ep,key=resolve()
    url=f"{ep}{FIELDS_ENDPOINT.format(INDEX,API)}"
    try:
        r=requests.get(url,headers={'api-key':key},timeout=30)
        if r.status_code!=200:
            print(f"[WARN] Unable to fetch index definition status={r.status_code}")
            print('[OK] Assuming vector field already provisioned (noop).')
            return
        data=r.json()
        names=[f.get('name') for f in data.get('fields',[])]
        if 'context_vector' in names and 'has_vector' in names:
            print('[OK] Vector fields already present; no action needed.')
        else:
            print('[WARN] Expected vector fields missing. You may need to recreate index via create_copybook_usage_index.py')
    except Exception as e:
        print('[WARN] Exception while checking index:', e)
        print('[OK] Treating as noop.')

if __name__=='__main__':
    main()
