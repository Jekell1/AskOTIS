"""Report coverage stats for symbol_id population in new_cobol_data_items.

Outputs total docs, docs with symbol_id, docs missing symbol_id, and coverage percent.

Usage:
  python symbol_id_coverage_data_items.py
"""
from __future__ import annotations
import os, sys, json, requests

INDEX='new_cobol_data_items'
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY'); sys.exit(1)
    return ep.rstrip('/'), key

def count(ep,key,flt=None):
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",
                    headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Count failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('@odata.count',0)

def main():
    load_settings(); ep,key=resolve()
    total=count(ep,key)
    with_id=count(ep,key,"symbol_id ne null") if total else 0
    without=total-with_id
    pct=(with_id/total*100.0) if total else 0.0
    print(json.dumps({
        'index': INDEX,
        'total_docs': total,
        'with_symbol_id': with_id,
        'missing_symbol_id': without,
        'coverage_percent': round(pct,2)
    }, indent=2))

if __name__=='__main__':
    main()
