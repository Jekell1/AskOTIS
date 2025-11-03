"""Report embedding coverage for new_cobol_variable_usage (has_vector true count / total)."""
from __future__ import annotations
import os, json, requests, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'

def load():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('Missing env'); sys.exit(1)
    return ep.rstrip('/'), key

def count(ep,key,flt=None):
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Count failed',r.status_code,r.text[:200]); sys.exit(1)
    return r.json().get('@odata.count',0)

if __name__=='__main__':
    load(); ep,key=resolve()
    total=count(ep,key)
    vect=count(ep,key,'has_vector eq true')
    pct=(vect/total*100) if total else 0
    print(f"Variable Usage Embedding Coverage: {vect}/{total} ({pct:.2f}%)")
