"""Compute summary stats for new_cobol_variable_usage embedding coverage and role alignment."""
from __future__ import annotations
import os,requests,json,sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'

def load_settings():
    """Load local.settings.json Values into environment if present (mirrors other scripts)."""
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
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:200]); sys.exit(1)
    return r.json()

def main():
    load_settings()
    ep,key=resolve()
    total=search(ep,key,{'search':'*','count':True,'top':0}).get('@odata.count',0)
    missing=search(ep,key,{'search':'*','count':True,'filter':'has_vector eq false','top':0}).get('@odata.count',0)
    # Scan for role mismatch
    select='variable_id,usage_role,last_embedded_usage_role,has_vector'
    page=1000; skip=0; mismatch=0; scanned=0
    while True:
        resp=search(ep,key,{'search':'*','top':page,'skip':skip,'select':select})
        rows=resp.get('value',[])
        if not rows: break
        skip+=len(rows)
        for d in rows:
            scanned+=1
            if d.get('has_vector') and d.get('usage_role')!=d.get('last_embedded_usage_role'):
                mismatch+=1
        if len(rows)<page: break
    print(json.dumps({'total':total,'missing_vectors':missing,'role_mismatches':mismatch,'scanned':scanned},indent=2))

if __name__=='__main__':
    main()
