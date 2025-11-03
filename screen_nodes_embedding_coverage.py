"""Report embedding coverage for new_cobol_screen_nodes (has_vector true / total)."""
import os, json, requests
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('missing endpoint/key')
    return ep.rstrip('/'), key

def count(ep,key,flt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':0,'count':True}
    if flt:
        body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise SystemExit(f"count failed {r.status_code} {r.text[:200]}")
    return r.json().get('@odata.count',0)

if __name__=='__main__':
    load(); ep,key=resolve()
    total=count(ep,key)
    vect=count(ep,key,'has_vector eq true')
    pct=(vect/total*100) if total else 0.0
    print(f"screen_nodes coverage: {vect}/{total} ({pct:.1f}%)")
