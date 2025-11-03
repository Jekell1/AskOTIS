import os, requests, json, random
import argparse, sys

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'

def main():
    load_local_settings()
    ep,key=resolve(os.getenv('AZURE_SEARCH_ENDPOINT'), os.getenv('AZURE_SEARCH_KEY'))
    total=fetch_total(ep,key)
    if total==0:
        print('No documents in index'); return
    samples=sample_docs(ep,key,total,limit=20)
    print(json.dumps(samples, indent=2))

def load_local_settings():
    try:
        data=json.load(open('local.settings.json','r',encoding='utf-8'))
        vals=data.get('Values',{}) or {}
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve(ep_arg=None,key_arg=None):
    ep=ep_arg or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=key_arg or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key (set env vars or local.settings.json)')
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_total(ep,key):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('[FATAL] count failed',r.status_code,r.text[:200]); sys.exit(1)
    return r.json().get('@odata.count',0)

def sample_docs(ep,key,total,limit=20):
    limit=min(limit,total)
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    picks=sorted(random.sample(range(total), limit))
    out=[]
    for skip in picks:
        body={'search':'*','top':1,'skip':skip,'select':'program_id,risk_score,node_count,edge_count,has_cycles'}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200:
            out.append({'error':r.status_code,'text':r.text[:120]}); continue
        rows=r.json().get('value',[])
        out.append(rows[0] if rows else {})
    return out

def distribution(stats):
    vals=[d.get('risk_score') for d in stats if isinstance(d,dict) and 'risk_score' in d and isinstance(d.get('risk_score'),(int,float))]
    if not vals:
        return {'count':0}
    import math
    return {
        'count':len(vals),
        'min':min(vals),
        'max':max(vals),
        'avg':round(sum(vals)/len(vals),3),
        'stdev':round(math.sqrt(sum((v-sum(vals)/len(vals))**2 for v in vals)/len(vals)),3) if len(vals)>1 else 0.0
    }

if __name__ == '__main__':
    main()
