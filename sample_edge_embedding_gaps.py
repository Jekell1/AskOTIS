"""Random gap sampler for new_cobol_flow_edges_v2 embedding coverage.

Samples N random skip offsets and inspects pages to compute fraction of edges lacking has_vector flag.
Helps estimate residual effort without scanning whole index.
"""
from __future__ import annotations
import os, json, argparse, random, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def count_total(ep,key):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('@odata.count',0)

def fetch(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'edge_id,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--samples',type=int,default=20)
    ap.add_argument('--page-size',type=int,default=256)
    ap.add_argument('--seed',type=int,default=42)
    args=ap.parse_args(); load(); ep,key=resolve(); random.seed(args.seed)
    total=count_total(ep,key)
    # Azure Search $skip limit typically 100,000; cap offsets to avoid 400 errors
    skip_limit = min(total, 100000)
    offsets=[ random.randrange(0, max(skip_limit-args.page_size,1), args.page_size) for _ in range(args.samples) ]
    sampled=0; unflagged=0
    pages=[]
    for off in offsets:
        rows=fetch(ep,key,off,args.page_size)
        nf=sum(1 for r in rows if not r.get('has_vector'))
        sampled+=len(rows); unflagged+=nf
        pages.append({'skip':off,'page_size':len(rows),'unflagged':nf})
    est_gap_ratio= (unflagged / sampled) if sampled else 0.0
    est_remaining = int(est_gap_ratio * total)
    print(json.dumps({
        'index':INDEX,
        'total_docs':total,
        'sampled_docs':sampled,
        'unflagged_in_sample':unflagged,
        'estimated_unflagged_total': est_remaining,
        'estimated_gap_pct': round(est_gap_ratio*100,2),
        'pages': pages
    }, indent=2))

if __name__=='__main__':
    main()
