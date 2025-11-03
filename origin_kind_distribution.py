"""Compute origin_kind distribution for supported indexes.

Prereq: indexes contain an 'origin_kind' field (filterable/facetable) added via add_origin_kind_field.py.

Usage:
  python origin_kind_distribution.py --indexes new_cobol_calls,cobol-symbols --sample 0
    (sample=0 means full facet; sample>0 restricts facet to top N values)

Output: JSON with per-index counts and percentages.
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
DEFAULT_INDEXES='new_cobol_calls,cobol-symbols,cobol-xrefs,new_code_chunks,new_cobol_flow_edges_v2'

FACET_FIELD='origin_kind'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key env vars')
    return ep.rstrip('/'), key

def facet(ep,key,index, sample_limit:int):
    # facet query to get counts
    params={'search':'*','count': True, 'top': 0, 'facets':[f"{FACET_FIELD}{':' + str(sample_limit) if sample_limit>0 else ''}"]}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=params,timeout=60)
    if r.status_code!=200:
        return {'index':index,'error':r.text[:300]}
    data=r.json()
    total=data.get('@odata.count')
    dist={}
    for facet in data.get('facets',{}).get(FACET_FIELD,[]):
        dist[facet['value']]=facet['count']
    # compute percentages
    pct={k: (v/total*100 if total else 0) for k,v in dist.items()}
    return {'index':index,'total':total,'counts':dist,'percent':{k:round(p,2) for k,p in pct.items()}}

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--indexes',default=DEFAULT_INDEXES,help='Comma-separated list of indexes to report')
    ap.add_argument('--sample',type=int,default=0,help='Facet sample top N (0 = all)')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    out=[facet(ep,key,i.strip(),args.sample) for i in args.indexes.split(',') if i.strip()]
    print(json.dumps({'results':out},indent=2))

if __name__=='__main__':
    main()
