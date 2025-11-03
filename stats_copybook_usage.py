"""Stats for new_cobol_copybook_usage embedding coverage and distinct counts.

Facet-based distinct counts in Azure Search are capped (currently 1000). If you
need the full distinct cardinalities (e.g., you actually have ~10k programs),
run with --exact to enumerate all documents and accumulate sets client-side.
Enumeration is O(N) over documents and will make ~total/\n(top) service calls.
"""
from __future__ import annotations
import os,json,requests,sys,time,argparse
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_usage'

def load():
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
    if not ep or not key: print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: print('Search failed',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def enumerate_distinct(ep,key,total,page_size):
    programs=set(); copybooks=set()
    skip=0; fetched=0; t0=time.time();
    while True:
        body={'search':'*','top':page_size,'skip':skip,'select':'program_id,copybook_name_plain'}
        resp=search(ep,key,body)
        rows=resp.get('value',[])
        if not rows:
            break
        for r in rows:
            p=r.get('program_id'); c=r.get('copybook_name_plain')
            if p: programs.add(p)
            if c: copybooks.add(c)
        fetched+=len(rows); skip+=len(rows)
        if fetched % (page_size*5)==0:
            rate=fetched/max(0.001,(time.time()-t0))
            print(f"[PROGRESS] enumerated={fetched}/{total} rate={rate:.1f}/s prog={len(programs)} copy={len(copybooks)}",file=sys.stderr)
        if len(rows) < page_size:
            break
    return programs, copybooks

def main():
    ap=argparse.ArgumentParser(description='Coverage and distinct stats for new_cobol_copybook_usage')
    ap.add_argument('--exact',action='store_true',help='Enumerate all docs to compute true distinct counts (slower)')
    ap.add_argument('--page-size',type=int,default=2000,help='Page size for exact enumeration')
    args=ap.parse_args()
    load(); ep,key=resolve()
    total=search(ep,key,{'search':'*','count':True,'top':0}).get('@odata.count',0)
    missing=search(ep,key,{'search':'*','count':True,'filter':'has_vector eq false','top':0}).get('@odata.count',0)
    facets=search(ep,key,{'search':'*','facets':['program_id,count:1000','copybook_name_plain,count:1000'],'top':0}).get('@search.facets',{})
    est_programs=len(facets.get('program_id',[]))
    est_copybooks=len(facets.get('copybook_name_plain',[]))
    exact_programs=exact_copybooks=None
    if args.exact:
        programs, copybooks=enumerate_distinct(ep,key,total,args.page_size)
        exact_programs=len(programs); exact_copybooks=len(copybooks)
    covered=total-missing
    pct= (covered/total*100.0) if total else 0.0
    out={
        'total':total,
        'with_vectors':covered,
        'missing_vectors':missing,
        'vector_coverage_pct':round(pct,2),
        'distinct_programs_estimate':est_programs,
        'distinct_copybooks_estimate':est_copybooks
    }
    if exact_programs is not None:
        out['distinct_programs_exact']=exact_programs
        out['distinct_copybooks_exact']=exact_copybooks
        out['estimate_truncated']= est_programs < exact_programs
    print(json.dumps(out,indent=2))

if __name__=='__main__':
    main()
