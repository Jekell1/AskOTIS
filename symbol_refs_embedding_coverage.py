"""Report embedding coverage statistics for new_cobol_symbol_refs.

Outputs:
  total_docs
  vectorized_docs
  missing_vectors
  coverage_pct
  sample_missing (first N ref_ids)
  avg_excerpt_length (overall)
  avg_excerpt_length_missing

Usage:
  python symbol_refs_embedding_coverage.py
  python symbol_refs_embedding_coverage.py --sample 20
"""
from __future__ import annotations
import os, json, argparse, requests, statistics

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def get_index_fields(ep,key):
    try:
        r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key},timeout=30)
        if r.status_code!=200:
            return set()
        data=r.json()
        return {f.get('name') for f in data.get('fields',[])}
    except Exception:
        return set()

def count_query(ep,key,flt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Count failed {r.status_code}: {r.text[:200]}")
    return r.json().get('@odata.count',0)

def sample_missing(ep,key,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','filter':'(has_vector eq false) or (has_vector eq null)','top':top,'select':'ref_id,excerpt'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return []
    return r.json().get('value',[])

def excerpt_stats(ep,key,flt=None,limit=5000):
    # lightweight average length estimation (capped)
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':min(limit,1000),'select':'ref_id,excerpt'}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return 0.0
    rows=r.json().get('value',[])
    if not rows: return 0.0
    lengths=[len((x.get('excerpt') or '')) for x in rows]
    return statistics.mean(lengths)

def main():
    ap=argparse.ArgumentParser(description='Symbol refs embedding coverage report')
    ap.add_argument('--sample',type=int,default=10)
    args=ap.parse_args(); load(); ep,key=resolve()
    fields=get_index_fields(ep,key)
    has_vec_field='has_vector' in fields
    total=count_query(ep,key)
    if has_vec_field:
        try:
            vectorized=count_query(ep,key,'has_vector eq true')
        except SystemExit:
            has_vec_field=False
            vectorized=0
    else:
        vectorized=0
    missing=total-vectorized if has_vec_field else None
    coverage=(vectorized/total*100.0) if total and has_vec_field else None
    sample=sample_missing(ep,key,args.sample) if has_vec_field and args.sample>0 else []
    avg_all=excerpt_stats(ep,key)
    avg_missing=excerpt_stats(ep,key,'(has_vector eq false) or (has_vector eq null)') if has_vec_field else None
    report={
        'index':INDEX,
        'total_docs':total,
        'vectorized_docs':vectorized if has_vec_field else 'UNKNOWN_FIELD',
        'missing_vectors':missing if has_vec_field else 'UNKNOWN_FIELD',
        'coverage_pct':round(coverage,2) if coverage is not None else 'UNKNOWN_FIELD',
        'sample_missing_ref_ids':[r.get('ref_id') for r in sample] if has_vec_field else [],
        'avg_excerpt_length_all':round(avg_all,2),
        'avg_excerpt_length_missing':round(avg_missing,2) if avg_missing is not None else 'UNKNOWN_FIELD'
    }
    js=json.dumps(report,indent=2)
    print(js)

if __name__=='__main__':
    main()
