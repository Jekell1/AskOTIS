"""CI assertion for vector coverage across key indexes.

Checks that each target index either:
  * Achieves >= REQUIRED_PCT coverage (has_vector true / total) OR
  * Is explicitly allowed to be below threshold via allowlist (ALLOWED_PARTIAL)

Also validates that any docs with has_vector true have a 'vector_quality' field unless index listed in NO_QUALITY_REQUIRED.

Exit codes:
  0 success
  1 failure (coverage gap or missing vector_quality)

Usage:
  python assert_vector_coverage.py --min 95
  python assert_vector_coverage.py --indexes new_cobol_program_meta,new_cobol_copybook_meta

Intended for pipeline integration.
"""
from __future__ import annotations
import os, json, argparse, sys, requests
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
DEFAULT_INDEXES=[
    'new_cobol_program_meta',
    'new_cobol_copybook_meta',
    'new_cobol_program_deps',
    'new_cobol_flow_edges_v2',
    'new_cobol_ui_paths',
    'new_cobol_screen_nodes',
    'new_cobol_program_flows',
    'new_cobol_copybook_usage',
    'cobol-facts-v3'
]

ALLOWED_PARTIAL=set([])  # All indexes now expected to meet thresholds; update maintainers if new temporary exceptions arise
NO_QUALITY_REQUIRED=set(['cobol-facts-v3'])       # facts may have synthetic vectors w/o tagging

PAGE_SIZE=512

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,str(v))
    except Exception: pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def count(ep,key,index,filt=None):
    body={'search':'*','top':0,'count':True}
    if filt: body['filter']=filt
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:200])
    return r.json().get('@odata.count',0)

def sample_quality(ep,key,index,limit=2000):
    # page through has_vector true docs ensuring vector_quality present. Use minimal select to maximize compatibility.
    checked=0; missing=0; skip=0
    while checked<limit:
        body={'search':'*','top':min(PAGE_SIZE,limit-checked),'skip':skip,'select':'has_vector,vector_quality','filter':'has_vector eq true'}
        r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200:
            break
        rows=r.json().get('value',[])
        if not rows:
            break
        for row in rows:
            checked+=1
            if row.get('has_vector') is True and 'vector_quality' not in row:
                missing+=1
            if checked>=limit:
                break
        if len(rows)<body['top']:
            break
        skip+=body['top']
    return {'checked':checked,'missing_quality':missing}

def main():
    ap=argparse.ArgumentParser(description='CI vector coverage assertion')
    ap.add_argument('--indexes',help='Comma separated list override')
    ap.add_argument('--min',type=float,default=99.0,help='Minimum required coverage percent')
    ap.add_argument('--enforce-all',action='store_true',help='Ignore ALLOWED_PARTIAL list (treat all indexes as required)')
    ap.add_argument('--fail-on-missing-quality',action='store_true',help='Fail if vector_quality absent (except allow list)')
    ap.add_argument('--quality-sample',type=int,default=2000,help='Docs to sample for vector_quality presence per index')
    ap.add_argument('--json',action='store_true',help='Emit JSON summary')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search()
    names=[n.strip() for n in (args.indexes.split(',') if args.indexes else DEFAULT_INDEXES) if n.strip()]
    overall=[]; failures=[]
    for name in names:
        try:
            total=count(ep,key,name)
            with_v=count(ep,key,name,'has_vector eq true')
        except Exception as e:
            failures.append({'index':name,'reason':f'count_error:{e}'})
            continue
        cov=0.0
        if total>0 and with_v is not None:
            cov=(with_v/total)*100.0
        rec={'index':name,'total':total,'with_vector':with_v,'coverage_pct':round(cov,2)}
        # coverage assertion
        allowed_partial = (name in ALLOWED_PARTIAL) and not args.enforce_all
        if not allowed_partial and cov < args.min:
            rec['coverage_fail']=True
            failures.append({'index':name,'reason':f'coverage {cov:.2f}% < required {args.min}%'})
        # quality sampling
        if args.fail_on_missing_quality and name not in NO_QUALITY_REQUIRED:
            q=sample_quality(ep,key,name,args.quality_sample)
            rec['quality_sample']=q
            if q['missing_quality']>0:
                rec['quality_fail']=True
                failures.append({'index':name,'reason':f"{q['missing_quality']} docs missing vector_quality"})
        overall.append(rec)
    ok=not failures
    if args.json:
        print(json.dumps({'results':overall,'failures':failures,'ok':ok},indent=2))
    else:
        for r in overall:
            print(f"{r['index']}: cov={r['coverage_pct']}% ({r['with_vector']}/{r['total']})")
        if failures:
            print('Failures:')
            for f in failures: print(' -',f)
    if not ok:
        sys.exit(1)

if __name__=='__main__':
    main()
