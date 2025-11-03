"""Consolidated vector health report across core COBOL search indexes.

Focuses on known RAG/chatbot relevant indexes (meta, deps, edges, ui paths, screen nodes, flows, facts, usage).

Outputs JSON (default) or pretty table summarizing:
  index, total_docs, with_vector, without_vector, coverage_pct, gap_type, notes

Gap classification:
  none           -> 100% coverage
  partial        -> has_vector present; coverage between 1 and <100
  unflagged_gap  -> has_vector flag present but remaining docs have no false bucket (likely missing flag)
  missing_flag   -> vector fields exist but has_vector field missing (coverage unknown)
  zero_coverage  -> has_vector present but 0 docs flagged true while total>0

Usage:
  python consolidated_vector_health.py --pretty
  python consolidated_vector_health.py --out report.json

Environment: pulls SEARCH endpoint/key + optional AZURE_SEARCH_* from local.settings.json if not set.
"""
from __future__ import annotations
import os, json, argparse, requests, sys
from typing import List, Dict, Any

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

TARGET_INDEXES = [
    # Meta
    'new_cobol_program_meta',
    'new_cobol_copybook_meta',
    # Dependencies & relationships
    'new_cobol_program_deps',
    'new_cobol_flow_edges_v2',
    'new_cobol_ui_paths',
    'new_cobol_screen_nodes',
    'new_cobol_program_flows',
    # Facts / usage / symbols (add if deployed)
    'cobol-facts-v3',
    'cobol-copybook-usage',
]

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            os.environ.setdefault(k,str(v))
    except Exception: pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API}", headers={'api-key':key})
    if r.status_code==404:
        return None
    if r.status_code!=200:
        raise RuntimeError(f"Fetch index {name} failed {r.status_code}: {r.text[:200]}")
    return r.json()

def count_query(ep,key,index,filt=None):
    body={'search':'*','top':0,'count':True}
    if filt: body['filter']=filt
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Count failed {index} {r.status_code}: {r.text[:160]}")
    return r.json().get('@odata.count',0)

def classify(entry:Dict[str,Any]) -> str:
    if entry.get('missing'): return 'missing_index'
    if not entry.get('vector_fields'): return 'no_vectors'
    if not entry.get('has_vector_flag'): return 'missing_flag'
    cov=entry.get('coverage_pct')
    with_v=entry.get('with_vector'); total=entry.get('total_docs'); without_v=entry.get('without_vector')
    if with_v==total and total is not None: return 'none'
    if with_v==0 and total>0: return 'zero_coverage'
    if without_v==0 and with_v is not None and total and with_v<total: return 'unflagged_gap'
    return 'partial'

def analyze_index(ep,key,name) -> Dict[str,Any]:
    idx=fetch_index(ep,key,name)
    if idx is None:
        return {'index':name,'missing':True,'note':'Index not found'}
    fields=idx.get('fields',[])
    vec_fields=[f for f in fields if (f.get('type')=='Collection(Edm.Single)' and ('vectorSearchDimensions' in f or f.get('name','').endswith('_vector')))]
    has_flag=any(f.get('name')=='has_vector' and f.get('type')=='Edm.Boolean' for f in fields)
    out={'index':name,'vector_fields':[{'name':f['name'],'dimensions':f.get('vectorSearchDimensions')} for f in vec_fields], 'has_vector_flag':has_flag}
    try:
        total=count_query(ep,key,name)
    except Exception as e:
        out['error']=str(e); return out
    out['total_docs']=total
    if has_flag:
        try:
            with_v=count_query(ep,key,name,'has_vector eq true')
        except Exception:
            with_v=None
        out['with_vector']=with_v
        without_v=None
        if with_v is not None:
            try:
                without_v=count_query(ep,key,name,'has_vector eq false')
            except Exception:
                without_v=None
        out['without_vector']=without_v
        if with_v is not None and total:
            out['coverage_pct']=round(with_v/total*100,2)
        if with_v is not None and total and without_v==0 and with_v<total:
            out['note']='Unflagged remainder lacking has_vector flag.'
    gap=classify(out); out['gap_type']=gap
    if gap=='missing_flag' and out.get('vector_fields'):
        out.setdefault('note','Vector fields present but has_vector flag missing – add & backfill flag for coverage tracking.')
    if gap=='zero_coverage':
        out.setdefault('note','No documents flagged has_vector yet.')
    return out

def main():
    ap=argparse.ArgumentParser(description='Consolidated vector health report')
    ap.add_argument('--indexes', help='Comma-separated override list of indexes to check')
    ap.add_argument('--pretty', action='store_true')
    ap.add_argument('--out', help='Write JSON report to file')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search()
    names=[n.strip() for n in (args.indexes.split(',') if args.indexes else TARGET_INDEXES) if n.strip()]
    report=[analyze_index(ep,key,n) for n in names]
    if args.pretty:
        # Simple aligned table
        hdr=['index','total','with','cov%','gap','note']
        rows=[]
        for r in report:
            cov=r.get('coverage_pct'); cov_str=f"{cov:.2f}" if cov is not None else ''
            rows.append([
                r.get('index'),
                r.get('total_docs',''),
                r.get('with_vector',''),
                cov_str,
                r.get('gap_type',''),
                (r.get('note','') or '')[:90]
            ])
        widths=[max(len(str(x)) for x in col) for col in zip(hdr,*rows)]
        def fmt(line): return ' | '.join(str(v).ljust(w) for v,w in zip(line,widths))
        print(fmt(hdr)); print('-+-'.join('-'*w for w in widths))
        for row in rows: print(fmt(row))
    else:
        print(json.dumps({'indexes':report}, indent=2))
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump({'indexes':report}, f, indent=2)
            print(f"Wrote report to {args.out}")

if __name__=='__main__':
    main()
