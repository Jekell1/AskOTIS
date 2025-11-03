"""Generic vector embedding progress probe for any index with has_vector flag.

If has_vector eq false returns 0 but (total - has_vector_true) > 0, we classify the remainder as
"unflagged_remaining" (likely docs lacking the flag entirely). This helps diagnose partial population cases
like new_cobol_flow_edges_v2.

Usage:
  python probe_vector_simple_progress.py --index new_cobol_program_deps
  python probe_vector_simple_progress.py --index new_cobol_flow_edges_v2 --pretty
"""
from __future__ import annotations
import os, json, argparse, sys, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def count(ep,key,index,filt=None):
    body={'search':'*','top':0,'count':True}
    if filt: body['filter']=filt
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('@odata.count',0)

def main():
    ap=argparse.ArgumentParser(description='Vector progress probe')
    ap.add_argument('--index', required=True)
    ap.add_argument('--pretty', action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    idx=args.index
    try:
        total=count(ep,key,idx)
    except Exception as e:
        print(json.dumps({'index':idx,'error':str(e)})); return
    try:
        with_v=count(ep,key,idx,'has_vector eq true')
    except Exception:
        with_v=None
    without_v=None
    if with_v is not None:
        try:
            without_v=count(ep,key,idx,'has_vector eq false')
        except Exception:
            without_v=None
    remaining=None; inferred_gap=None
    if with_v is not None and total is not None:
        remaining = total - with_v if total>=with_v else 0
        # If explicit without_v is zero but remaining>0 treat as unflagged remainder
        if without_v==0 and remaining>0:
            inferred_gap = remaining
    cov=None
    if with_v is not None and total:
        cov = round(with_v/total*100,2)
    out={'index':idx,'total_docs':total,'with_vector':with_v,'without_vector':without_v,'coverage_pct':cov,'remaining_docs':remaining}
    if inferred_gap is not None:
        out['unflagged_remaining']=inferred_gap
        out['note']='Unflagged docs likely still need embedding; has_vector field missing/absent on those docs.'
    print(json.dumps(out, indent=2 if args.pretty else None))

if __name__=='__main__':
    main()
