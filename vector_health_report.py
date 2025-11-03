"""Produce a consolidated vector coverage health report across specified indexes.

Combines the logic of the simple probe with optional threshold evaluation.

Usage:
  python vector_health_report.py --indexes new_cobol_program_deps new_cobol_flow_edges_v2 --pretty
  python vector_health_report.py --all-defaults --pretty

Exit code is 0 unless --assert and a threshold fails.
"""
from __future__ import annotations
import os, json, argparse, sys, requests, time

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
DEFAULT_INDEXES=[
    'new_cobol_program_deps',
    'new_cobol_flow_edges_v2',
    'new_cobol_ui_paths'
]

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
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

def probe_index(ep,key,index):
    try:
        total=count(ep,key,index)
    except Exception as e:
        return {'index':index,'error':str(e)}
    with_v=None; without_v=None; remaining=None; unflagged=None; cov=None
    try:
        with_v=count(ep,key,index,'has_vector eq true')
    except Exception:
        with_v=None
    if with_v is not None:
        try:
            without_v=count(ep,key,index,'has_vector eq false')
        except Exception:
            without_v=None
    if with_v is not None and total is not None:
        remaining=total-with_v if total>=with_v else 0
        if without_v==0 and remaining>0:
            unflagged=remaining
        if total:
            cov=round(with_v/total*100,2)
    out={'index':index,'total_docs':total,'with_vector':with_v,'without_vector':without_v,'coverage_pct':cov,'remaining_docs':remaining}
    if unflagged is not None:
        out['unflagged_remaining']=unflagged
        out['note']='Unflagged docs (no has_vector field) still need embedding.'
    return out

def main():
    ap=argparse.ArgumentParser(description='Vector coverage health report')
    ap.add_argument('--indexes', nargs='+', help='Explicit index list')
    ap.add_argument('--all-defaults', action='store_true')
    ap.add_argument('--pretty', action='store_true')
    ap.add_argument('--require', nargs='*', help='Coverage requirements: index>=pct (e.g. new_cobol_program_deps>=99)')
    args=ap.parse_args(); load(); ep,key=resolve()
    indexes=args.indexes or (DEFAULT_INDEXES if args.all_defaults else [])
    if not indexes:
        print('No indexes specified (use --indexes or --all-defaults)', file=sys.stderr)
        sys.exit(1)
    report=[probe_index(ep,key,i) for i in indexes]
    failures=[]
    if args.require:
        rules={}
        for token in args.require:
            if '>=' not in token:
                print(f'Invalid requirement token {token}; expected index>=pct', file=sys.stderr); sys.exit(1)
            idx, thresh = token.split('>=',1)
            try: rules[idx]=float(thresh)
            except ValueError:
                print(f'Invalid threshold {thresh}', file=sys.stderr); sys.exit(1)
        for rec in report:
            if rec.get('coverage_pct') is not None:
                need=rules.get(rec['index'])
                if need is not None and rec['coverage_pct'] < need:
                    failures.append({'index':rec['index'],'coverage_pct':rec['coverage_pct'],'required':need})
    out={'generated_utc':time.time(),'indexes':report}
    if failures:
        out['requirement_failures']=failures
    print(json.dumps(out, indent=2 if args.pretty else None))
    if failures:
        sys.exit(2)

if __name__=='__main__':
    main()
