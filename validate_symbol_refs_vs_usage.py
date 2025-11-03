"""Validate alignment between occurrence-level symbol_refs and aggregate variable_usage.

Checks performed:
  1. Aggregate symbol reference docs (new_cobol_symbol_refs) by symbol_name:
       read = count(kind in {READ, PARAM_IN})
       write = count(kind in {WRITE, PARAM_OUT})
  2. Fetch corresponding variable usage doc (new_cobol_variable_usage.variable_id)
  3. Compare read_count / write_count parity.
  4. Report mismatches, top offenders, and overall divergence percentage.

Assumptions:
  * variable_id is uppercase canonical; symbol_refs.symbol_name may need .upper().
  * symbol_refs.kind uses the raw kind (READ/WRITE/PARAM_IN/PARAM_OUT).
  * It's acceptable that param_in/out are double-counted in variable_usage read/write (per existing aggregation logic).

Usage:
  python validate_symbol_refs_vs_usage.py --sample 50000
  python validate_symbol_refs_vs_usage.py --full

Outputs JSON summary: { total_symbols, checked_symbols, mismatches, mismatch_pct, samples:[...] }

Performance Notes:
  For very large ref sets, use --sample to limit symbol_refs scanned (random reservoir sampling by pages).
"""
from __future__ import annotations
import os, json, argparse, requests, random, math, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
USAGE_INDEX='new_cobol_variable_usage'
PAGE=1000

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def count_refs(ep,key):
    url=f"{ep}/indexes/{REF_INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(f"Count refs failed {r.status_code}: {r.text[:200]}")
    return r.json().get('@odata.count',0)

def fetch_ref_page(ep,key,skip,top):
    url=f"{ep}/indexes/{REF_INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'skip':skip,'select':'symbol_name,kind'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(f"Fetch refs failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def fetch_usage_batch(ep,key,ids):
    if not ids: return {}
    out={}
    # chunk filter clauses to avoid overly long queries
    for i in range(0,len(ids),100):
        subset=ids[i:i+100]
        flt=' or '.join([f"variable_id eq '{sid}'" for sid in subset])
        url=f"{ep}/indexes/{USAGE_INDEX}/docs/search?api-version={API_VERSION}"
        body={'search':'*','filter':flt,'top':len(subset),'select':'variable_id,read_count,write_count'}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200: continue
        for doc in r.json().get('value',[]):
            out[doc.get('variable_id').upper()]=doc
    return out

def aggregate_refs(ep,key,sample_limit:int|None):
    # Option 1: full scan (sample_limit None) sequential pages until exhausted
    # Option 2: sample approximate N docs: simple first N pages (adequate for large sets) OR implement reservoir sampling
    # We'll implement deterministic first N approach for simplicity.
    agg={}
    skip=0
    processed=0
    while True:
        if sample_limit and processed>=sample_limit: break
        rows=fetch_ref_page(ep,key,skip,PAGE)
        if not rows: break
        skip+=len(rows)
        for r in rows:
            if sample_limit and processed>=sample_limit: break
            processed+=1
            name=(r.get('symbol_name') or '').upper()
            if not name: continue
            kind=(r.get('kind') or '').upper()
            st=agg.setdefault(name,{'read':0,'write':0})
            if kind in ('READ','PARAM_IN'):
                st['read']+=1
            if kind in ('WRITE','PARAM_OUT'):
                st['write']+=1
        if len(rows)<PAGE: break
        if skip>=1000000: break  # safety
    return agg, processed

def main():
    ap=argparse.ArgumentParser(description='Validate symbol_refs occurrence counts vs variable_usage aggregates')
    g=ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--full',action='store_true',help='Full scan of symbol_refs index')
    g.add_argument('--sample',type=int,help='Approximate number of reference docs to scan')
    ap.add_argument('--report-mismatches',type=int,default=25,help='Include up to N mismatch examples')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    sample_limit=None if args.full else (args.sample or 50000)
    agg, scanned=aggregate_refs(ep,key,sample_limit)
    usage_docs=fetch_usage_batch(ep,key,list(agg.keys()))
    mismatches=[]
    for name,counts in agg.items():
        u=usage_docs.get(name)
        if not u:
            mismatches.append({'symbol':name,'reason':'missing_variable_usage','read_refs':counts['read'],'write_refs':counts['write']})
            continue
        # variable_usage counts treat param_in/out as read/write (already aggregated separately but not double added) -> expected parity
        vr=u.get('read_count',0)
        vw=u.get('write_count',0)
        if vr!=counts['read'] or vw!=counts['write']:
            mismatches.append({'symbol':name,'read_refs':counts['read'],'read_usage':vr,'write_refs':counts['write'],'write_usage':vw})
    mismatch_pct=(len(mismatches)/len(agg)*100.0) if agg else 0.0
    report={
        'scanned_ref_docs':scanned,
        'unique_symbols_scanned':len(agg),
        'mismatched_symbols':len(mismatches),
        'mismatch_pct':round(mismatch_pct,3),
        'sample_mismatches':mismatches[:args.report_mismatches]
    }
    print(json.dumps(report,indent=2)[:200000])

if __name__=='__main__':
    main()
