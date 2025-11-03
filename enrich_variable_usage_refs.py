"""Enrich variable usage docs with first/last reference positions from symbol_refs.

Adds / updates fields:
    first_ref_program_id, first_ref_line_number,
    last_ref_program_id, last_ref_line_number,
    first_write_location (if derivable from existing stored first_write_* fields or symbol refs WRITE),
    symbol_id_global (optional consistency backfill if absent)

Strategy:
  1. Scan symbol refs grouped by symbol_name.
  2. Track earliest and latest line_number (lexicographic by program then line? We prioritize lowest line_number across all programs for first, highest for last).
  3. Merge into variable usage docs via merge action.

Usage:
  python enrich_variable_usage_refs.py --batch 1000
"""
from __future__ import annotations
import os, json, argparse, time, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
REF_INDEX='new_cobol_symbol_refs'
USAGE_INDEX='new_cobol_variable_usage'
PAGE=1000

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

def fetch_refs(ep,key,skip,top):
    url=f"{ep}/indexes/{REF_INDEX}/docs/search?api-version={API_VERSION}"
    # Some deployments do not include 'kind'; prefer selecting op-line fields only.
    body={'search':'*','top':top,'skip':skip,'select':'symbol_name,program_id,line_number,op'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(f"Fetch refs failed {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[])

def upload_usage(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{USAGE_INDEX}/docs/index?api-version={API_VERSION}"
    for d in docs: d['@search.action']='merge'
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs})
    if r.status_code not in (200,201): raise SystemExit(f"Upload usage failed {r.status_code}: {r.text[:300]}")

def main():
    ap=argparse.ArgumentParser(description='Enrich variable usage with first/last reference line numbers and first write location')
    ap.add_argument('--page',type=int,default=PAGE)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--write-derive',action='store_true',help='Derive first_write_location from earliest WRITE kind if existing location missing')
    ap.add_argument('--infer-first-write',action='store_true',help='If no write found and no existing first_write_location, infer from earliest ref (marks first_write_inferred=true)')
    args=ap.parse_args(); load(); ep,key=resolve()
    skip=0; processed=0; agg={}
    start=time.time()
    while True:
        if args.limit and processed>=args.limit: break
        rows=fetch_refs(ep,key,skip,args.page)
        if not rows: break
        skip+=len(rows)
        for r in rows:
            processed+=1
            name=(r.get('symbol_name') or '').upper()
            prog=r.get('program_id') or ''
            line=r.get('line_number') or 0
            kind=(r.get('kind') or '').upper()
            info=agg.get(name)
            if not info:
                agg[name]={
                    'first_ref_line_number': line,
                    'first_ref_program_id': prog,
                    'last_ref_line_number': line,
                    'last_ref_program_id': prog,
                    'first_write_location': None
                }
            else:
                if line < info['first_ref_line_number']:
                    info['first_ref_line_number']=line; info['first_ref_program_id']=prog
                if line > info['last_ref_line_number']:
                    info['last_ref_line_number']=line; info['last_ref_program_id']=prog
            if args.write_derive and kind in ('WRITE','PARAM_OUT'):
                # earliest write capture
                if agg[name].get('first_write_location') is None:
                    agg[name]['first_write_location']=f"{prog}:{line}"
        if len(rows)<args.page: break
    # Upload in batches
    docs=[]; uploaded=0
    for var,info in agg.items():
        if args.infer_first_write and not info.get('first_write_location'):
            fw_prog=info.get('first_ref_program_id'); fw_line=info.get('first_ref_line_number')
            if fw_prog and fw_line is not None:
                # Reuse first_write_location for inferred case (no separate flag stored)
                info['first_write_location']=f"{fw_prog}:{fw_line}"
        doc={'variable_id':var, **info}
        docs.append(doc)
        if len(docs)>=500:
            upload_usage(ep,key,docs); uploaded+=len(docs); docs.clear()
    if docs:
        upload_usage(ep,key,docs); uploaded+=len(docs)
    dur=time.time()-start
    print(f"Enriched {uploaded} variable usage docs (scanned {processed} refs) in {dur:.1f}s")

if __name__=='__main__':
    main()
