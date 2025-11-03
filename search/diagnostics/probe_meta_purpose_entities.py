"""Probe presence and summary quality for specific program or copybook IDs.

Usage:
  python search/diagnostics/probe_meta_purpose_entities.py --ids LONPF2 TIM360 ABC123

Outputs JSON with per-id:
  - type: program|copybook|missing
  - found: bool
  - summary_len
  - has_vector
  - sample_summary (truncated)
  - suggestions (when missing)

Loads endpoint/key from local.settings.json automatically.
"""
from __future__ import annotations
import os, json, argparse, requests, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROG_INDEX='new_cobol_program_meta'
COPY_INDEX='new_cobol_copybook_meta'

STOP_VARIANTS={ '0':'O', 'O':'0' }

def load_env():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_exact(ep,key,index,id_field,ids):
    out={}
    url=f"{ep}/indexes/{index}/docs/search?api-version={API}"
    chunk=15
    for i in range(0,len(ids),chunk):
        subset=ids[i:i+chunk]
        flt=' or '.join([f"{id_field} eq '{x}'" for x in subset])
        body={'search':'*','filter':flt,'top':len(subset),'select':f"{id_field},program_summary,summary,has_vector"}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
        if r.status_code!=200:
            continue
        for row in r.json().get('value',[]):
            out[row.get(id_field)] = row
    return out

# Fallback single fetch to capture summary if initial select omitted it (e.g., due to older index projection)
def fetch_single_program(ep,key,pid):
    url=f"{ep}/indexes/{PROG_INDEX}/docs/search?api-version={API}"
    body={'search':'*','filter':f"program_id eq '{pid}'",'top':1,'select':'program_id,program_summary,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=20)
    if r.status_code!=200: return None
    vals=r.json().get('value',[])
    return vals[0] if vals else None

def variants(token:str):
    v={token}
    # zero/O swaps
    swapped=''.join([STOP_VARIANTS.get(c,c) for c in token])
    v.add(swapped)
    return list(v)

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--ids', nargs='+', required=True)
    args=ap.parse_args(); load_env(); ep,key=resolve()
    ids=[x.upper() for x in args.ids]
    # Build variant set
    all_ids=set(ids)
    for t in ids:
        for v in variants(t): all_ids.add(v.upper())
    prog_rows=fetch_exact(ep,key,PROG_INDEX,'program_id',list(all_ids))
    copy_rows=fetch_exact(ep,key,COPY_INDEX,'copybook_id',list(all_ids))
    report={}
    for original in ids:
        entry={'query_id':original,'found':False,'type':'missing','summary_len':0,'has_vector':False,'sample_summary':'','variants':variants(original),'suggestions':[]}
        base_row=prog_rows.get(original)
        row=base_row
        if not row:
            # attempt single fetch directly
            row=fetch_single_program(ep,key,original)
        elif row and not (row.get('program_summary') or row.get('summary')):
            fr=fetch_single_program(ep,key,original)
            if fr:
                # merge new info
                for k,v in fr.items():
                    if v is not None: row[k]=v
        if row:
            summ=row.get('program_summary') or row.get('summary') or ''
            entry.update({'found':True,'type':'program','summary_len':len(summ),'has_vector':bool(row.get('has_vector')),'sample_summary':summ[:400]})
        else:
            # variant matches
            var_hit=None
            for v in variants(original):
                if v in prog_rows:
                    var_hit=prog_rows[v]; break
            if var_hit:
                summ=var_hit.get('program_summary') or var_hit.get('summary') or ''
                entry.update({'found':True,'type':'program-variant','summary_len':len(summ),'has_vector':bool(var_hit.get('has_vector')),'sample_summary':summ[:400],'suggestions':[f"Use variant {v}"]})
            else:
                # copybook
                crow=copy_rows.get(original)
                if crow:
                    summ=crow.get('summary') or ''
                    entry.update({'found':True,'type':'copybook','summary_len':len(summ),'has_vector':bool(crow.get('has_vector')),'sample_summary':summ[:400]})
        if not entry['found']:
            entry['suggestions'].append('Rebuild meta or verify ingestion for this ID.')
        report[original]=entry
    print(json.dumps(report,indent=2))

if __name__=='__main__':
    main()
