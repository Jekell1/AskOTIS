#!/usr/bin/env python3
"""Recompute has_vector flags to match actual populated primary vector fields.

This script enforces the semantics documented in coverage_semantics.md.
It scans documents in an index, determines whether the designated primary
vector field is present and non-empty (expected length > 0), and updates
`has_vector` flag accordingly (mergeOrUpload only for mismatches).

Usage examples:
  python recompute_has_vector.py --index new-cobol-files --primary-key id --vector-field contentVector --expected-dims 3072
  python recompute_has_vector.py --index new_cobol_calls --primary-key call_id --vector-field snippet_vector --expected-dims 1536 --batch 400

Supports sampling mode for a quick drift estimate:
  python recompute_has_vector.py --index new_cobol_calls --primary-key call_id --vector-field snippet_vector --expected-dims 1536 --sample 2000

Environment credentials: SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_* variants or local.settings.json Values section)
"""
from __future__ import annotations
import os, sys, json, argparse, time, math, random, requests
from typing import Dict, Any, List, Optional

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')

# -------------- Credential Helpers --------------

def load_local_settings_values() -> Dict[str,str]:
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json','r',encoding='utf-8') as f:
                return json.load(f).get('Values',{}) or {}
        except Exception:
            return {}
    return {}

def resolve_creds():
    vals=load_local_settings_values()
    ep=os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key env vars', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key

# -------------- Core Logic --------------

def search_page(ep,key,index,select,filter_expr=None,top=100,skip=None,orderby=None):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'select':select,'count':True}
    if filter_expr: body['filter']=filter_expr
    if skip is not None: body['skip']=skip
    if orderby: body['orderby']=orderby
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"search failed {r.status_code}: {r.text[:160]}")
    js=r.json()
    return js.get('value',[]), js.get('@odata.count')

def batch_update(ep,key,index,actions):
    if not actions: return
    url=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':actions}, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"index batch failed {r.status_code}: {r.text[:160]}")

# -------------- Recompute --------------

def count_total(ep,key,index):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"count failed {r.status_code}: {r.text[:140]}")
    return r.json().get('@odata.count',0)

def fmt_eta(start, done, total):
    if done==0 or total is None or total==0:
        return 'ETA ?'
    rate = done / (time.time()-start)
    remaining = total - done
    if rate<=0:
        return 'ETA ?'
    sec = remaining / rate
    if sec<60:
        return f"ETA {sec:.1f}s"
    mins=sec/60
    if mins<60:
        return f"ETA {mins:.1f}m"
    hrs=mins/60
    return f"ETA {hrs:.1f}h"

def recompute(ep,key,index,primary_key,vector_field,expected_dims,batch,limit=None,sample=None,orderby_pk=False, assume_absent=False, assume_present=False, progress_every=1):
    start=time.time()
    total_seen=0
    mismatches=0
    updated=0
    page=0
    sample_targets=None
    if sample:
        # We'll scan sequential pages until sample size reached or exhaustion
        pass

    # First attempt including vector field; if it's not retrievable we will retry without it.
    select=f"{primary_key},has_vector,{vector_field}"
    orderby=primary_key if orderby_pk else None

    try:
        total_docs = count_total(ep,key,index)
    except Exception as e:
        print(f"[warn] could not count total docs: {e}")
        total_docs=None
    print(f"[start] Recompute has_vector on index='{index}' total_docs={total_docs} batch={batch} mode={'sample' if sample else 'full'}")
    if assume_absent and assume_present:
        raise SystemExit('--assume-absent and --assume-present are mutually exclusive')

    while True:
        # Determine skip logic: if ordering by PK we avoid skip for stability; else we use skip offset.
        page+=1
        skip_val = None if orderby_pk else total_seen
        try:
            docs, total_count = search_page(ep,key,index,select,top=batch,skip=skip_val,orderby=orderby)
        except RuntimeError as e:
            msg=str(e)
            if 'not a retrievable field' in msg and vector_field in select:
                # Fallback: drop vector field from selection, we can only validate existing flags (no change to true/false logic)
                print(f"[warn] Vector field '{vector_field}' not retrievable; falling back to flag-only validation (no corrections possible)")
                select=f"{primary_key},has_vector"
                docs, total_count = search_page(ep,key,index,select,top=batch,skip=skip_val,orderby=orderby)
                vector_unavailable=True
            else:
                raise
        else:
            vector_unavailable=False
        if not docs:
            break
        actions=[]
        for d in docs:
            pk=d.get(primary_key)
            if pk is None:
                continue
            vec = None if vector_unavailable else d.get(vector_field)
            has_flag=d.get('has_vector')
            # Determine presence: explicit assumptions override detection
            if assume_absent:
                present=False
            elif assume_present:
                present=True
            else:
                present= isinstance(vec,list) and (expected_dims is None or len(vec)==expected_dims) and len(vec)>0
            if vector_unavailable and not (assume_absent or assume_present):
                # Cannot determine presence; skip updates
                pass
            else:
                if present!=has_flag:
                    mismatches+=1
                    actions.append({'@search.action':'mergeOrUpload', primary_key: pk, 'has_vector': present})
            total_seen+=1
            if sample and total_seen>=sample:
                break
        if actions:
            batch_update(ep,key,index,actions); updated+=len(actions)
        if page % progress_every == 0:
            pct = (total_seen/total_docs*100) if total_docs else None
            pct_s = f"{pct:.2f}%" if pct is not None else '?'
            eta = fmt_eta(start,total_seen,total_docs)
            print(f"[progress] pages={page} processed={total_seen} updated={updated} mismatches={mismatches} pct={pct_s} {eta}")
        # Termination conditions
        if sample and total_seen>=sample:
            break
        if limit and total_seen>=limit:
            break
        if total_docs is not None and total_seen>=total_docs:
            break
        if len(docs)<batch:
            break
        # Safety guard: if not ordering by PK and we've exceeded 1.5x reported total_docs, abort (likely pagination drift)
        if not orderby_pk and total_docs and total_seen > total_docs*1.5:
            print(f"[warn] Processed {total_seen} > 1.5x reported total {total_docs}; aborting to prevent runaway loop. Consider using --orderby-pk.")
            break
    elapsed=time.time()-start
    return {
        'index': index,
        'total_processed': total_seen,
        'mismatches_found': mismatches,
        'flags_updated': updated,
        'elapsed_sec': round(elapsed,2),
        'mode': 'sample' if sample else 'full',
        'assumed_absent': assume_absent,
        'assumed_present': assume_present
    }

# -------------- CLI --------------

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--primary-key', required=True)
    ap.add_argument('--vector-field', required=True)
    ap.add_argument('--expected-dims', type=int, default=None, help='Expected vector length (None to allow any >0)')
    ap.add_argument('--batch', type=int, default=256)
    ap.add_argument('--limit', type=int, help='Process at most this many docs (debug)')
    ap.add_argument('--sample', type=int, help='Sample mode: stop after this many docs')
    ap.add_argument('--orderby-pk', action='store_true', help='Order by primary key (stable pagination)')
    ap.add_argument('--assume-absent', action='store_true', help='Force has_vector=false for all docs (used when vector field not retrievable)')
    ap.add_argument('--assume-present', action='store_true', help='Force has_vector=true for all docs (use with caution)')
    ap.add_argument('--progress-every', type=int, default=1, help='Print progress every N pages')
    ap.add_argument('--json', action='store_true')
    args=ap.parse_args()
    ep,key=resolve_creds()
    result=recompute(ep,key,args.index,args.primary_key,args.vector_field,args.expected_dims,args.batch,args.limit,args.sample,args.orderby_pk,args.assume_absent,args.assume_present,args.progress_every)
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        print(f"Recompute {result['index']}: processed={result['total_processed']} mismatches={result['mismatches_found']} updated={result['flags_updated']} in {result['elapsed_sec']}s mode={result['mode']}")

if __name__=='__main__':
    main()
