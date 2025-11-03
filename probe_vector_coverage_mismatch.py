"""Detect potential mismatches between reported has_vector coverage and total docs.

Heuristic flags:
  - Index has vector fields AND a has_vector field.
  - Count(has_vector eq true) < total_docs but Count(has_vector eq false) == 0.
    This usually means some docs have no flag populated (null) so the false filter misses them.

Output: JSON list of suspicious indexes with counts + suggested remediation.

Usage:
  python probe_vector_coverage_mismatch.py --index-filter cobol
"""
from __future__ import annotations
import os, json, argparse, sys, requests
from typing import Any, Dict, List

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

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

def list_indexes(ep,key):
    r=requests.get(f"{ep}/indexes?api-version={API_VERSION}",headers={'api-key':key})
    r.raise_for_status(); return r.json().get('value',[])

def get_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}",headers={'api-key':key}); r.raise_for_status(); return r.json()

def count(ep,key,index, filt):
    body={'search':'*','top':0,'count':True,'filter':filt}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('@odata.count',0)

def total(ep,key,index):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('@odata.count',0)

def detect_vector_fields(idx):
    out=[]
    for f in idx.get('fields',[]):
        dim=f.get('vectorSearchDimensions') or f.get('dimensions')
        if dim and f.get('type','').startswith('Collection(Edm.Single)'):
            out.append({'name':f['name'],'dimensions':dim})
    return out

def has_vector_field(idx):
    names={f['name'] for f in idx.get('fields',[])}
    return 'has_vector' in names

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index-filter')
    ap.add_argument('--pretty',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    result={'suspicious':[], 'checked':0}
    for meta in list_indexes(ep,key):
        name=meta.get('name');
        if args.index_filter and args.index_filter not in name: continue
        try:
            idx=get_index(ep,key,name)
        except Exception as e:
            result['suspicious'].append({'name':name,'error':str(e)}); continue
        vfields=detect_vector_fields(idx)
        if not vfields: continue
        if not has_vector_field(idx):
            continue
        try:
            t=total(ep,key,name)
            with_v=count(ep,key,name,'has_vector eq true')
            without_v=count(ep,key,name,'has_vector eq false')
        except Exception as e:
            result['suspicious'].append({'name':name,'error':str(e)}); continue
        result['checked']+=1
        if (with_v < t) and without_v==0:
            result['suspicious'].append({
                'name':name,
                'total_docs':t,
                'with_vector':with_v,
                'without_vector':without_v,
                'vector_fields':vfields,
                'issue':'missing_false_flag_or_partial_population',
                'suggest':'Re-run backfill OR run patch_add_has_vector_field.py --populate to set flags where vectors exist.'
            })
    print(json.dumps(result, indent=2 if args.pretty else None))

if __name__=='__main__':
    main()
