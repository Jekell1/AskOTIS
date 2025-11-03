"""Accurately report vector coverage for the symbol reference index.

Counts total docs and those missing vectors (has_vector false or null), then prints
JSON + human summary. Uses direct search counts — ignores any stale checkpoint files.

Usage:
    python verify_symbol_ref_vector_coverage.py
    python verify_symbol_ref_vector_coverage.py --index new_cobol_symbol_refs

Exit codes:
  0 = success (prints coverage)
  1 = API failure
"""
from __future__ import annotations
import os, json, argparse, sys, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
DEFAULT_INDEX = 'new_cobol_symbol_refs'

def load_env():
    try:
        data = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in data and k not in os.environ:
                os.environ[k]=data[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[ERROR] Missing endpoint/key environment variables.')
        sys.exit(1)
    return ep.rstrip('/'), key

def count(ep,key,index,flt=None):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt:
        body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print(f"[ERROR] Count failed {r.status_code}: {r.text[:300]}")
        sys.exit(1)
    return r.json().get('@odata.count',0)

def main():
    ap=argparse.ArgumentParser(description='Verify vector coverage for symbol reference index')
    ap.add_argument('--index',default=DEFAULT_INDEX)
    ap.add_argument('--json',action='store_true',help='Output JSON only')
    args=ap.parse_args(); load_env(); ep,key=resolve()
    idx=args.index
    missing_filter="((has_vector eq false) or (has_vector eq null))"
    total=count(ep,key,idx)
    missing=count(ep,key,idx,missing_filter) if total else 0
    with_vec=total-missing
    pct=(with_vec/total*100) if total else 0.0
    payload={
        'index':idx,
        'total_docs':total,
        'with_vector':with_vec,
        'missing_vector':missing,
        'coverage_percent':round(pct,6)
    }
    if args.json:
        print(json.dumps(payload,indent=2))
    else:
        print(f"Index: {idx}\nTotal docs: {total}\nWith vector: {with_vec}\nMissing vector: {missing}\nCoverage: {pct:.6f}%")
    # Provide guidance if not 100%
    if missing>0:
        print('\nNext action: run remaining backfill (will resume) or investigate ingestion gaps.')
    sys.exit(0)

if __name__=='__main__':
    main()
