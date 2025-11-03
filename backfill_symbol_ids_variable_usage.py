"""Backfill symbol_id_global (and placeholder symbol_id) for variable usage docs.

Since variable usage docs aggregate across programs, symbol_id will mirror symbol_id_global for now.
If a future per-program variant is needed, we can explode docs.
"""
from __future__ import annotations
import os, sys, json, time, argparse, requests
from id_normalization import make_global_symbol_id

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_variable_usage'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch(ep,key,skip,top):
    body={'search':'*','top':top,'skip':skip,'select':'variable_id,symbol_id_global,symbol_id'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:200]); sys.exit(1)
    return r.json().get('value',[])

def count(ep,key):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if r.status_code!=200: print('Count failed', r.status_code, r.text[:200]); sys.exit(1)
    return r.json().get('@odata.count',0)

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs})
    if r.status_code not in (200,201): print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--recompute',action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    total=count(ep,key); target=args.limit if args.limit and args.limit<total else total
    print(f"Total={total} processing={target} batch={args.batch}")
    skip=0; processed=0; updated=0; start=time.time()
    while processed<target:
        rows=fetch(ep,key,skip,args.batch)
        if not rows: break
        out=[]
        for r in rows:
            if processed>=target: break
            processed+=1
            if r.get('symbol_id_global') and not args.recompute:
                continue
            vid=r.get('variable_id') or ''
            if not vid: continue
            sidg=make_global_symbol_id(vid)
            out.append({'@search.action':'mergeOrUpload','variable_id':vid,'symbol_id_global':sidg,'symbol_id':sidg})
        upload(ep,key,out); updated+=len(out)
        print(f"Processed {processed}/{target} ({processed/target*100:.2f}%) updated={updated}")
        skip+=args.batch
    print(f"Done. Updated {updated} docs in {time.time()-start:.1f}s")

if __name__=='__main__':
    main()
