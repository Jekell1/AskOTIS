"""Backfill symbol_id for data items (program_id + path based).

Usage:
  python backfill_symbol_id_data_items.py --batch 500 --limit 5000
"""
from __future__ import annotations
import os, sys, json, time, requests, argparse
from id_normalization import make_data_item_symbol_id

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_data_items'

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

def count(ep,key):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if r.status_code!=200: print('Count failed', r.status_code, r.text[:200]); sys.exit(1)
    return r.json().get('@odata.count',0)

def fetch(ep,key,skip,top):
    body={'search':'*','top':top,'skip':skip,'select':'item_id,program_id,path,symbol_id'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200: print('Fetch failed', r.status_code, r.text[:200]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs})
    if r.status_code not in (200,201): print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--limit',type=int,default=0)
    ap.add_argument('--recompute',action='store_true',help='Recompute even if symbol_id already set')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    total=count(ep,key)
    target=args.limit if args.limit and args.limit<total else total
    print(f"Total={total} processing={target} batch={args.batch}")
    skip=0; processed=0; updated=0; start=time.time()
    while processed < target:
        rows=fetch(ep,key,skip,args.batch)
        if not rows: break
        out=[]
        for r in rows:
            if processed>=target: break
            processed+=1
            if r.get('symbol_id') and not args.recompute:
                continue
            pid=r.get('program_id') or ''
            path=r.get('path') or ''
            if not pid or not path:
                continue
            sid=make_data_item_symbol_id(pid,path)
            out.append({'@search.action':'mergeOrUpload','item_id':r['item_id'],'symbol_id':sid})
        upload(ep,key,out); updated+=len(out)
        pct=processed/target*100
        print(f"Processed {processed}/{target} ({pct:.2f}%) updated={updated}")
        skip+=args.batch
    dur=time.time()-start
    print(f"Done. Updated {updated} docs in {dur:.1f}s")

if __name__=='__main__':
    main()
