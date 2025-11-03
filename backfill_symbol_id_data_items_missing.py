"""Backfill symbol_id for ALL remaining data items without it (filter-based; avoids $skip>100k).

Strategy:
    * Loop: search with filter `symbol_id eq null` (top <= 1000)
    * For each doc having required fields, compute symbol_id and mergeOrUpload
    * Stop when no docs remain OR adaptive threshold triggers

Adaptive Options:
    --adaptive-window N       Track updates over last N loops
    --adaptive-min-updates M  If average updates in that window < M, stop (plateau)
    --coverage-every K        Every K loops print coverage (extra count queries)

Usage Examples:
    python backfill_symbol_id_data_items_missing.py --batch 1000
    python backfill_symbol_id_data_items_missing.py --adaptive-window 10 --adaptive-min-updates 50
    python backfill_symbol_id_data_items_missing.py --coverage-every 25
"""
from __future__ import annotations
import os, sys, json, time, argparse, requests, collections
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

def fetch_missing(ep,key,top):
    if top>1000: top=1000  # service cap
    body={'search':'*','filter':'symbol_id eq null','top':top,'select':'item_id,program_id,path'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs})
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:300]); sys.exit(1)

def count_with_symbol_id(ep,key):
    body={'search':'*','count':True,'top':0,'filter':'symbol_id ne null'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200: return None
    return r.json().get('@odata.count')

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=1000)
    ap.add_argument('--max-iterations',type=int,default=0,help='Stop after N loops even if more remain')
    ap.add_argument('--sleep',type=float,default=0.02)
    ap.add_argument('--adaptive-window',type=int,default=0)
    ap.add_argument('--adaptive-min-updates',type=int,default=0)
    ap.add_argument('--coverage-every',type=int,default=0,help='Every K loops, compute and print with_symbol_id count (extra queries)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    total_updated=0; loops=0; start=time.time()
    recent=collections.deque(maxlen=args.adaptive_window or 0)
    initial_with = count_with_symbol_id(ep,key) if args.coverage_every else None
    while True:
        if args.max_iterations and loops>=args.max_iterations:
            print('Reached iteration cap. Stopping.')
            break
        rows=fetch_missing(ep,key,args.batch)
        if not rows:
            print('No more missing symbol_id docs.')
            break
        batch=[]
        for r in rows:
            pid=r.get('program_id') or ''
            path=r.get('path') or ''
            if not pid or not path: continue
            sid=make_data_item_symbol_id(pid,path)
            batch.append({'@search.action':'mergeOrUpload','item_id':r['item_id'],'symbol_id':sid})
        upload(ep,key,batch)
        total_updated+=len(batch)
        loops+=1
        if recent.maxlen:
            recent.append(len(batch))
            if args.adaptive_min_updates and len(recent)==recent.maxlen:
                avg=sum(recent)/len(recent)
                if avg < args.adaptive_min_updates:
                    print(f"Adaptive stop: avg last {len(recent)} loops={avg:.1f} < {args.adaptive_min_updates}")
                    break
        if args.coverage_every and loops % args.coverage_every == 0:
            with_id=count_with_symbol_id(ep,key)
            if with_id is not None:
                delta = with_id - (initial_with or 0)
                print(f"Loop {loops} updated {len(batch)} cumulative {total_updated} coverage_with_symbol_id={with_id} (+{delta})")
            else:
                print(f"Loop {loops} updated {len(batch)} cumulative {total_updated} (coverage query failed)")
        else:
            print(f"Loop {loops} updated {len(batch)} (cumulative {total_updated})")
        time.sleep(args.sleep)
    print(f"Completed. Total newly set symbol_id={total_updated} in {time.time()-start:.1f}s")

if __name__=='__main__':
    main()
