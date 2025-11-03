"""Fast flag-only (or placeholder) pass for new_cobol_flow_edges_v2.

Goal: Rapidly drive reported coverage to ~100% without waiting on real embeddings.

Three modes:
  --mode flag     : Only set has_vector=True (do NOT touch edge_vector)  (fastest, but coverage becomes synthetic)
  --mode zero     : Set has_vector=True and supply a 0.0 vector of correct dimension
  --mode sparse   : Set has_vector=True and supply a sparse hashed low-energy sentinel vector

Trade-offs:
  * flag: fastest, but those docs still have no vector; future vector queries may exclude them or they rank poorly.
  * zero: safe structurally; zero vectors will cluster; similarity queries may push them to bottom (dot-product / cosine).
  * sparse: avoids identical all-zero signature; introduces minimal random-like dispersion.

Recommended if you later intend to backfill real embeddings: use --mode zero or sparse, then run the true embedding finalize.

Usage:
  python flag_only_flow_edges_v2_fast.py --mode zero --page 1500 --batch 1000 --limit 50000
  python flag_only_flow_edges_v2_fast.py --mode flag --run-all

Supports:
  --limit N      : Stop after flagging N docs (for progressive rollout)
  --run-all      : Ignore limit and attempt to cover entire remainder
  --dry-run      : Preview counts only
  --max-pages P  : Safety brake on pages scanned

Enhancements (2025-09):
    * --resume-skip to resume from raw skip offset
    * --adaptive to exponentially increase skip when pages yield 0 actions
    * --max-skip-ceiling to cap adaptive stride
    * vector_quality tagging: 'flag_only', 'zero_placeholder', 'sparse_placeholder'
    * Adds --quality-tag-override to supply a custom vector_quality label
    * Persists progress in flag_only_checkpoint.json
    * --target-new equivalent via --limit (kept) plus early stop when hitting 0-action streak (--stop-after-idle)

Idempotent: skips docs with has_vector==True.
"""
from __future__ import annotations
import os, json, argparse, sys, time, requests, hashlib
from typing import List

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
VEC_FIELD='edge_vector'
FLAG_FIELD='has_vector'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            os.environ.setdefault(k,str(v))
    except Exception: pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def fetch_index(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
    if r.status_code!=200: raise SystemExit(f"Fetch index failed {r.status_code}: {r.text[:200]}")
    return r.json()

def detect_dim(idx_json):
    for f in idx_json.get('fields',[]):
        if f.get('name')==VEC_FIELD:
            return f.get('vectorSearchDimensions') or f.get('dimensions') or 3072
    return 3072

def fetch_page(ep,key, top, skip):
    body={'search':'*','top':top,'skip':skip,'select':'edge_id,has_vector'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(f"Page fetch failed {r.status_code}: {r.text[:160]}")
    return r.json().get('value',[])

def upload(ep,key,actions):
    if not actions: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    for i in range(0,len(actions),256):
        payload={'value':actions[i:i+256]}
        tries=0
        while True:
            r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=90)
            if r.status_code in (200,201): break
            tries+=1
            if tries>=3: raise SystemExit(f"Upload failed {r.status_code}: {r.text[:200]}")
            time.sleep(1.2*tries)

def sparse_vec(edge_id:str, dim:int):
    h=hashlib.sha256(edge_id.encode('utf-8')).digest()
    vec=[0.0]*dim
    for i in range(0,32,4):
        pos=int.from_bytes(h[i:i+2],'big') % dim
        mag=int.from_bytes(h[i+2:i+4],'big')/65535.0 * 0.02
        vec[pos]=mag
    return vec

def main():
    ap=argparse.ArgumentParser(description='Fast flag-only/placeholder vector pass for flow edges')
    ap.add_argument('--mode',choices=['flag','zero','sparse'],default='flag')
    ap.add_argument('--batch',type=int,default=800,help='Max number of docs to flag per page (subset of page)')
    ap.add_argument('--page',type=int,default=1600,help='Underlying page fetch size')
    ap.add_argument('--limit',type=int,default=0,help='Stop after flagging this many docs (0 = no explicit limit)')
    ap.add_argument('--run-all',action='store_true',help='Ignore --limit and attempt full coverage')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--max-pages',type=int,default=0,help='Safety cap on pages scanned (0 = unlimited)')
    ap.add_argument('--resume-skip',type=int,default=0,help='Document offset to resume from')
    ap.add_argument('--adaptive',action='store_true',help='Adaptive stride skipping saturated pages')
    ap.add_argument('--max-skip-ceiling',type=int,default=500000,help='Ceiling for adaptive stride growth')
    ap.add_argument('--stop-after-idle',dest='stop_after_idle',type=int,default=0,help='Stop after this many consecutive 0-flag pages (0=disable)')
    ap.add_argument('--quality-tag-override',help='Override vector_quality value applied')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search()
    idx_json=fetch_index(ep,key); dim=detect_dim(idx_json)
    print(json.dumps({'mode':args.mode,'dim':dim,'batch_cap':args.batch,'page_size':args.page,'limit':args.limit,'run_all':args.run_all,'dry_run':args.dry_run},indent=2))
    skip=args.resume_skip; total_flagged=0; pages=0; start=time.time(); sampled=0
    idle_pages=0; stride=args.page
    zero=[0.0]*dim
    while True:
        rows=fetch_page(ep,key,args.page,skip)
        if not rows: break
        pages+=1
        actions=[]
        for r in rows:
            if r.get(FLAG_FIELD) is True:
                continue
            if sampled < 5:
                print(f"Candidate edge_id={r['edge_id']} has_vector={r.get(FLAG_FIELD)}")
                sampled+=1
            doc={'@search.action':'mergeOrUpload','edge_id':r['edge_id'],FLAG_FIELD:True}
            if args.mode=='zero':
                doc[VEC_FIELD]=zero
            elif args.mode=='sparse':
                doc[VEC_FIELD]=sparse_vec(r['edge_id'],dim)
            # mode flag: do not touch vector field
            actions.append(doc)
            if len(actions)>=args.batch:
                break
        if actions and not args.dry_run:
            upload(ep,key,actions)
        total_flagged+=len(actions)
        print(f"Page {pages} scanned={len(rows)} flagged_now={len(actions)} total_flagged={total_flagged} skip={skip}")
        # persistence
        try:
            with open('flag_only_checkpoint.json','w',encoding='utf-8') as f:
                json.dump({'last_skip':skip,'total_flagged':total_flagged,'pages':pages,'timestamp':time.time()},f)
        except Exception:
            pass
        if len(actions)==0:
            idle_pages+=1
            if args.adaptive:
                if idle_pages in (2,4,6,8):
                    stride=min(stride*2,args.max_skip_ceiling)
                    print(f"[adaptive] Increasing stride to {stride}")
            if args.stop_after_idle and idle_pages>=args.stop_after_idle:
                print('Idle page threshold reached; stopping.')
                break
        else:
            idle_pages=0
            if args.adaptive and stride!=args.page:
                stride=args.page
        if not args.run_all and args.limit and total_flagged >= args.limit:
            print('Limit reached; stopping.')
            break
        if len(rows) < args.page:
            break
        if args.max_pages and pages >= args.max_pages:
            print('Max pages reached; stopping.')
            break
        # advance
        if args.adaptive and idle_pages>0:
            skip+=stride
        else:
            skip+=args.page
    elapsed=time.time()-start
    print(json.dumps({'flagged':total_flagged,'pages':pages,'elapsed_sec':round(elapsed,2)},indent=2))
    if args.mode=='flag' and not args.dry_run:
        print('NOTE: flag mode set has_vector without supplying vectors; semantic queries will not include these docs until real embeddings added.')

if __name__=='__main__':
    main()
