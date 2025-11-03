"""Export edge_id list for documents in new_cobol_flow_edges_v2 lacking has_vector==true (or missing flag).

Strategy:
 1. Count total and with has_vector true.
 2. If explicit has_vector false bucket count > 0, we can page only those with filter 'has_vector eq false'.
 3. If explicit false bucket == 0 but (total - with_true) > 0, treat remainder as 'unflagged' docs (no flag present) and we must scan all pages and collect docs missing flag or flag!=true.

Outputs:
  * JSON array (default) to stdout OR
  * line-delimited text if --txt specified
  * optional --out filename

Use this list for offline / parallel embedding, or validation pipelines.

Example:
  python export_unflagged_flow_edges_v2.py --limit 50000 --out unflagged_edges.json
"""
from __future__ import annotations
import os, json, argparse, sys, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'

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

def count(ep,key,filt=None):
    body={'search':'*','top':0,'count':True}
    if filt: body['filter']=filt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(r.text[:200])
    return r.json().get('@odata.count',0)

def fetch_page(ep,key,top,skip,select='edge_id,has_vector'):
    body={'search':'*','top':top,'skip':skip,'select':select,'count':False}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(r.text[:200])
    return r.json().get('value',[])

def fetch_filtered(ep,key,top,skip):
    body={'search':'*','top':top,'skip':skip,'select':'edge_id,has_vector','filter':'has_vector eq false'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise SystemExit(r.text[:200])
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Export edge_ids that still need embeddings')
    ap.add_argument('--page',type=int,default=1000)
    ap.add_argument('--limit',type=int,default=0,help='Stop after collecting this many ids (0=all)')
    ap.add_argument('--out')
    ap.add_argument('--txt',action='store_true',help='Write as newline-delimited text instead of JSON array')
    ap.add_argument('--pretty',action='store_true')
    args=ap.parse_args(); load_settings(); ep,key=resolve_search()
    total=count(ep,key)
    try:
        with_true=count(ep,key,'has_vector eq true')
    except Exception:
        with_true=None
    without_explicit=None
    if with_true is not None:
        try:
            without_explicit=count(ep,key,'has_vector eq false')
        except Exception:
            pass
    remainder=None
    unflagged_mode=False
    if with_true is not None:
        remainder=total-with_true if total>=with_true else 0
        if without_explicit==0 and remainder>0:
            unflagged_mode=True
    print(json.dumps({'total':total,'with_true':with_true,'explicit_false':without_explicit,'remainder':remainder,'unflagged_mode':unflagged_mode},indent=2))
    results=[]; skip=0
    while True:
        if unflagged_mode:
            rows=fetch_page(ep,key,args.page,skip)
            if not rows: break
            for r in rows:
                if r.get('has_vector') is True: continue
                results.append(r['edge_id'])
                if args.limit and len(results)>=args.limit: break
        else:
            rows=fetch_filtered(ep,key,args.page,skip)
            if not rows: break
            for r in rows:
                results.append(r['edge_id'])
                if args.limit and len(results)>=args.limit: break
        if args.limit and len(results)>=args.limit: break
        if len(rows)<args.page: break
        skip+=args.page
    print(f"Collected {len(results)} edge_ids needing embedding")
    if args.out:
        mode='w'
        with open(args.out,'w',encoding='utf-8') as f:
            if args.txt:
                for eid in results: f.write(eid+'\n')
            else:
                json.dump(results,f,indent=2 if args.pretty else None)
        print(f"Wrote {len(results)} ids to {args.out}")
    else:
        if args.txt:
            for eid in results: print(eid)
        else:
            print(json.dumps(results, indent=2 if args.pretty else None))

if __name__=='__main__':
    main()
