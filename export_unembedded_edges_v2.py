"""Export edge_ids for new_cobol_flow_edges_v2 that lack has_vector.

Because has_vector eq false returns 0 (docs missing field entirely), we must scan and output where has_vector not true.
This is a full scan; use --limit to stop early.
"""
from __future__ import annotations
import os, json, argparse, requests, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'edge_id,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--page',type=int,default=1000)
    ap.add_argument('--limit',type=int,help='Stop after exporting this many IDs')
    ap.add_argument('--out',required=True,help='Output JSONL file path')
    args=ap.parse_args(); load(); ep,key=resolve()
    out=open(args.out,'w',encoding='utf-8')
    skip=0; exported=0; scanned=0
    try:
        while True:
            rows=fetch(ep,key,skip,args.page)
            if not rows: break
            for r in rows:
                if r.get('has_vector') is True: continue
                out.write(json.dumps({'edge_id':r['edge_id']})+'\n')
                exported+=1
                if args.limit and exported>=args.limit:
                    raise StopIteration
            scanned+=len(rows)
            if len(rows)<args.page: break
            skip+=args.page
    except StopIteration:
        pass
    finally:
        out.close()
    print(json.dumps({'exported':exported,'scanned':scanned,'out':args.out},indent=2))

if __name__=='__main__':
    main()
