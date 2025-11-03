"""Export IDs of documents lacking embeddings for specified index(es).

Uses filter has_vector eq false and streams doc primary keys to JSONL.

Usage:
  python export_missing_vectors.py --index new_cobol_flow_edges_v2 --out missing_edges.jsonl
  python export_missing_vectors.py --index new_cobol_program_deps --top 2000
"""
from __future__ import annotations
import os, json, argparse, sys, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

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

def get_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API}",headers={'api-key':key}); r.raise_for_status(); return r.json()

def find_pk(idx):
    for f in idx.get('fields',[]):
        if f.get('key'): return f['name']
    raise RuntimeError('Primary key field not found')

def search(ep,key,index, top=1000, skip=0, filt=None, select='*'):
    body={'search':'*','top':top,'skip':skip,'select':select,'count':False}
    if filt: body['filter']=filt
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', action='append', required=True)
    ap.add_argument('--out', help='Output JSONL (default stdout)')
    ap.add_argument('--top', type=int, default=0, help='Optional cap across each index')
    ap.add_argument('--batch', type=int, default=1000)
    args=ap.parse_args(); load(); ep,key=resolve()
    out_f = open(args.out,'w',encoding='utf-8') if args.out else sys.stdout
    total_written=0
    for name in args.index:
        idx=get_index(ep,key,name)
        pk=find_pk(idx)
        skip=0; remaining=args.top if args.top>0 else None
        while True:
            page=search(ep,key,name, top=min(args.batch, remaining) if remaining else args.batch, skip=skip, filt='has_vector eq false', select=pk)
            if not page: break
            for d in page:
                out_f.write(json.dumps({'index':name,'id':d[pk]})+'\n')
                total_written+=1
                if remaining:
                    remaining-=1
                    if remaining<=0: break
            if remaining and remaining<=0: break
            if len(page)< (min(args.batch, remaining) if remaining else args.batch): break
            skip+=args.batch
    if out_f is not sys.stdout:
        out_f.close()
    print(f"Wrote {total_written} missing vector ids", file=sys.stderr)

if __name__=='__main__':
    main()
