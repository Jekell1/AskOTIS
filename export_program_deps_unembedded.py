"""Export unembedded program dependency docs (missing has_vector true) with blob length.

Usage:
  python export_program_deps_unembedded.py --out unembedded_program_deps.jsonl
  python export_program_deps_unembedded.py --limit 200 --pretty
"""
from __future__ import annotations
import os, json, argparse, requests, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'

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
    body={'search':'*','top':top,'skip':skip,'select':'program_id,dependency_blob,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--page',type=int,default=400)
    ap.add_argument('--limit',type=int,help='Max docs to export')
    ap.add_argument('--out',help='JSONL output file (stdout if omitted)')
    ap.add_argument('--pretty',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    out = open(args.out,'w',encoding='utf-8') if args.out else sys.stdout
    skip=0; exported=0; scanned=0
    while True:
        rows=fetch(ep,key,skip,args.page)
        if not rows: break
        for r in rows:
            if r.get('has_vector') is True:
                continue
            blob=(r.get('dependency_blob') or '')
            rec={'program_id':r.get('program_id'), 'blob_len': len(blob), 'starts_with': blob[:60]}
            line=json.dumps(rec, indent=2) if args.pretty else json.dumps(rec)
            out.write(line+('\n' if not line.endswith('\n') else ''))
            exported+=1
            if args.limit and exported>=args.limit:
                break
        scanned+=len(rows)
        if args.limit and exported>=args.limit:
            break
        if len(rows)<args.page:
            break
        skip+=args.page
    if out is not sys.stdout:
        out.close()
    print(json.dumps({'exported':exported,'scanned':scanned,'output':args.out or 'stdout'},indent=2), file=sys.stderr)

if __name__=='__main__':
    main()
