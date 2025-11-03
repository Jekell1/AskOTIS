"""Export Mermaid flowcharts (full & condensed) for all program flow docs.

Creates a directory (default: flow_exports) with files:
  PROGRAMID_full.mmd
  PROGRAMID_condensed.mmd (if available)

Usage:
  python export_mermaid_flows.py [--outdir flow_exports] [--limit 200]
"""
from __future__ import annotations
import os, json, argparse, requests, pathlib

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def fetch_page(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'skip':skip,'select':'program_id,mermaid_flow,condensed_mermaid'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Export Mermaid graphs for program flows')
    ap.add_argument('--outdir',default='flow_exports')
    ap.add_argument('--page-size',type=int,default=1000)
    ap.add_argument('--limit',type=int)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    outdir=pathlib.Path(args.outdir); outdir.mkdir(parents=True,exist_ok=True)
    skip=0; written=0
    while True:
        page=fetch_page(ep,key,skip,args.page_size)
        if not page: break
        for doc in page:
            pid=doc.get('program_id');
            if not pid: continue
            full=doc.get('mermaid_flow'); condensed=doc.get('condensed_mermaid')
            if full:
                (outdir / f"{pid}_full.mmd").write_text(full,encoding='utf-8')
            if condensed:
                (outdir / f"{pid}_condensed.mmd").write_text(condensed,encoding='utf-8')
            written+=1
            if args.limit and written>=args.limit: break
        if args.limit and written>=args.limit: break
        skip+=len(page)
        if len(page)<args.page_size: break
    print('Exported graphs for',written,'programs to',outdir)

if __name__=='__main__':
    main()
