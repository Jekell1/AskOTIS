"""Export ref_ids lacking vectors in new_cobol_symbol_refs to a file for targeted re-embed.

Usage:
  python export_missing_symbol_ref_vectors.py --out missing_symbol_refs.txt --max 20000
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

SELECT='ref_id'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items(): os.environ.setdefault(k,v)
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def fetch(ep,key,top=1000):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','filter':'(has_vector eq false) or (has_vector eq null)','top':top,'select':SELECT}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise SystemExit(r.text[:400])
    return [v['ref_id'] for v in r.json().get('value',[])]


def main():
    ap=argparse.ArgumentParser(description='Export missing symbol ref vector IDs')
    ap.add_argument('--out',required=True)
    ap.add_argument('--max',type=int,default=50000)
    args=ap.parse_args(); load(); ep,key=resolve()
    missing=[]
    while len(missing) < args.max:
        batch=fetch(ep,key,top=min(1000,args.max-len(missing)))
        if not batch: break
        missing.extend(batch)
    with open(args.out,'w',encoding='utf-8') as f:
        for rid in missing:
            f.write(rid+'\n')
    print(f"Exported {len(missing)} missing ref_ids to {args.out}")

if __name__=='__main__':
    main()
