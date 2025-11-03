"""Quick probe of cobol-xrefs for a given simple_name or raw search token.

Usage:
  python quick_probe_xrefs.py --name BT-BRANCH [--top 20]
  python quick_probe_xrefs.py --search BRANCH --top 10

Resolves endpoint/key from local.settings.json or env.
"""
from __future__ import annotations
import os, json, argparse, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='cobol-xrefs'


def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key


def run_query(ep,key,body):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('ERROR',r.status_code,r.text[:200]); return []
    return r.json().get('value',[])


def main():
    ap=argparse.ArgumentParser(description='Probe cobol-xrefs for variable/simple_name occurrences')
    g=ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--name',help='Exact simple_name to filter (case-sensitive match in index)')
    g.add_argument('--search',help='Raw search string (no filter)')
    ap.add_argument('--top',type=int,default=20)
    args=ap.parse_args(); load(); ep,key=resolve()
    if args.name:
        body={'search':args.name,'filter':f"simple_name eq '{args.name}'",'top':args.top,'select':'simple_name,program_id,direction,line'}
        hits=run_query(ep,key,body)
        print(f"Exact simple_name='{args.name}' hits={len(hits)}")
    else:
        body={'search':args.search,'top':args.top,'select':'simple_name,program_id,direction,line'}
        hits=run_query(ep,key,body)
        print(f"Search token '{args.search}' hits={len(hits)}")
    for h in hits:
        print(h)

if __name__=='__main__':
    main()
