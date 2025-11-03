"""Generate a chronological timeline of symbol occurrences across programs.

Inputs:
  --symbol NAME (case-insensitive) OR --symbol-id-global GID

Output (JSON): ordered list of occurrences with program_id, line_number,
paragraph_name (if present), and snippet (excerpt). Sorted by program_id then line_number.

Usage:
  python symbol_usage_timeline.py --symbol CLAIM-NO
  python symbol_usage_timeline.py --symbol-id-global G_SYM_xxxx
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search_symbol(ep,key,filt,top=1000,skip=0):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    # Use only fields that actually exist in index schema (paragraph_name was validated as present; if removed, fallback occurs)
    select_fields=['ref_id','program_id','symbol_name','line_number','paragraph_name','excerpt']
    body={'search':'*','filter':filt,'top':top,'skip':skip,'select':','.join(select_fields)}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code==400 and 'paragraph_name' in r.text:
        # fallback without paragraph_name
        fallback=[f for f in select_fields if f!='paragraph_name']
        body['select']=','.join(fallback)
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser(description='Symbol usage timeline generator')
    g=ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--symbol')
    g.add_argument('--symbol-id-global')
    ap.add_argument('--limit',type=int,default=0,help='Max occurrences (0=all)')
    args=ap.parse_args(); load(); ep,key=resolve()
    if args.symbol:
        name=args.symbol.upper()
        filt=f"symbol_name eq '{name}'"
    else:
        gid=args.symbol_id_global
        filt=f"symbol_id_global eq '{gid}'"
    out=[]; skip=0; PAGE=1000
    while True:
        rows=search_symbol(ep,key,filt,top=PAGE,skip=skip)
        if not rows: break
        skip+=len(rows)
        out.extend(rows)
        if args.limit and len(out)>=args.limit: break
        if len(rows)<PAGE: break
        if skip>=100000: break  # safety guard
    # Sort deterministically
    out.sort(key=lambda r: (r.get('program_id') or '', r.get('line_number') or 0))
    # Basic time-like sequence number
    for i,r in enumerate(out, start=1):
        r['sequence']=i
    print(json.dumps({'count':len(out),'occurrences':out},indent=2)[:200000])

if __name__=='__main__':
    main()
