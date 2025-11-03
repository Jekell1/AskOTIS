"""Generate a stoplist JSON of high-frequency (boilerplate) paragraph canonical names.

Heuristic:
  - Fetch paragraph alias rows (one pass collecting canonical_occurrences max per canonical)
  - Select those with occurrence >= threshold (default 100)
  - Output JSON with metadata (count, threshold, top canonicals) to file or stdout

Usage:
  python generate_paragraph_stoplist.py --min-occ 120 --export paragraph_stoplist.json
"""
from __future__ import annotations
import os,json,requests,argparse,sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_name_aliases'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'),key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('[ERROR]',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def collect(ep,key):
    skip=0; page=3000; counts={}
    while True:
        body={'search':'*','top':page,'skip':skip,'filter':"kind eq 'PARAGRAPH'",'select':'canonical_name,canonical_occurrences'}
        data=search(ep,key,body)
        rows=data.get('value',[])
        if not rows: break
        for r in rows:
            c=r['canonical_name']; occ=r.get('canonical_occurrences') or 0
            if occ>counts.get(c,0): counts[c]=occ
        skip+=len(rows)
        if len(rows)<page: break
    return counts

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--min-occ',type=int,default=100)
    ap.add_argument('--export')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    counts=collect(ep,key)
    selected={k:v for k,v in counts.items() if v>=args.min_occ}
    out={'min_occ':args.min_occ,'selected_count':len(selected),'canonicals':sorted(selected.items(), key=lambda x:x[1], reverse=True)}
    print(json.dumps(out,indent=2))
    if args.export:
        with open(args.export,'w',encoding='utf-8') as f:
            json.dump(out,f,indent=2)
        print(f"Exported stoplist -> {args.export}")

if __name__=='__main__':
    main()
