"""Detect normalization collisions where distinct raw names collapse to same canonical form.

Usage:
  python analysis_alias_collisions.py --kind PROGRAM
"""
from __future__ import annotations
import os,json,requests,argparse,sys,re

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
ALIAS_INDEX='new_cobol_name_aliases'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{ALIAS_INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def enumerate_aliases(ep,key,kind):
    skip=0; page=3000; out=[]
    filt=f"kind eq '{kind}'" if kind else None
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'canonical_name,alias,variant_type,kind'}
        if filt: body['filter']=filt
        data=search(ep,key,body); rows=data.get('value',[])
        if not rows: break
        out.extend(rows); skip+=len(rows)
        if len(rows)<page: break
    return out

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--kind',help='PROGRAM|COPYBOOK|PARAGRAPH')
    ap.add_argument('--min',type=int,default=2,help='Report collisions with at least this many distinct raw forms')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    rows=enumerate_aliases(ep,key,args.kind)
    buckets={}
    for r in rows:
        canon=r['canonical_name']
        buckets.setdefault(canon,set()).add(r['alias'])
    collisions={k:sorted(v) for k,v in buckets.items() if len(v)>=args.min}
    print(json.dumps({'kind':args.kind,'collision_groups':len(collisions),'examples':list(dict(sorted(collisions.items(), key=lambda x: -len(x[1]))[:20]).items())},indent=2))

if __name__=='__main__':
    main()
