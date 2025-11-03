"""List COBOL program source files that contain no COPY usage (based on file scan vs usage index).

Run:
  python analysis_programs_without_copy_usage.py --roots . --limit 5
"""
from __future__ import annotations
import os, json, argparse, sys, re

USAGE_STATE_INDEX='new_cobol_copybook_usage'  # referenced indirectly by enumerating programs seen in usage index
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
COBOL_EXTS={'.cbl','.cob','.cobol','.CBL','.COB'}

import requests

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
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def search(ep,key,body):
    r=requests.post(f"{ep}/indexes/{USAGE_STATE_INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:300],file=sys.stderr); sys.exit(1)
    return r.json()

def enumerate_usage_programs(ep,key):
    progs=set(); skip=0; page=3000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':'program_id'}
        data=search(ep,key,body)
        rows=data.get('value',[])
        if not rows: break
        for r in rows:
            p=r.get('program_id')
            if p: progs.add(p)
        skip+=len(rows)
        if len(rows)<page: break
    return progs

def program_id_from_path(path: str) -> str:
    base=os.path.basename(path)
    name,_=os.path.splitext(base)
    return name.upper()

def walk_programs(roots):
    for root in roots:
        for dp,_,files in os.walk(root):
            for f in files:
                ext=os.path.splitext(f)[1]
                if ext in COBOL_EXTS or ext.lower() in {e.lower() for e in COBOL_EXTS}:
                    yield os.path.join(dp,f)

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--roots',nargs='+',default=['.'])
    ap.add_argument('--limit',type=int,help='Only show first N missing programs')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    usage_programs=enumerate_usage_programs(ep,key)
    missing=[]; total=0
    for path in walk_programs(args.roots):
        total+=1
        pid=program_id_from_path(path)
        if pid not in usage_programs:
            missing.append({'program_id':pid,'path':path})
            if args.limit and len(missing)>=args.limit:
                break
    print(json.dumps({'scanned_files':total,'programs_with_copy':len(usage_programs),'missing_programs_listed':len(missing),'sample_missing':missing},indent=2))

if __name__=='__main__':
    main()
