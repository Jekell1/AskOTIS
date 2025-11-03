"""Analyze distribution of COPY usage density per program and copybook spread.

Outputs basic stats + top heavy hitters.
Run:
  python analysis_program_copy_usage_distribution.py --top 20
"""
from __future__ import annotations
import os,json,requests,argparse,statistics,sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_usage'

FIELDS='program_id,copybook_name_plain'

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
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:300],file=sys.stderr); sys.exit(1)
    return r.json()

def iter_all(ep,key,select):
    skip=0; page=3000
    while True:
        body={'search':'*','top':page,'skip':skip,'select':select}
        data=search(ep,key,body)
        rows=data.get('value',[])
        if not rows: break
        for r in rows: yield r
        skip+=len(rows)
        if len(rows)<page: break

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--top',type=int,default=20)
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    per_program={}
    copybooks_per_program={}
    for r in iter_all(ep,key,FIELDS):
        p=r.get('program_id'); c=r.get('copybook_name_plain')
        if not p: continue
        per_program[p]=per_program.get(p,0)+1
        if c:
            copybooks_per_program.setdefault(p,set()).add(c)
    counts=list(per_program.values())
    if not counts:
        print('No data'); return
    stats={
        'program_count':len(per_program),
        'total_usages':sum(counts),
        'min_copies':min(counts),
        'max_copies':max(counts),
        'mean_copies':round(sum(counts)/len(counts),2),
        'median_copies':int(statistics.median(counts)),
        'p90':sorted(counts)[int(len(counts)*0.90)-1],
        'p99':sorted(counts)[int(len(counts)*0.99)-1],
    }
    top=sorted(per_program.items(),key=lambda x:x[1],reverse=True)[:args.top]
    top_rows=[{'program_id':p,'copy_usages':cnt,'distinct_copybooks':len(copybooks_per_program.get(p,[]))} for p,cnt in top]
    print(json.dumps({'stats':stats,'top':top_rows},indent=2))

if __name__=='__main__':
    main()
