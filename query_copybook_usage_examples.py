"""Example queries for new_cobol_copybook_usage acceptance checks.

Usage:
  python query_copybook_usage_examples.py --copybook SCREEN.CPY
  python query_copybook_usage_examples.py --program DAILY
"""
from __future__ import annotations
import os,sys,json,argparse,requests
from retrieval.alias_resolver import AliasResolver

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_usage'

def load_settings():
    """Load local.settings.json Values if endpoint/key not already set."""
    if os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT'):
        return
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
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def main():
    ap=argparse.ArgumentParser(description='Copybook usage acceptance / discovery queries (alias aware)')
    ap.add_argument('--copybook',help='Copybook name (alias-friendly)')
    ap.add_argument('--program',help='Program name (alias-friendly)')
    ap.add_argument('--top',type=int,default=200)
    ap.add_argument('--facet-copybooks',action='store_true',help='List top copybook_name facet values')
    ap.add_argument('--facet-programs',action='store_true',help='List top program_id facet values')
    ap.add_argument('--expand-aliases',action='store_true',help='Expand provided program/copybook through alias resolver to canonical set')
    args=ap.parse_args()
    load_settings()
    ep,key=resolve()

    if args.facet_copybooks:
        resp=search(ep,key,{'search':'*','facets':['copybook_name,count:50'],'top':0})
        print(json.dumps({'facet':'copybook_name','values':resp.get('@search.facets',{}).get('copybook_name',[])},indent=2))
    if args.facet_programs:
        resp=search(ep,key,{'search':'*','facets':['program_id,count:50'],'top':0})
        print(json.dumps({'facet':'program_id','values':resp.get('@search.facets',{}).get('program_id',[])},indent=2))

    resolver=None
    if args.expand_aliases and (args.copybook or args.program):
        resolver=AliasResolver(preload=False)
    if args.copybook:
        if resolver:
            canons=resolver.resolve([args.copybook],'COPYBOOK')
            filt=' or '.join([f"copybook_name eq '{c}'" for c in sorted(canons)])
            body={'search':'*','filter':f"({filt})",'select':'program_id,copybook_name,line_number','top':args.top}
            resp=search(ep,key,body)
            progs=sorted({v['program_id'] for v in resp.get('value',[])})
            print(json.dumps({'query':'copybook','input':args.copybook,'resolved':sorted(canons),'program_ids':progs,'occurrence_count':len(resp.get('value',[]))},indent=2))
        else:
            body={'search':'*','filter':f"copybook_name eq '{args.copybook.upper()}'",'select':'program_id,copybook_name,line_number','top':args.top}
            resp=search(ep,key,body)
            progs=sorted({v['program_id'] for v in resp.get('value',[])})
            print(json.dumps({'query':'copybook','copybook':args.copybook.upper(),'program_ids':progs,'occurrence_count':len(resp.get('value',[]))},indent=2))
    if args.program:
        if resolver:
            canons=resolver.resolve([args.program],'PROGRAM')
            filt=' or '.join([f"program_id eq '{c}'" for c in sorted(canons)])
            body={'search':'*','filter':f"({filt})",'select':'program_id,copybook_name,line_number','top':args.top}
            resp=search(ep,key,body)
            cbs=sorted({v['copybook_name'] for v in resp.get('value',[])})
            print(json.dumps({'query':'program','input':args.program,'resolved':sorted(canons),'copybooks':cbs,'occurrence_count':len(resp.get('value',[]))},indent=2))
        else:
            body={'search':'*','filter':f"program_id eq '{args.program.upper()}'",'select':'program_id,copybook_name,line_number','top':args.top}
            resp=search(ep,key,body)
            cbs=sorted({v['copybook_name'] for v in resp.get('value',[])})
            print(json.dumps({'query':'program','program':args.program.upper(),'copybooks':cbs,'occurrence_count':len(resp.get('value',[]))},indent=2))

if __name__=='__main__':
    main()
