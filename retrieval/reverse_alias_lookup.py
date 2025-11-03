"""Reverse alias lookup: given canonical_name(s) return all alias variants grouped.

Usage:
  python retrieval/reverse_alias_lookup.py --canonical TIM360 --kind PROGRAM
"""
from __future__ import annotations
import os,json,requests,argparse,sys

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
    r=requests.post(f"{ep}/indexes/{ALIAS_INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Search failed',r.status_code,r.text[:300]); sys.exit(1)
    return r.json()

def query_aliases(ep,key,canon_list,kind):
    acc={}
    for canon in canon_list:
        filt=f"canonical_name eq '{canon}'"
        if kind:
            filt += f" and kind eq '{kind}'"
        data=search(ep,key,{'search':'*','filter':filt,'top':1000,'select':'canonical_name,alias,variant_type,kind,canonical_occurrences,alias_occurrences'})
        rows=data.get('value',[])
        acc[canon]=rows
    return acc

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--canonical',nargs='+',required=True)
    ap.add_argument('--kind')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    data=query_aliases(ep,key,args.canonical,args.kind)
    print(json.dumps({'results':data},indent=2))

if __name__=='__main__':
    main()
