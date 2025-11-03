"""Diagnose remaining data item docs lacking symbol_id.

Finds up to a limit of docs where symbol_id is null, categorizes reasons:
  - missing_program_id
  - missing_path
  - both_missing
  - has_fields (should have been set -> maybe race)

Usage:
  python diagnose_unset_symbol_id_data_items.py --sample 2000
"""
from __future__ import annotations
import os, sys, json, argparse, requests, collections

INDEX='new_cobol_data_items'
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=str(v)
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_batch(ep,key,remaining):
    top=min(remaining,1000)
    body={'search':'*','filter':'symbol_id eq null','top':top,'select':'item_id,program_id,path'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--sample',type=int,default=2000,help='Max docs to pull for diagnosis')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    remaining=args.sample
    counts=collections.Counter()
    examples={'missing_program_id':[], 'missing_path':[], 'both_missing':[], 'has_fields':[]}
    total_examined=0
    while remaining>0:
        rows=fetch_batch(ep,key,remaining)
        if not rows: break
        for r in rows:
            total_examined+=1
            pid=bool(r.get('program_id'))
            path=bool(r.get('path'))
            if not pid and not path:
                counts['both_missing']+=1
                if len(examples['both_missing'])<5: examples['both_missing'].append(r)
            elif not pid:
                counts['missing_program_id']+=1
                if len(examples['missing_program_id'])<5: examples['missing_program_id'].append(r)
            elif not path:
                counts['missing_path']+=1
                if len(examples['missing_path'])<5: examples['missing_path'].append(r)
            else:
                counts['has_fields']+=1
                if len(examples['has_fields'])<5: examples['has_fields'].append(r)
        remaining-=len(rows)
        if len(rows)<1000: # likely exhausted
            break
    report={
        'examined': total_examined,
        'categories': counts,
        'examples': examples
    }
    print(json.dumps(report, indent=2))

if __name__=='__main__':
    main()
