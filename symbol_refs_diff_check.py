"""Diff-check prospective symbol_refs ingestion JSONL against existing index.

Usage:
  python symbol_refs_diff_check.py --input new_refs.jsonl --sample 1000

Outputs counts: existing, new, duplicates, and writes a JSON file (optional) of only new docs.
"""
from __future__ import annotations
import os, json, argparse, requests, hashlib

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'

def load():
    try:
        vals=json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def iter_input(path):
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip();
            if not line: continue
            try: yield json.loads(line)
            except Exception: continue

def fetch_existing(ep,key,ref_ids):
    out=set(); ids=list(ref_ids)
    for i in range(0,len(ids),20):
        batch=ids[i:i+20]
        filt=' or '.join([f"ref_id eq '{rid}'" for rid in batch])
        body={'search':'*','filter':filt,'top':len(batch),'select':'ref_id'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
        if r.status_code==200:
            for v in r.json().get('value',[]):
                out.add(v.get('ref_id'))
    return out

def main():
    ap=argparse.ArgumentParser(description='Symbol refs diff checker')
    ap.add_argument('--input',required=True)
    ap.add_argument('--sample',type=int,default=0,help='Limit number of lines considered')
    ap.add_argument('--out-new')
    args=ap.parse_args(); load(); ep,key=resolve()
    candidates=[]; ref_ids=[]
    for i,obj in enumerate(iter_input(args.input)):
        if args.sample and i>=args.sample: break
        rid=obj.get('ref_id')
        if rid:
            ref_ids.append(rid)
            candidates.append(obj)
    existing=fetch_existing(ep,key,ref_ids)
    new=[c for c in candidates if c.get('ref_id') not in existing]
    print(f"Total candidates={len(candidates)} existing={len(existing)} new={len(new)} duplicates={len(candidates)-len(new)}")
    if args.out_new:
        with open(args.out_new,'w',encoding='utf-8') as f:
            for obj in new:
                f.write(json.dumps(obj)+'\n')
        print('Wrote new-only JSONL',args.out_new)

if __name__=='__main__':
    main()
