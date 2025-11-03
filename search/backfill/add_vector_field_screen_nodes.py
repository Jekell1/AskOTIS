"""Idempotently ensure summary_vector field exists on new_cobol_screen_nodes.

Since the index is created with the field, this usually just validates schema.
"""
from __future__ import annotations
import os, sys, json, requests, argparse

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_screen_nodes'

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve(args):
    ep=args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def get_schema(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code==200: return r.json()
    print('[FATAL] Index not found'); sys.exit(1)

def update(ep,key,schema):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=schema)
    if r.status_code not in (200,201):
        print('[FATAL] Update failed',r.status_code,r.text[:300]); sys.exit(1)
    print('Updated index schema (added summary_vector)')

def main():
    ap=argparse.ArgumentParser(description='Ensure summary_vector field exists for screen nodes index')
    ap.add_argument('--endpoint'); ap.add_argument('--key')
    args=ap.parse_args(); load_local(); ep,key=resolve(args)
    schema=get_schema(ep,key)
    fields=schema.get('fields',[])
    if any(f.get('name')=='summary_vector' for f in fields):
        print('summary_vector field present (ok)'); return
    fields.append({'name':'summary_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':3072,'vectorSearchProfile':'vprofile'})
    if 'vectorSearch' not in schema:
        schema['vectorSearch']={'algorithms':[{'name':'hnsw-alg','kind':'hnsw'}],'profiles':[{'name':'vprofile','algorithm':'hnsw-alg'}]}
    update(ep,key,schema)

if __name__=='__main__':
    main()
