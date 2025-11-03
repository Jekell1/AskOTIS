"""Add/ensure vector + has_vector fields for new_cobol_program_meta.

Adds program_summary_vector (3072 dims) and has_vector (Edm.Boolean) if missing.
Safe to re-run (idempotent). Uses PUT full index update pattern.
"""
from __future__ import annotations
import os, sys, json, requests

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'
VEC_FIELD='program_summary_vector'
DIM=3072
HAS_FIELD='has_vector'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key})
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()

def ensure_vector_search(data):
    vs=data.setdefault('vectorSearch',{})
    algs=vs.setdefault('algorithms',[])
    if not any(a.get('name')=='hnsw-alg' for a in algs):
        algs.append({'name':'hnsw-alg','kind':'hnsw'})
    prof=vs.setdefault('profiles',[])
    if not any(p.get('name')=='vprofile' for p in prof):
        prof.append({'name':'vprofile','algorithm':'hnsw-alg'})

def field_present(data,name):
    return any(f.get('name')==name for f in data.get('fields',[]))

def add_fields(data):
    changed=False
    if not field_present(data,VEC_FIELD):
        data['fields'].append({'name':VEC_FIELD,'type':'Collection(Edm.Single)','searchable':True,'dimensions':DIM,'vectorSearchProfile':'vprofile','retrievable':False})
        changed=True
    if not field_present(data,HAS_FIELD):
        data['fields'].append({'name':HAS_FIELD,'type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True})
        changed=True
    return changed

def update(ep,key,data):
    ensure_vector_search(data)
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=data)
    if r.status_code not in (200,201,204):
        print('Update failed', r.status_code, r.text[:600]); sys.exit(1)
    print(f'Index updated (status {r.status_code}).')

if __name__=='__main__':
    load(); ep,key=resolve(); data=fetch(ep,key)
    if add_fields(data):
        print('Adding missing fields...')
        update(ep,key,data)
    else:
        print('Fields already present (no changes).')
