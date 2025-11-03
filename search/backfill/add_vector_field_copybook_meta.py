"""Ensure has_vector boolean exists for new_cobol_copybook_meta (summary_vector already created)."""
from __future__ import annotations
import os, sys, json, requests

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_meta'
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
    if r.status_code!=200: print('Fetch failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()

def has_field(data):
    return any(f.get('name')==HAS_FIELD for f in data.get('fields',[]))

def update(ep,key,data):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=data)
    if r.status_code not in (200,201):
        print('Update failed', r.status_code, r.text[:600]); sys.exit(1)
    print('Index updated.')

if __name__=='__main__':
    load(); ep,key=resolve(); data=fetch(ep,key)
    if not has_field(data):
        data['fields'].append({'name':HAS_FIELD,'type':'Edm.Boolean','searchable':False,'filterable':True,'facetable':True,'retrievable':True})
        print('Adding has_vector field...')
        update(ep,key,data)
    else:
        print('has_vector already present.')
