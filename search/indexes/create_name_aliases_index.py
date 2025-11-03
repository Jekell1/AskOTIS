"""Create Azure AI Search index: new_cobol_name_aliases

Fields:
  alias_id (key) - deterministic hash of canonical_name|alias|kind
  canonical_name - canonical normalized form (searchable/filterable)
  alias          - one variant spelling / punctuation / case / raw form
  variant_type   - classification of transformation (raw, upper, no_hyphen, no_underscore, collapsed, other)
  kind           - PROGRAM | COPYBOOK | PARAGRAPH
  source_hint    - provenance hint (e.g., program file path or index origin)
  ingested_at    - timestamp

Run:
  python search/indexes/create_name_aliases_index.py
"""
from __future__ import annotations
import os, json, hashlib, requests, sys

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_name_aliases'

SCHEMA = {
  "name": INDEX,
  "fields": [
    {"name":"alias_id","type":"Edm.String","key":True,"filterable":True,"sortable":False,"facetable":False,"searchable":False},
    {"name":"canonical_name","type":"Edm.String","searchable":True,"filterable":True,"sortable":True,"facetable":True},
    {"name":"alias","type":"Edm.String","searchable":True,"filterable":True,"sortable":False,"facetable":False},
    {"name":"variant_type","type":"Edm.String","searchable":False,"filterable":True,"sortable":False,"facetable":True},
    {"name":"kind","type":"Edm.String","searchable":False,"filterable":True,"sortable":False,"facetable":True},
    {"name":"source_hint","type":"Edm.String","searchable":True,"filterable":False,"sortable":False,"facetable":False},
    {"name":"canonical_occurrences","type":"Edm.Int32","searchable":False,"filterable":True,"sortable":True,"facetable":False},
    {"name":"alias_occurrences","type":"Edm.Int32","searchable":False,"filterable":True,"sortable":True,"facetable":False},
    {"name":"ingested_at","type":"Edm.DateTimeOffset","filterable":True,"sortable":True}
  ],
  "corsOptions": {"allowedOrigins": ["*"], "maxAgeInSeconds": 60}
}


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
        print('[FATAL] Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def delete_if_exists(ep,key):
    r=requests.delete(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key})
    if r.status_code in (200,204):
        print('[INFO] Deleted existing index')

def create(ep,key):
    r=requests.put(f"{ep}/indexes/{INDEX}?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=SCHEMA)
    if r.status_code not in (200,201):
        print('[ERROR] Create failed',r.status_code,r.text[:500]); sys.exit(1)
    print('[OK] Created index', INDEX)

if __name__=='__main__':
    load_settings(); ep,key=resolve()
    delete_if_exists(ep,key)
    create(ep,key)
