"""Create the new_cobol_copybook_usage index.

One document per COPY statement occurrence (usage) inside a COBOL program.
Fields support dependency analysis, usage enumeration, and semantic retrieval.

Usage:
  python create_copybook_usage_index.py
"""
from __future__ import annotations
import os, json, sys, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_copybook_usage'
VECTOR_DIM = 3072

def load_local_settings():
    try:
        data = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in data and k not in os.environ:
                os.environ[k] = data[k]
    except Exception:
        pass

def endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key env vars')
        sys.exit(1)
    return ep.rstrip('/'), key

def index_payload():
    return {
        'name': INDEX_NAME,
        'vectorSearch': {
            # Simplified algorithm declaration (API version does not accept nested 'parameters').
            'algorithms': [
                {
                    'name':'vector_hnsw',
                    'kind':'hnsw'
                }
            ],
            'profiles': [{'name':'vprofile','algorithm':'vector_hnsw'}]
        },
        # Semantic config omitted due to API schema mismatch; can be added later if required.
        'fields': [
            {'name':'usage_id','type':'Edm.String','key':True,'searchable':False,'filterable':True,'sortable':False,'facetable':False},
            {'name':'program_id','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'program_name','type':'Edm.String','searchable':True,'filterable':False},
            # copybook_name retains original extension (e.g., SCREEN.CPY) for acceptance filtering.
            {'name':'copybook_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            # normalized_copybook_name strips extension and path for joins / grouping.
            {'name':'normalized_copybook_name','type':'Edm.String','searchable':True,'filterable':False},
            # Plain copybook name (no path) preserving extension for simpler user filtering.
            {'name':'copybook_name_plain','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'section','type':'Edm.String','searchable':False,'filterable':True,'facetable':True},
            {'name':'paragraph_name','type':'Edm.String','searchable':True,'filterable':True,'facetable':False},
            {'name':'line_start','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'line_end','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'inclusion_order','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'file_path','type':'Edm.String','searchable':False,'filterable':True},
            {'name':'line_number','type':'Edm.Int32','filterable':True,'sortable':True},
            {'name':'raw_copy_line','type':'Edm.String','searchable':True},
            {'name':'context_snippet','type':'Edm.String','searchable':True},
            {'name':'expansion_present','type':'Edm.Boolean','filterable':True},
            # Added to capture presence of REPLACING clause on COPY usage
            {'name':'has_replacing_clause','type':'Edm.Boolean','filterable':True},
            {'name':'program_classification','type':'Edm.String','filterable':True,'facetable':True},
            {'name':'ingested_at','type':'Edm.DateTimeOffset','filterable':True,'sortable':True},
            {'name':'has_vector','type':'Edm.Boolean','filterable':True},
            {'name':'context_vector','type':'Collection(Edm.Single)','searchable':True,'dimensions':VECTOR_DIM,'vectorSearchProfile':'vprofile'}
        ]
    }

def create_index():
    load_local_settings()
    ep, key = endpoint_key()
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    # Delete if exists
    r = requests.delete(url, headers={'api-key':key})
    if r.status_code in (200,204):
        print('[INFO] Deleted existing index')
    payload = index_payload()
    r = requests.put(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=90)
    if r.status_code not in (200,201):
        print('[FATAL] Failed to create index', r.status_code, r.text[:500])
        sys.exit(1)
    print('[OK] Created index', INDEX_NAME)

if __name__ == '__main__':
    create_index()
