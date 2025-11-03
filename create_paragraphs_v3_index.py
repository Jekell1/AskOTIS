#!/usr/bin/env python3
"""create_paragraphs_v3_index.py

Creates a new paragraphs index `cobol-paragraphs-v3` identical to v2 but with an added
`text` field (searchable) to store full paragraph body for richer fact extraction.

Safe to run multiple times; will skip creation if index already exists.
Requires SEARCH_ENDPOINT / SEARCH_KEY (or local.settings.json Values fallback).
"""
import os, json, sys, requests

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
INDEX_NAME = "cobol-paragraphs-v3"

def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def index_schema():
    return {
        "name": INDEX_NAME,
        "fields": [
            {"name":"para_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"file_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"name","type":"Edm.String","searchable":True,"filterable":True},
            {"name":"kind","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"start_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"end_line","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            # New full paragraph text field
            {"name":"text","type":"Edm.String","searchable":True},
            # Enrichment fields
            {"name":"role","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"phase","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"phase_seq","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
            {"name":"role_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"phase_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            # Vector for name
            {"name":"name_vector","type":"Collection(Edm.Single)","searchable":True,"filterable":False,"facetable":False,"sortable":False,"dimensions":3072,"vectorSearchProfile":"vec-default"},
            {"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True},
        ],
        "vectorSearch": {
            "algorithms": [{"name":"hnsw","kind":"hnsw"}],
            "profiles": [{"name":"vec-default","algorithm":"hnsw"}]
        },
        "suggesters": [
            {"name":"sg","searchMode":"analyzingInfixMatching","sourceFields":["name","text"]}
        ]
    }

def main():
    load_settings()
    ep = os.environ.get('SEARCH_ENDPOINT'); key = os.environ.get('SEARCH_KEY')
    if not ep or not key:
        print("Missing SEARCH_ENDPOINT/SEARCH_KEY", file=sys.stderr); sys.exit(2)
    # Check if exists
    url = f"{ep.rstrip('/')}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 200:
        print(f"Index {INDEX_NAME} already exists; skipping creation")
        return
    if r.status_code != 404:
        print(f"Unexpected status checking index: {r.status_code} {r.text[:300]}")
    # Create
    urlc = f"{ep.rstrip('/')}/indexes?api-version={API_VERSION}"
    schema = index_schema()
    rc = requests.post(urlc, headers={"api-key": key, "Content-Type":"application/json"}, json=schema, timeout=120)
    if rc.status_code >= 300:
        print(f"Create failed {rc.status_code}: {rc.text[:500]}")
        sys.exit(3)
    print(f"Created index {INDEX_NAME}")

if __name__ == '__main__':
    main()
