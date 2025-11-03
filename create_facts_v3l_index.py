#!/usr/bin/env python3
"""create_facts_v3l_index.py

Create a new facts index `cobol-facts-v3l` with 3072-dimension vector field to align with
Azure OpenAI embedding deployment `text-embedding-3-large`.

Safe to run multiple times (skips if exists).
Requires SEARCH_ENDPOINT / SEARCH_KEY (or local.settings.json fallback).
"""
import os, json, sys, requests

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
INDEX_NAME = "cobol-facts-v3l"
VECTOR_DIMENSIONS = 3072
VECTOR_PROFILE = "facts-profile"
VECTOR_ALGO = "facts-hnsw"

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
            {"name":"fact_id","type":"Edm.String","key":True,"filterable":True,"sortable":True},
            {"name":"program_id","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"action_role","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"posting_type","type":"Edm.String","filterable":True,"facetable":True},
            {"name":"gating_cond","type":"Edm.String","searchable":True},
            {"name":"fact_confidence","type":"Edm.Double","filterable":True,"sortable":True},
            {"name":"sources","type":"Collection(Edm.String)","filterable":False,"searchable":False},
            {"name":"fact_text","type":"Edm.String","searchable":True},
            {"name":"fact_vector","type":"Collection(Edm.Single)","searchable":True,"dimensions":VECTOR_DIMENSIONS,"filterable":False,"facetable":False,"sortable":False,"vectorSearchProfile":VECTOR_PROFILE},
            {"name":"has_vector","type":"Edm.Boolean","filterable":True,"facetable":True}
        ],
        "vectorSearch": {
            "algorithms": [
                {"name": VECTOR_ALGO, "kind": "hnsw", "hnswParameters": {"m":4, "efConstruction":400, "metric":"cosine"}}
            ],
            "profiles": [
                {"name": VECTOR_PROFILE, "algorithm": VECTOR_ALGO}
            ]
        }
    }

def main():
    load_settings()
    ep = os.environ.get('SEARCH_ENDPOINT'); key = os.environ.get('SEARCH_KEY')
    if not ep or not key:
        print("Missing SEARCH_ENDPOINT/SEARCH_KEY", file=sys.stderr); sys.exit(2)
    url = f"{ep.rstrip('/')}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 200:
        print(f"Index {INDEX_NAME} already exists; skipping creation")
        return
    if r.status_code != 404:
        print(f"Unexpected status checking index: {r.status_code} {r.text[:300]}")
    schema = index_schema()
    urlc = f"{ep.rstrip('/')}/indexes?api-version={API_VERSION}"
    rc = requests.post(urlc, headers={"api-key": key, "Content-Type":"application/json"}, json=schema, timeout=120)
    if rc.status_code >= 300:
        print(f"Create failed {rc.status_code}: {rc.text[:500]}")
        sys.exit(3)
    print(f"Created index {INDEX_NAME}")

if __name__ == '__main__':
    main()
