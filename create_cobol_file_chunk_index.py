#!/usr/bin/env python3
"""create_cobol_file_chunk_index.py

Create (or idempotently ensure) an Azure AI Search index to store COBOL file text chunks
for semantic retrieval. Each chunk is a sliding / windowed segment of a COBOL source file.

Index name (default): cobol-file-chunks-v1 (override via COBOL_FILE_CHUNK_INDEX env)

Design Goals:
  * Smaller, token-safe text segments for high-quality embeddings and recall.
  * Preserve ordering with chunk_ordinal and character offsets (start_char, end_char).
  * Enable filtering / aggregation by program_name and file_id.
  * Vector field for chunk_text_vector using same embedding dimension as facts/files
    (env FACT_FILE_EMBED_DIM, default 1536) and dedicated vectorSearch profile.
  * has_vector flag for incremental embedding backfill.

Prereqs (env): SEARCH_ENDPOINT, SEARCH_KEY

Usage:
  python create_cobol_file_chunk_index.py

Safe to re-run; warns (does not mutate) if index already exists (use RECREATE=1 to drop & recreate).
"""
import os, sys, json, requests
from typing import List, Dict

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-05-01-preview')
INDEX_NAME = os.getenv('COBOL_FILE_CHUNK_INDEX','cobol-file-chunks-v1')

VECTOR_DIM = int(os.getenv('FACT_FILE_EMBED_DIM','1536'))
VECTOR_PROFILE_NAME = os.getenv('COBOL_FILE_CHUNK_VECTOR_PROFILE','chunk-prof')
VECTOR_ALGO_NAME = os.getenv('COBOL_FILE_CHUNK_VECTOR_ALGO','chunk-hnsw')

BASE_FIELDS = [
    {"name":"chunk_id","type":"Edm.String","key":True,"filterable":True,"searchable":False},
    {"name":"file_id","type":"Edm.String","filterable":True,"searchable":False},
    {"name":"program_name","type":"Edm.String","filterable":True,"facetable":True,"sortable":True,"searchable":True},
    {"name":"chunk_ordinal","type":"Edm.Int32","filterable":True,"sortable":True,"searchable":False},
    {"name":"start_char","type":"Edm.Int32","filterable":True,"sortable":True,"searchable":False},
    {"name":"end_char","type":"Edm.Int32","filterable":True,"sortable":True,"searchable":False},
    {"name":"ingest_timestamp","type":"Edm.DateTimeOffset","filterable":True,"sortable":True},
    {"name":"has_vector","type":"Edm.Boolean","filterable":True},
    {"name":"chunk_text","type":"Edm.String","searchable":True,"filterable":False,"sortable":False,"facetable":False},
]

VECTOR_FIELD = {
    "name":"chunk_text_vector",
    "type":"Collection(Edm.Single)",
    "dimensions":VECTOR_DIM,
    "vectorSearchProfile":VECTOR_PROFILE_NAME
}


def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k]=v
        except Exception:
            pass

def get_existing_index(ep: str, key: str, name: str):
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key}, timeout=20)
    if r.status_code == 404:
        return None
    if r.status_code >= 300:
        raise SystemExit(f"Error getting index {name}: {r.status_code} {r.text[:300]}")
    return r.json()

def create_index(ep: str, key: str):
    body = {
        "name": INDEX_NAME,
        "fields": BASE_FIELDS + [VECTOR_FIELD],
        "vectorSearch": {
            "algorithms": [
                {"name": VECTOR_ALGO_NAME, "kind": "hnsw"}
            ],
            "profiles": [
                {"name": VECTOR_PROFILE_NAME, "algorithm": VECTOR_ALGO_NAME}
            ]
        }
    }
    r = requests.put(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",
                     headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=40)
    if r.status_code >= 300:
        raise SystemExit(f"Create index failed {r.status_code}: {r.text[:400]}")
    print(f"Created index {INDEX_NAME}")

def delete_index(ep: str, key: str):
    r = requests.delete(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key': key}, timeout=20)
    if r.status_code not in (204,404):
        raise SystemExit(f"Delete failed {r.status_code}: {r.text[:300]}")
    if r.status_code == 204:
        print(f"Deleted index {INDEX_NAME}")
    else:
        print(f"Index {INDEX_NAME} did not exist (nothing to delete)")

def main():
    load_settings()
    ep = os.getenv('SEARCH_ENDPOINT'); key = os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    ep = ep.rstrip('/')
    if os.getenv('RECREATE','0') in ('1','true','yes'):
        delete_index(ep,key)
    existing = get_existing_index(ep,key,INDEX_NAME)
    if existing is not None:
        print(f"Index {INDEX_NAME} already exists; delete it first (set RECREATE=1) to rebuild.")
        return
    create_index(ep,key)

if __name__ == '__main__':
    main()
