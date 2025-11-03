#!/usr/bin/env python3
"""create_cobol_file_index.py

Create (or idempotently ensure) an Azure AI Search index to store full COBOL program
files as single documents.

Index name: cobol-files-v1

Schema Goals:
  * One document per source file (.CBL, .CB2, .CPY optional) – raw full_text preserved.
  * Filterable program_name, path, size (length_lines)
  * Optional vector field (full_text_vector) for semantic retrieval of entire file
    (embedding dimension taken from env FACT_FILE_EMBED_DIM or defaults to 1536)
  * has_vectors flag to allow gradual backfill

Prereqs (env):
  SEARCH_ENDPOINT, SEARCH_KEY

Usage:
  python create_cobol_file_index.py

Safe to re-run; will patch missing fields if index exists (additive only).
"""
import os, sys, json, requests
from typing import List, Dict

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-05-01-preview')
INDEX_NAME = os.getenv('COBOL_FILE_INDEX','cobol-files-v1')

BASE_FIELDS = [
    {"name": "file_id", "type": "Edm.String", "key": True, "filterable": True, "sortable": False, "facetable": False, "searchable": False},
    {"name": "program_name", "type": "Edm.String", "filterable": True, "facetable": True, "sortable": True, "searchable": True},
    {"name": "path", "type": "Edm.String", "filterable": True, "facetable": False, "sortable": False, "searchable": True},
    {"name": "length_lines", "type": "Edm.Int32", "filterable": True, "sortable": True, "facetable": False, "searchable": False},
    {"name": "file_hash", "type": "Edm.String", "filterable": True, "searchable": False},
    {"name": "ingest_timestamp", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True, "searchable": False},
    {"name": "has_vectors", "type": "Edm.Boolean", "filterable": True, "searchable": False},
    # Removed analyzerName for API compatibility (2024-07-01 stable). Default analyzer will be applied.
    {"name": "full_text", "type": "Edm.String", "searchable": True, "filterable": False, "sortable": False, "facetable": False},
]

# Vector field configuration (optional)
VECTOR_PROFILE_NAME = os.getenv('COBOL_FILE_VECTOR_PROFILE','file-prof')
VECTOR_ALGO_NAME = os.getenv('COBOL_FILE_VECTOR_ALGO','file-hnsw')
VECTOR_DIM = int(os.getenv('FACT_FILE_EMBED_DIM','1536'))

VECTOR_FIELD = {
    "name": "full_text_vector",
    "type": "Collection(Edm.Single)",
    "dimensions": VECTOR_DIM,
    "vectorSearchProfile": VECTOR_PROFILE_NAME
}


def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
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


def patch_missing_fields(ep: str, key: str, existing: dict):
    existing_fields = {f['name'] for f in existing.get('fields',[])}
    patch = []
    # add any missing base fields
    for f in BASE_FIELDS:
        if f['name'] not in existing_fields:
            patch.append(f)
    # add vector field if missing
    if VECTOR_FIELD['name'] not in existing_fields:
        patch.append(VECTOR_FIELD)
    if not patch:
        print("Index already up-to-date.")
        return
    # Azure Search does not support partial field adds via simple PATCH; must recreate or ignore.
    # Conservative approach: warn user.
    print("WARNING: Index exists and is missing fields that cannot be patched in-place. Consider re-creating manually:")
    for f in patch:
        print("  - missing field:", f['name'])


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
    # If RECREATE env flag set, delete first
    if os.getenv('RECREATE','0') in ('1','true','yes'):
        delete_index(ep, key)
    existing = get_existing_index(ep, key, INDEX_NAME)
    if existing is not None:
        print(f"Index {INDEX_NAME} already exists; delete it first (set RECREATE=1) to rebuild.")
        return
    create_index(ep, key)

if __name__ == '__main__':
    main()
