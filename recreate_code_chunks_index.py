#!/usr/bin/env python3
"""Recreate the code-chunks index with overlap-aware metadata fields.

This will DELETE the existing index named 'code-chunks' and create a clean replacement
including new fields:
    window_index (Int32)
  window_size (Int32)
  stride (Int32)
  created_at (String)
  tokens_estimate (Int32)

Vector field:
  text_vector (Collection(Edm.Single), 3072 dims) using HNSW profile vec-default.

USAGE (PowerShell examples):
  pwsh -Command "python .\recreate_code_chunks_index.py --confirm"
  # Then reingest with overlap (example window=60 stride=30):
  pwsh -Command "python .\ingest_code_chunks.py --window 60 --stride 30 --stream-batch-size 1200 --embed-batch-size 64 --verbose"

Optional arguments:
  --endpoint / --key : override automatic discovery from env or local.settings.json
  --confirm          : required to actually perform delete + create
  --dry-run          : show intended payload only

Safety:
  You MUST pass --confirm or nothing is deleted.

Exit codes:
    0 success, non-zero on error.
"""
import os, json, argparse, sys, requests
from typing import Dict, Any

API_VERSION = "2024-07-01"
INDEX_NAME = "code-chunks"
INDEX_NAME = "new_code_chunks"
VECTOR_DIM = 3072

# ------------------ helpers ------------------

def load_settings():
    try:
        data = json.load(open('local.settings.json','r', encoding='utf-8'))
        return data.get('Values', {})
    except Exception:
        return {}

def resolve_credentials(endpoint_arg, key_arg):
    vals = load_settings()
    endpoint = endpoint_arg or os.environ.get('AZURE_SEARCH_ENDPOINT') or os.environ.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key = key_arg or os.environ.get('AZURE_SEARCH_KEY') or os.environ.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing search endpoint/key (set AZURE_SEARCH_ENDPOINT & AZURE_SEARCH_KEY)')
    return endpoint.rstrip('/'), key

def index_exists(endpoint: str, key: str, name: str) -> bool:
    url = f"{endpoint}/indexes/{name}?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code == 200:
        return True
    if r.status_code == 404:
        return False
    raise SystemExit(f"Failed to check index existence {r.status_code}: {r.text[:300]}")

def delete_index(endpoint: str, key: str, name: str):
    url = f"{endpoint}/indexes/{name}?api-version={API_VERSION}"
    r = requests.delete(url, headers={'api-key': key})
    if r.status_code not in (200, 202, 204, 404):
        raise SystemExit(f"Delete failed {r.status_code}: {r.text[:300]}")


def build_index_payload() -> Dict[str, Any]:
    return {
        'name': INDEX_NAME,
        'fields': [
            {'name': 'chunk_id','type': 'Edm.String','key': True},
            {'name': 'file_id','type': 'Edm.String','filterable': True,'facetable': True},
            {'name': 'path','type': 'Edm.String','searchable': True,'filterable': True},
            {'name': 'program_id','type': 'Edm.String','filterable': True,'facetable': True},
            {'name': 'scope','type': 'Edm.String','filterable': True,'facetable': True},
            {'name': 'name','type': 'Edm.String','searchable': True,'filterable': True},
            {'name': 'start_line','type': 'Edm.Int32','filterable': True,'facetable': True,'sortable': True},
            {'name': 'end_line','type': 'Edm.Int32','filterable': True,'facetable': True,'sortable': True},
            {'name': 'text','type': 'Edm.String','searchable': True},
            {'name': 'text_vector','type': 'Collection(Edm.Single)','searchable': True,'dimensions': VECTOR_DIM,'vectorSearchProfile': 'vec-default'},
            {'name': 'has_vector','type': 'Edm.Boolean','filterable': True,'facetable': True},
            # New overlap / provenance fields
            {'name': 'window_index','type': 'Edm.Int32','filterable': True,'facetable': True,'sortable': True},
            {'name': 'window_size','type': 'Edm.Int32','filterable': True,'facetable': True,'sortable': True},
            {'name': 'stride','type': 'Edm.Int32','filterable': True,'facetable': True,'sortable': True},
            {'name': 'created_at','type': 'Edm.String','filterable': True,'sortable': True},
            {'name': 'tokens_estimate','type': 'Edm.Int32','filterable': True,'sortable': True},
        ],
        'suggesters': [
            {'name': 'sg','searchMode': 'analyzingInfixMatching','sourceFields': ['name','path','program_id']}
        ],
        'vectorSearch': {
            'algorithms': [{'name': 'hnsw','kind': 'hnsw'}],
            'profiles': [{'name': 'vec-default','algorithm': 'hnsw'}]
        }
    }

def create_index(endpoint: str, key: str, payload: Dict[str, Any]):
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload)
    if r.status_code not in (200, 201):
        raise SystemExit(f"Create failed {r.status_code}: {r.text[:300]}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--confirm', action='store_true', help='Actually perform delete & recreate')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    endpoint, key = resolve_credentials(args.endpoint, args.key)
    payload = build_index_payload()

    if args.dry_run:
        print('[DRY-RUN] Would recreate index with payload:')
        print(json.dumps(payload, indent=2)[:4000])
        return

    exists = index_exists(endpoint, key, INDEX_NAME)
    if exists:
        if not args.confirm:
            print(f"Index '{INDEX_NAME}' exists. Re-run with --confirm to delete & recreate.")
            return
        print(f"Deleting existing index '{INDEX_NAME}'...")
        delete_index(endpoint, key, INDEX_NAME)
    else:
        if not args.confirm:
            print(f"Index '{INDEX_NAME}' does not exist. Re-run with --confirm to create.")
            return

    print(f"Creating index '{INDEX_NAME}'...")
    create_index(endpoint, key, payload)
    print('Index recreation complete.')

if __name__ == '__main__':
    main()
