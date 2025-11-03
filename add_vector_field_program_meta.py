"""Add summary_vector + has_vector fields to new_cobol_program_meta index (idempotent).

Usage:
  python add_vector_field_program_meta.py

Adds vector fields for embedding program_summary text if not already present.
Follows same pattern as add_vector_field_screen_nodes.py.
"""
from __future__ import annotations
import os, json, argparse, requests, sys

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'
VECTOR_DIM = int(os.getenv('PROGRAM_META_VECTOR_DIM','3072'))

def load_local_settings():
    try:
        data=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in data.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key. Provide --endpoint/--key or set env vars.', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def get_current_schema(ep: str, key: str) -> dict:
    url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
    resp = requests.get(url, headers={'api-key': key})
    if resp.status_code != 200:
        print(f"Failed to get index schema: {resp.status_code} {resp.text}")
        sys.exit(1)
    return resp.json()

def has_vector_fields(schema: dict) -> bool:
    field_names = {f['name'] for f in schema.get('fields', [])}
    return 'summary_vector' in field_names and 'has_vector' in field_names

def add_vector_fields_to_schema(schema: dict) -> dict:
    fields = schema['fields']
    
    # Check existing vectorSearch config to use correct profile name
    vector_search = schema.get('vectorSearch') or {}
    profiles = vector_search.get('profiles', [])
    profile_name = 'vector-profile'  # default
    if profiles:
        profile_name = profiles[0]['name']  # use first available profile
    
    # Add summary_vector field
    vector_field = {
        "name": "summary_vector",
        "type": "Collection(Edm.Single)",
        "searchable": True,
        "dimensions": VECTOR_DIM,
        "vectorSearchProfile": profile_name
    }
    
    # Add has_vector boolean field
    has_vector_field = {
        "name": "has_vector",
        "type": "Edm.Boolean",
        "filterable": True,
        "facetable": True
    }
    
    # Insert before the last field (typically updated_at)
    fields.insert(-1, vector_field)
    fields.insert(-1, has_vector_field)
    
    # Ensure vectorSearch configuration exists if none found
    if 'vectorSearch' not in schema:
        schema['vectorSearch'] = {
            "algorithms": [
                {"name": "hnsw-alg", "kind": "hnsw"}
            ],
            "profiles": [
                {"name": "vector-profile", "algorithm": "hnsw-alg"}
            ]
        }
    
    return schema

def update_index_schema(ep: str, key: str, schema: dict):
    url = f"{ep}/indexes/{INDEX}?api-version={API_VERSION}"
    resp = requests.put(url, json=schema, headers={'api-key': key, 'Content-Type': 'application/json'})
    if resp.status_code not in [200, 201]:
        print(f"Failed to update index schema: {resp.status_code} {resp.text}")
        sys.exit(1)
    print('Updated program_meta schema with vector field...')

def main():
    ap = argparse.ArgumentParser(description='Add vector fields to program meta index')
    ap.add_argument('--endpoint', help='Azure Search endpoint')
    ap.add_argument('--key', help='Azure Search admin key')
    args = ap.parse_args()
    
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    
    schema = get_current_schema(ep, key)
    
    if has_vector_fields(schema):
        print(f"Index {INDEX} already has vector fields. No update needed.")
        return
    
    print(f"Adding vector fields to {INDEX}...")
    updated_schema = add_vector_fields_to_schema(schema)
    update_index_schema(ep, key, updated_schema)
    print('Vector fields added successfully.')

if __name__ == '__main__':
    main()