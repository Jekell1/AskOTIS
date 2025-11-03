"""Add vector embedding field to existing new_cobol_program_deps index.

Adds:
  * dependency_blob_vector (Collection(Edm.Single), 3072 dims)
  * has_vector (Boolean) convenience flag
  * vectorSearch configuration (hnsw + profile) if missing

Idempotent: re-running will detect existing fields and skip changes.

Usage:
  python add_vector_fields_program_deps.py
Then run:
  python backfill_embeddings_program_deps.py
"""
from __future__ import annotations
import os, sys, json, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_deps'
VECTOR_FIELD = 'dependency_blob_vector'
HAS_FIELD = 'has_vector'
DIM = int(os.getenv('PROGRAM_DEPS_VECTOR_DIM','3072'))


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY',
                  'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_schema(ep,key):
    r = requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code != 200:
        print('Fetch schema failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json()


def save_schema(ep,key,schema):
    r = requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=schema)
    if r.status_code not in (200,201):
        print('Update schema failed', r.status_code, r.text[:400]); sys.exit(1)


def ensure_vector_config(schema):
    vs = schema.get('vectorSearch')
    if not isinstance(vs, dict):
        schema['vectorSearch'] = vs = {}
    algos = vs.setdefault('algorithms', [])
    if not any(a.get('name')=='hnsw-alg' for a in algos):
        algos.append({'name':'hnsw-alg','kind':'hnsw'})
    profiles = vs.setdefault('profiles', [])
    if not any(p.get('name')=='vprofile' for p in profiles):
        profiles.append({'name':'vprofile','algorithm':'hnsw-alg'})
    return schema


def ensure_fields(schema):
    fields = schema.get('fields', [])
    names = {f.get('name') for f in fields}
    changed = False
    if VECTOR_FIELD not in names:
        fields.append({
            'name': VECTOR_FIELD,
            'type': 'Collection(Edm.Single)',
            'searchable': True,
            'dimensions': DIM,
            'vectorSearchProfile': 'vprofile',
            'retrievable': False
        })
        changed = True
    if HAS_FIELD not in names:
        fields.append({
            'name': HAS_FIELD,
            'type': 'Edm.Boolean',
            'filterable': True,
            'facetable': True,
            'searchable': False
        })
        changed = True
    schema['fields'] = fields
    return changed

if __name__ == '__main__':
    load_settings(); ep,key = resolve()
    schema = fetch_schema(ep,key)
    before = json.dumps(schema, sort_keys=True)
    ensure_vector_config(schema)
    changed_fields = ensure_fields(schema)
    after = json.dumps(schema, sort_keys=True)
    if before == after:
        print('No schema changes required (vector field already present).')
        sys.exit(0)
    print('Updating index schema with vector field ...')
    save_schema(ep,key,schema)
    print('Schema updated. Run backfill_embeddings_program_deps.py next.')
