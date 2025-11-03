"""Create the new_cobol_program_deps index.

Purpose: ultra-fast one-shot dependency answers like:
  - "What does APIPAY call?" (outgoing programs, screens, externals, copybooks)
  - "What depends on LONPF2?" (incoming programs)

We pre-aggregate per program linking data scattered across:
  * new_cobol_calls (call edges caller_program -> callee_program)
  * new_cobol_copybook_usage (program_id -> copybook_name occurrences)
  * new_cobol_program_meta (program_role, ui_flag, reach sizes, depth, etc.)

Document granularity: 1 document per program_id.

Fields (minimal / denormalized):
  program_id (key)
  program_role (from meta)
  outgoing_programs_json (JSON string list of distinct callee program IDs)
  incoming_programs_json (JSON string list of distinct caller program IDs)
  external_programs_json (JSON string list of called names not present as program_id in corpus)
  copybooks_used_json (JSON string list of distinct copybook names)
  screens_touched_json (JSON string list of distinct outgoing UI programs)
  outgoing_count / incoming_count / copybook_count / external_count / screens_count (ints)
  has_outgoing / has_incoming (bool)
  dependency_blob (searchable concatenation for simple text queries)
  updated_at (ISO timestamp)

No vector field is added initially: retrieval is exact by key or simple text search.
Add vector later if semantic expansion needed.

Usage:
  python create_program_deps_index.py --force

"""
from __future__ import annotations
import os, json, argparse, sys, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX_NAME = 'new_cobol_program_deps'


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key (provide --endpoint/--key or set env).', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def index_schema():
    return {
        'name': INDEX_NAME,
        'fields': [
            {'name':'program_id','type':'Edm.String','key':True,'searchable':True,'filterable':True,'facetable':False,'sortable':False},
            {'name':'program_role','type':'Edm.String','searchable':True,'filterable':True,'facetable':True},
            {'name':'outgoing_programs_json','type':'Edm.String','searchable':False},
            {'name':'incoming_programs_json','type':'Edm.String','searchable':False},
            {'name':'external_programs_json','type':'Edm.String','searchable':False},
            {'name':'copybooks_used_json','type':'Edm.String','searchable':False},
            {'name':'screens_touched_json','type':'Edm.String','searchable':False},
            {'name':'dependency_blob','type':'Edm.String','searchable':True},
            {'name':'outgoing_count','type':'Edm.Int32','filterable':True,'facetable':False},
            {'name':'incoming_count','type':'Edm.Int32','filterable':True,'facetable':False},
            {'name':'copybook_count','type':'Edm.Int32','filterable':True,'facetable':False},
            {'name':'external_count','type':'Edm.Int32','filterable':True,'facetable':False},
            {'name':'screens_count','type':'Edm.Int32','filterable':True,'facetable':False},
            {'name':'has_outgoing','type':'Edm.Boolean','filterable':True,'facetable':True},
            {'name':'has_incoming','type':'Edm.Boolean','filterable':True,'facetable':True},
            {'name':'updated_at','type':'Edm.String','filterable':False,'facetable':False,'searchable':False}
        ],
        # Removed semantic config due to API mismatch; can re-add with correct shape later.
        'corsOptions': {'allowedOrigins':['*']},
    }


def delete_index(ep, key):
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.delete(url, headers={'api-key':key})
    if r.status_code in (200,204,404):
        if r.status_code == 404:
            print('Index not present (nothing to delete).')
        else:
            print('Deleted existing index.')
    else:
        print('Delete failed', r.status_code, r.text[:200])
        sys.exit(1)


def create_index(ep, key):
    url = f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    body = index_schema()
    r = requests.put(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body)
    if r.status_code not in (200,201):
        print('Create failed', r.status_code, r.text[:400])
        sys.exit(1)
    print(f"Created index {INDEX_NAME}")


def main():
    ap = argparse.ArgumentParser(description='Create new_cobol_program_deps index.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--force', action='store_true', help='Delete existing before create')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    if args.force:
        delete_index(ep,key)
    # Idempotent create (PUT semantics) will overwrite if changed.
    create_index(ep,key)

if __name__ == '__main__':
    main()
