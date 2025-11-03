#!/usr/bin/env python3
"""Patch the cobol-flow-edges-v2 index to add vector search fields.

Adds:
  - edge_text (concatenated textual content of edge for embedding context)
  - edge_vector (vector field, 1536 dims)
  - has_vector (boolean flag for quick coverage checks)
Also injects vectorSearch + vectorSearchAlgorithmConfigurations if missing.

Idempotent: safe to re-run; will skip adding fields that exist.

Environment:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (or SEARCH_*)

Usage:
  python patch_flow_edges_index.py --dry-run
"""
from __future__ import annotations
import os, json, argparse, sys, time
import requests

API_VERSION = "2024-07-01"
INDEX_NAME = "cobol-flow-edges-v2"
VECTOR_DIMENSIONS = 1536
VECTOR_PROFILE = "standard-profile"
VECTOR_ALGO = "standard-hnsw"


def load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit("Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY")
    return endpoint, key


def get_index(endpoint, key):
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key':key})
    if r.status_code != 200:
        raise SystemExit(f"Failed to fetch index: {r.status_code} {r.text}")
    return r.json()


def patch_index(defn, dry_run=False, verbose=False):
    # Ensure vectorSearch root objects exist
    changed = False
    if 'vectorSearch' not in defn:
        defn['vectorSearch'] = {"algorithms": [], "profiles": []}
        changed = True
    # Some older indexes may have vectorSearch:null
    if defn.get('vectorSearch') is None:
        defn['vectorSearch'] = {"algorithms": [], "profiles": []}
        changed = True
    vs = defn['vectorSearch']
    # Compatibility: some services use 'algorithms' vs 'algorithmConfigurations'
    alg_key = 'algorithms' if 'algorithms' in vs else 'algorithmConfigurations'
    if alg_key not in vs:
        vs[alg_key] = []
        changed = True
    if 'profiles' not in vs:
        vs['profiles'] = []
        changed = True

    # Add algorithm config if missing
    if not any(a.get('name') == VECTOR_ALGO for a in vs.get(alg_key, [])):
        vs[alg_key].append({
            'name': VECTOR_ALGO,
            'kind': 'hnsw',
            'hnswParameters': {
                'm': 4,
                'efConstruction': 400,
                'metric': 'cosine'
            }
        })
        changed = True

    # Add profile if missing
    if not any(p.get('name') == VECTOR_PROFILE for p in vs.get('profiles', [])):
        vs['profiles'].append({
            'name': VECTOR_PROFILE,
            'algorithm': VECTOR_ALGO
        })
        changed = True

    # Check / add fields
    field_names = {f['name'] for f in defn.get('fields', [])}
    new_fields = []
    if 'edge_text' not in field_names:
        new_fields.append({
            'name':'edge_text','type':'Edm.String','searchable':True
        })
    if 'edge_vector' not in field_names:
        new_fields.append({
            'name':'edge_vector',
            'type':'Collection(Edm.Single)',
            'searchable':True,
            'filterable':False,
            'facetable':False,
            'sortable':False,
            'dimensions': VECTOR_DIMENSIONS,
            'vectorSearchProfile': VECTOR_PROFILE
        })
    if 'has_vector' not in field_names:
        new_fields.append({
            'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True
        })
    if new_fields:
        defn['fields'].extend(new_fields)
        changed = True

    if not changed:
        return False, defn

    if dry_run:
        return True, defn

    # Remove read-only props that block update
    for k in ['@odata.etag','encryptionKey','similarity']: # safety
        if k in defn:
            defn.pop(k, None)

    return True, defn


def update_index(endpoint, key, defn):
    url = f"{endpoint}/indexes/{INDEX_NAME}?api-version={API_VERSION}"
    r = requests.put(url, headers={'api-key':key,'Content-Type':'application/json'}, data=json.dumps(defn))
    if r.status_code not in (200,201):
        raise SystemExit(f"Update failed: {r.status_code} {r.text}")
    return r.json()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--verbose', action='store_true')
    args = ap.parse_args()
    endpoint, key = load_config()
    idx = get_index(endpoint, key)
    changed, patched = patch_index(idx, dry_run=args.dry_run, verbose=args.verbose)
    if not changed:
        print("No changes needed; index already vector-enabled.")
        return
    if args.dry_run:
        print("DRY RUN - patched definition preview:\n" + json.dumps(patched, indent=2)[:4000])
        return
    updated = update_index(endpoint, key, patched)
    print("Index updated. Fields now:")
    for f in updated.get('fields', []):
        if f['name'] in ('edge_text','edge_vector','has_vector'):
            print("  *", f['name'])

if __name__ == '__main__':
    main()
