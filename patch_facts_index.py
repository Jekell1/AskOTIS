#!/usr/bin/env python3
"""Patch the cobol-facts index to add vector embedding support.

Adds (if missing):
  - fact_text   (string, searchable)
  - fact_vector (Collection(Edm.Single)) 1536 dims
  - has_vector  (boolean flag)
Also ensures vectorSearch algorithms/profiles are present.

Usage:
  python patch_facts_index.py --dry-run
  python patch_facts_index.py

Environment vars (or local.settings.json Values):
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
"""
from __future__ import annotations
import os, json, argparse, sys
import requests

API_VERSION = "2024-07-01"
INDEX_NAME = "cobol-facts"
VECTOR_DIMENSIONS = 1536
VECTOR_PROFILE = "facts-profile"
VECTOR_ALGO = "facts-hnsw"


def load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    return ep, key


def get_index(ep, key):
    r = requests.get(f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code != 200:
        raise SystemExit(f"Failed to fetch index: {r.status_code} {r.text[:400]}")
    return r.json()


def patch(defn):
    changed = False
    if 'vectorSearch' not in defn or defn.get('vectorSearch') is None:
        defn['vectorSearch'] = {"algorithms": [], "profiles": []}
        changed = True
    vs = defn['vectorSearch']
    alg_key = 'algorithms' if 'algorithms' in vs else 'algorithmConfigurations'
    if alg_key not in vs:
        vs[alg_key] = []
        changed = True
    if 'profiles' not in vs:
        vs['profiles'] = []
        changed = True
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
    if not any(p.get('name') == VECTOR_PROFILE for p in vs.get('profiles', [])):
        vs['profiles'].append({'name': VECTOR_PROFILE, 'algorithm': VECTOR_ALGO})
        changed = True

    field_names = {f['name'] for f in defn.get('fields', [])}
    add_fields = []
    if 'fact_text' not in field_names:
        add_fields.append({'name':'fact_text','type':'Edm.String','searchable':True})
    if 'fact_vector' not in field_names:
        add_fields.append({'name':'fact_vector','type':'Collection(Edm.Single)','searchable':True,'filterable':False,'facetable':False,'sortable':False,'dimensions':VECTOR_DIMENSIONS,'vectorSearchProfile':VECTOR_PROFILE})
    if 'has_vector' not in field_names:
        add_fields.append({'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True})
    if add_fields:
        defn['fields'].extend(add_fields)
        changed = True
    # remove read-only props
    for k in ['@odata.etag','encryptionKey','similarity']:
        if k in defn:
            defn.pop(k, None)
    return changed, defn


def update_index(ep, key, defn):
    r = requests.put(
        f"{ep}/indexes/{INDEX_NAME}?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        data=json.dumps(defn)
    )
    # Some Azure Search responses for idempotent updates may return 204 No Content
    if r.status_code in (200, 201):
        return r.json()
    if r.status_code == 204:
        # Treat as success; return the defn we attempted to set
        return defn
    raise SystemExit(f"Update failed: {r.status_code} {r.text[:400]}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()
    ep,key = load_config()
    idx = get_index(ep,key)
    changed, patched = patch(idx)
    if not changed:
        print('No changes needed; cobol-facts already vector-enabled.')
        return
    if args.dry_run:
        print('DRY RUN: would update definition with added/changed fields/profiles:')
        print(json.dumps(patched, indent=2)[:4000])
        return
    updated = update_index(ep,key,patched)
    added = [f['name'] for f in updated.get('fields',[]) if f['name'] in ('fact_text','fact_vector','has_vector')]
    print('Index updated. Added fields:', ', '.join(added))

if __name__ == '__main__':
    main()
