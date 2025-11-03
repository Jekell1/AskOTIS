#!/usr/bin/env python3
"""Unified patch script to ensure all chatbot indexes have:
  - A filterable, facetable Edm.Boolean 'has_vector' field (added if missing)
  - Vector fields (Collection(Edm.Single)) marked searchable and NOT filterable/facetable
  - Vector field 'retrievable': False (conserving payload) unless already True
  - Consistent vectorSearch config (algorithm + profile) if any vector field exists but vectorSearch missing

It will:
  1. Iterate target index list (default = EXPECTED + optional vector-capable.
  2. Fetch current index definitions.
  3. Detect vector fields (Collection(Edm.Single)).
  4. If vector field(s) present and vectorSearch absent -> add minimal HNSW profile/algorithm.
  5. If 'has_vector' missing -> append with filterable/facetable = True.
  6. (Optional) --force-retag to normalize existing has_vector field to filterable/facetable even if currently not.
  7. (Optional) --make-retrievable VECTOR_FIELD pattern to set retrievable=True for chosen vector field names.

Dry run by default; apply with --apply

Usage:
  python patch_vector_flags.py                # dry run
  python patch_vector_flags.py --apply        # apply changes
  python patch_vector_flags.py --indexes code-chunks,cobol-paragraphs --apply

Exit codes: 0 success (even if no changes), 1 on error.
"""
from __future__ import annotations
import os, json, argparse, sys
from typing import Any, Dict, List
import requests

API_VERSION = "2024-07-01"
DEFAULT_HNSW = {
    'name': 'auto-hnsw',
    'kind': 'hnsw',
    'hnswParameters': {
        'm': 4,
        'efConstruction': 200,
        'metric': 'cosine'
    }
}
DEFAULT_PROFILE = {
    'name': 'auto-profile',
    'algorithm': 'auto-hnsw'
}
# Indices we care about by default
TARGET_INDEXES = [
    'code-chunks','cobol-paragraphs','cobol-symbols','cobol-calls','cobol-xrefs',
    'cobol-flow-edges-v2','cobol-facts','cobol-copybooks','cobol-files','cobol-routine-aliases'
]

ADD_HEADERS = ['@odata.etag','encryptionKey','similarity']


def load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def fetch_index(ep: str, key: str, name: str) -> Dict[str,Any]:
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key':key}, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Fetch {name} failed {r.status_code}: {r.text[:200]}")
    return r.json()


def update_index(ep: str, key: str, name: str, defn: Dict[str,Any]):
    r = requests.put(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, data=json.dumps(defn))
    if r.status_code in (200,201,204):
        return True
    raise RuntimeError(f"Update {name} failed {r.status_code}: {r.text[:300]}")


def ensure_vector_search(defn: Dict[str,Any]) -> bool:
    changed = False
    vector_fields = [f for f in defn.get('fields',[]) if f.get('type')=='Collection(Edm.Single)']
    if vector_fields:
        vs = defn.get('vectorSearch')
        if not vs:
            defn['vectorSearch'] = {'algorithms':[DEFAULT_HNSW], 'profiles':[DEFAULT_PROFILE]}
            changed = True
        else:
            alg_key = 'algorithms' if 'algorithms' in vs else 'algorithmConfigurations'
            if alg_key not in vs:
                vs[alg_key] = []
                changed = True
            if 'profiles' not in vs:
                vs['profiles'] = []
                changed = True
            if not any(a.get('name')==DEFAULT_HNSW['name'] for a in vs.get(alg_key,[])):
                vs[alg_key].append(DEFAULT_HNSW)
                changed = True
            if not any(p.get('name')==DEFAULT_PROFILE['name'] for p in vs.get('profiles',[])):
                vs['profiles'].append(DEFAULT_PROFILE)
                changed = True
            # Assign profile to vector fields missing profile
            for f in vector_fields:
                if not f.get('vectorSearchProfile'):
                    f['vectorSearchProfile'] = DEFAULT_PROFILE['name']
                    changed = True
    return changed


def ensure_has_vector(defn: Dict[str,Any], force_retag: bool) -> bool:
    changed = False
    fields = defn.get('fields',[])
    hv = next((f for f in fields if f.get('name')=='has_vector'), None)
    if not hv:
        fields.append({'name':'has_vector','type':'Edm.Boolean','filterable':True,'facetable':True})
        changed = True
    else:
        # Normalize properties if force
        norm_props = {'type':'Edm.Boolean','filterable':True,'facetable':True}
        if force_retag:
            for k,v in norm_props.items():
                if hv.get(k) != v:
                    hv[k] = v
                    changed = True
    return changed


def tweak_vector_fields(defn: Dict[str,Any], make_retrievable: List[str]) -> bool:
    changed = False
    for f in defn.get('fields',[]):
        if f.get('type')=='Collection(Edm.Single)':
            # Ensure not filterable/facetable/sortable
            for k in ('filterable','facetable','sortable'):
                if f.get(k):
                    f[k] = False
                    changed = True
            # Optionally set retrievable if name matches any pattern
            if make_retrievable:
                if any(pat in f.get('name','') for pat in make_retrievable):
                    if f.get('retrievable') is not True:
                        f['retrievable'] = True
                        changed = True
            else:
                # default: leave as-is (often non-retrievable to save payload)
                pass
            # ensure searchable True (required)
            if f.get('searchable') is not True:
                f['searchable'] = True
                changed = True
    return changed


def sanitize(defn: Dict[str,Any]):
    for k in ADD_HEADERS:
        if k in defn:
            defn.pop(k, None)


def process_index(ep: str, key: str, name: str, apply: bool, force_retag: bool, make_retrievable: List[str]):
    try:
        defn = fetch_index(ep,key,name)
    except Exception as e:
        print(f"{name}: ERROR fetching index: {e}")
        return
    original = json.dumps(defn, sort_keys=True)
    changed = False
    changed |= ensure_vector_search(defn)
    changed |= ensure_has_vector(defn, force_retag)
    changed |= tweak_vector_fields(defn, make_retrievable)
    sanitize(defn)
    final = json.dumps(defn, sort_keys=True)
    if original == final:
        print(f"{name}: no changes needed")
        return
    if not apply:
        print(f"{name}: (dry-run) would apply changes")
        # Show a concise diff-like summary
        print_summary_changes(defn)
        return
    try:
        update_index(ep,key,name,defn)
        print(f"{name}: updated")
    except Exception as e:
        print(f"{name}: ERROR updating index: {e}")


def print_summary_changes(defn: Dict[str,Any]):
    vecs = [f['name'] for f in defn.get('fields',[]) if f.get('type')=='Collection(Edm.Single)']
    hv = any(f.get('name')=='has_vector' for f in defn.get('fields',[]))
    print(f"  vector fields: {vecs}")
    print(f"  has_vector present: {hv}")
    vs = defn.get('vectorSearch')
    if vs:
        alg_key = 'algorithms' if 'algorithms' in vs else 'algorithmConfigurations'
        print(f"  vectorSearch algos: {[a.get('name') for a in vs.get(alg_key,[])]}")
        print(f"  vectorSearch profiles: {[p.get('name') for p in vs.get('profiles',[])]}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--indexes', default='auto', help='Comma list or auto (default target set)')
    ap.add_argument('--apply', action='store_true', help='Perform updates (otherwise dry-run)')
    ap.add_argument('--force-retag', action='store_true', help='Normalize existing has_vector field properties')
    ap.add_argument('--make-retrievable', default='', help='Comma list of substrings; if a vector field name contains one, set retrievable=True')
    args = ap.parse_args()
    ep,key = load_config()

    if args.indexes == 'auto':
        indexes = TARGET_INDEXES
    else:
        indexes = [x.strip() for x in args.indexes.split(',') if x.strip()]
    make_ret = [x.strip() for x in args.make_retrievable.split(',') if x.strip()]

    for name in indexes:
        process_index(ep,key,name,args.apply,args.force_retag,make_ret)

if __name__ == '__main__':
    main()
