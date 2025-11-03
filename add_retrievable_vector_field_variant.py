#!/usr/bin/env python3
"""Add a new retrievable vector field variant to an existing Azure Cognitive Search index.

Rationale: Existing vector field cannot have retrievable flipped post-creation. We add a parallel
field (default name: contentVector_r) copying dimensions/profile from the source field and setting
retrievable=true so we can inspect / sample vectors directly.

Usage:
  python add_retrievable_vector_field_variant.py --index new-cobol-files \
      --source-field contentVector --new-field contentVector_r

Environment credentials: SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_* variants) or local.settings.json Values section.
"""
from __future__ import annotations
import os, sys, json, argparse, copy, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')


def load_local_settings_values():
    path='local.settings.json'
    if not os.path.exists(path):
        return {}
    try:
        with open(path,'r',encoding='utf-8') as f:
            return json.load(f).get('Values',{}) or {}
    except Exception:
        return {}

def resolve_creds():
    vals=load_local_settings_values()
    ep=(os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or
         vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT'))
    key=(os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or
          vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY'))
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY (env or local.settings.json Values)', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key

def get_index(ep,key,name):
    r=requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key':key})
    if r.status_code!=200:
        raise SystemExit(f"Failed to get index {name}: {r.status_code} {r.text[:200]}")
    return r.json()

def update_index(ep,key,name,payload):
    r=requests.put(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=payload)
    if r.status_code!=200:
        # Try to extract structured error for clarity.
        detail = r.text
        try:
            js = r.json()
            detail = json.dumps(js, indent=2)[:1200]
        except Exception:
            detail = detail[:600]
        raise SystemExit(f"Failed to update index {name}: HTTP {r.status_code}\n{detail}")
    return r.json()

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--source-field', default='contentVector')
    ap.add_argument('--new-field', default='contentVector_r')
    ap.add_argument('--force', action='store_true', help='Recreate if already present (abort without --force)')
    args=ap.parse_args()
    ep,key=resolve_creds()
    idx=get_index(ep,key,args.index)

    fields=idx.get('fields',[])
    src=None
    for f in fields:
        if f.get('name')==args.source_field:
            src=f; break
    if not src:
        raise SystemExit(f"Source field '{args.source_field}' not found in index {args.index}")
    existing = next((f for f in fields if f.get('name')==args.new_field), None)
    if existing:
        # Treat as success if already retrievable + vector dims present.
        dims = existing.get('vectorSearchDimensions') or existing.get('dimensions')
        if dims and existing.get('retrievable') is True:
            print(f"Field {args.new_field} already present (retrievable={existing.get('retrievable')}, dims={dims}). Nothing to do.")
            return
        else:
            if not args.force:
                print(f"Field {args.new_field} exists but retrievable={existing.get('retrievable')} dims={dims}. Use --force to recreate (not implemented). Aborting.")
                return
            else:
                print(f"--force specified but drop & recreate not implemented yet. Aborting.")
                return

    # Create new field by copying source and adjusting name + retrievability.
    new_field=copy.deepcopy(src)
    # Remove immutable / unnecessary keys just in case.
    for k in ['analyzer','normalizer','synonymMaps','key','hidden']:
        if k in new_field: new_field.pop(k)
    new_field['name']=args.new_field
    # Azure may use either 'dimensions' or 'vectorSearchDimensions' in different API responses. Keep what is there.
    # Ensure retrievable true.
    new_field['retrievable']=True
    # Ensure not marked stored=False if present.
    if 'stored' in new_field and new_field['stored'] is False:
        new_field['stored']=True
    # These flags typical for vector fields (keep defaults from source unless absent):
    for flag in ['searchable','filterable','sortable','facetable']:
        if flag not in new_field:
            new_field[flag]=False
    dims_display = new_field.get('vectorSearchDimensions') or new_field.get('dimensions')
    print(f"Adding new retrievable vector field '{args.new_field}' (dims={dims_display}) using profile '{new_field.get('vectorSearchProfile')}'")
    # Quick sanity: ensure vectorSearch section contains referenced profile if any.
    prof = new_field.get('vectorSearchProfile')
    if prof:
        profiles = [p.get('name') for p in (idx.get('vectorSearch') or {}).get('profiles', [])]
        if prof not in profiles:
            print(f"WARNING: Field references vectorSearchProfile '{prof}' not present in index.profiles -> {profiles}")
    fields.append(new_field)
    try:
        update_index(ep,key,args.index,idx)
    except SystemExit as e:
        print(str(e), file=sys.stderr)
        # Output minimal field dict for debugging
        print('Field payload snippet:', json.dumps({k:new_field.get(k) for k in ['name','type','retrievable','vectorSearchDimensions','dimensions','vectorSearchProfile']}, indent=2))
        raise
    print('Success: field added and index updated.')

if __name__=='__main__':
    main()
