#!/usr/bin/env python3
"""Patch an Azure Cognitive Search index to make a specified vector field retrievable=true.

Usage:
  python patch_index_make_retrievable.py --index new-cobol-files --field contentVector

Requires environment vars SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_* variants) like other tools.
Per 2024-07-01 API we fetch full index schema, mutate target field, PUT back.
Safe / idempotent: only performs update if field exists and retrievable is False.
"""
from __future__ import annotations
import os, sys, json, argparse, copy, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')

def load_local_settings_values():
    """Load Values from local.settings.json if present (Functions-style)."""
    path='local.settings.json'
    if not os.path.exists(path):
        return {}
    try:
        import json
        with open(path,'r',encoding='utf-8') as f:
            js=json.load(f)
        return js.get('Values',{}) or {}
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
        raise SystemExit(f"Failed to update index {name}: {r.status_code} {r.text[:200]}")
    return r.json()


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--field', required=True)
    args=ap.parse_args()
    ep,key=resolve_creds()
    idx=get_index(ep,key,args.index)
    orig=copy.deepcopy(idx)
    target=None
    for f in idx.get('fields',[]):
        if f.get('name')==args.field:
            target=f; break
    if not target:
        print(f"Field {args.field} not found in index {args.index}", file=sys.stderr)
        sys.exit(1)
    # Only change if not already retrievable; if property absent default is False
    if target.get('retrievable') is True:
        print(f"No change: field {args.field} already retrievable=true")
        return
    target['retrievable']=True
    # If service uses 'stored': false to block retrieval, flip it.
    if 'stored' in target and target.get('stored') is False:
        target['stored']=True
    # Some vector field schemas may omit 'stored'; service may infer; we leave absent if not present.
    print(f"Updating index {args.index}: setting {args.field}.retrievable=true (and stored=true if applicable)")
    update_index(ep,key,args.index,idx)
    print("Done")

if __name__=='__main__':
    main()
