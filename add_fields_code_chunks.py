#!/usr/bin/env python3
"""Add new metadata fields to existing code-chunks index (non-destructive).

Adds:
  - window_index (Edm.Int32, filterable, facetable, sortable)
  - created_at (Edm.String, filterable, sortable)
  - tokens_estimate (Edm.Int32, filterable, sortable)
  - window_size (Edm.Int32, filterable, facetable, sortable)  # provenance of chunking
  - stride (Edm.Int32, filterable, facetable, sortable)       # provenance of overlap

Safe to run multiple times: skips fields that already exist.

Usage:
  python add_fields_code_chunks.py --endpoint $env:AZURE_SEARCH_ENDPOINT --key $env:AZURE_SEARCH_KEY

If endpoint/key omitted, tries local.settings.json Values (SEARCH_ENDPOINT, SEARCH_KEY).
"""
import argparse, json, os, sys, requests

API_VERSION = "2024-07-01"
INDEX_NAME = "code-chunks"
NEW_FIELDS = [
    {"name":"window_index","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
    {"name":"created_at","type":"Edm.String","filterable":True,"sortable":True},
    {"name":"tokens_estimate","type":"Edm.Int32","filterable":True,"sortable":True},
    {"name":"window_size","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
    {"name":"stride","type":"Edm.Int32","filterable":True,"facetable":True,"sortable":True},
]


def load_settings():
    vals = {}
    try:
        data = json.load(open('local.settings.json','r', encoding='utf-8'))
        vals = data.get('Values', {})
    except Exception:
        pass
    return vals


def get_index(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 404:
        raise SystemExit(f"Index '{name}' not found")
    if r.status_code >= 300:
        raise SystemExit(f"Failed to fetch index: {r.status_code} {r.text[:300]}")
    return r.json()


def update_index(endpoint: str, key: str, name: str, body: dict):
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.put(url, headers={"api-key": key, "Content-Type": "application/json"}, json=body)
    if r.status_code not in (200,201):
        raise SystemExit(f"Update failed {r.status_code}: {r.text[:500]}")
    return r.json()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    settings = load_settings()
    endpoint = args.endpoint or os.environ.get('AZURE_SEARCH_ENDPOINT') or os.environ.get('SEARCH_ENDPOINT') or settings.get('AZURE_SEARCH_ENDPOINT') or settings.get('SEARCH_ENDPOINT')
    key = args.key or os.environ.get('AZURE_SEARCH_KEY') or os.environ.get('SEARCH_KEY') or settings.get('AZURE_SEARCH_KEY') or settings.get('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing endpoint/key')

    idx = get_index(endpoint, key, INDEX_NAME)
    existing_names = {f['name'] for f in idx.get('fields', [])}
    additions = [f for f in NEW_FIELDS if f['name'] not in existing_names]
    if not additions:
        print('All new fields already present; nothing to do.')
        return

    # Azure Search index update requires full index definition with new fields appended
    idx['fields'].extend(additions)

    if args.dry_run:
        print('Dry run - would add fields: ' + ', '.join(f['name'] for f in additions))
        return

    update_index(endpoint, key, INDEX_NAME, idx)
    print('Added fields: ' + ', '.join(f['name'] for f in additions))

if __name__ == '__main__':
    main()
