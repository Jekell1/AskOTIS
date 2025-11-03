"""List all Azure AI Search indexes for this environment.

Uses SEARCH_ENDPOINT / SEARCH_KEY (or AZURE_* variants) and falls back to
local.settings.json Values.

Usage:
  python list_indexes.py
"""
import os, json, sys, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def main():
    load_local_settings()
    ep, key = resolve_endpoint_key()
    url = f"{ep}/indexes?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise SystemExit(f"List failed {r.status_code}: {r.text[:300]}")
    data = r.json().get('value', [])
    names = sorted([d.get('name') for d in data])
    print(f"Found {len(names)} indexes:")
    for n in names:
        print('  -', n)

if __name__ == '__main__':
    main()
