"""List relevant new_cobol_* Azure AI Search indexes.

Loads AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY from environment, falling back to local.settings.json Values
(SEARCH_ENDPOINT / SEARCH_KEY / AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY) without printing secrets.

Usage:
    python search/indexes/list_target_indexes.py
    python search/indexes/list_target_indexes.py --all   # show all index names
"""
from __future__ import annotations
import os, json, sys, argparse, urllib.request, urllib.error, re
from typing import List

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2023-11-01')
TARGET_PATTERN = re.compile(r'^new_cobol_(copybook_meta|program_deps|program_flows|copybook_usage|symbol_refs|variable_usage|screen_nodes|ui_paths|program_meta)$')

POSSIBLE_ENDPOINT_KEYS = ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT']
POSSIBLE_KEY_KEYS = ['AZURE_SEARCH_KEY','SEARCH_KEY']


def load_local_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            data = json.load(f)
        vals = data.get('Values',{}) if isinstance(data,dict) else {}
        for k in POSSIBLE_ENDPOINT_KEYS + POSSIBLE_KEY_KEYS:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def resolve_endpoint_key():
    ep = None
    key = None
    for k in POSSIBLE_ENDPOINT_KEYS:
        if os.getenv(k):
            ep = os.getenv(k)
            break
    for k in POSSIBLE_KEY_KEYS:
        if os.getenv(k):
            key = os.getenv(k)
            break
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (or SEARCH_ENDPOINT / SEARCH_KEY).', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def fetch_indexes(ep: str, key: str) -> List[str]:
    url = f"{ep}/indexes?api-version={API_VERSION}"
    req = urllib.request.Request(url, headers={'api-key': key})
    try:
        with urllib.request.urlopen(req) as resp:  # nosec B310
            js = json.load(resp)
    except urllib.error.HTTPError as e:
        print(f"HTTP error: {e.code} {e.read().decode('utf-8','replace')[:400]}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error fetching indexes: {e}", file=sys.stderr)
        sys.exit(1)
    return [v.get('name','') for v in js.get('value',[]) if isinstance(v,dict)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--all', action='store_true', help='Show all index names (not just target pattern)')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key()
    names = fetch_indexes(ep, key)
    if args.all:
        for n in sorted(names):
            print(n)
    else:
        targets = [n for n in names if TARGET_PATTERN.match(n)]
        for n in sorted(targets):
            print(n)

if __name__ == '__main__':
    main()
