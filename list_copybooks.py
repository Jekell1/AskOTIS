"""List copybook inventory, usage counts, and zero-usage detection.

Outputs:
  * Total copybooks (meta index)
  * Total usage docs (usage index)
  * Top N most-used copybooks
  * Copybooks with zero usage

Usage:
  python list_copybooks.py --top 20
"""
from __future__ import annotations
import os, sys, json, argparse, requests, collections

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
META_INDEX = 'new_cobol_copybook_meta'
USAGE_INDEX = 'new_cobol_copybook_usage'

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key')
        sys.exit(1)
    return ep.rstrip('/'), key

def count_index(ep, key, index):
    url = f"{ep}/indexes/{index}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key':key}, timeout=30)
    if r.status_code != 200:
        return -1
    return int(r.text)

def facet_usage(ep, key, top):
    url = f"{ep}/indexes/{USAGE_INDEX}/docs/search?api-version={API_VERSION}"
    payload = {
        'search': '*',
        'facets': [f'copybook_name,count:{top}'],
        'top': 0
    }
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=60)
    if r.status_code != 200:
        print('[WARN] Facet request failed', r.status_code, r.text[:200])
        return []
    fac = r.json().get('@search.facets', {})
    return fac.get('copybook_name', [])

def list_all_copybooks(ep, key):
    # paginate meta index retrieving only copybook_name
    names = set()
    url = f"{ep}/indexes/{META_INDEX}/docs/search?api-version={API_VERSION}"
    payload = {'search':'*','select':'copybook_name','top':1000}
    cont = None
    while True:
        if cont:
            payload['continuationToken'] = cont
        r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=60)
        if r.status_code != 200:
            print('[ERROR] Failed listing copybooks', r.status_code, r.text[:200])
            break
        data = r.json()
        for v in data.get('value', []):
            if 'copybook_name' in v:
                names.add(v['copybook_name'])
        cont = data.get('@search.nextPageParameters', {}).get('continuationToken')
        if not cont:
            break
    return names

def usage_copybooks(ep, key):
    # retrieve unique copybook_name via facet with large size chunks
    all_names = set()
    # we can approximate by paging facets in slices using search filter on first letter
    letters = list('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
    url = f"{ep}/indexes/{USAGE_INDEX}/docs/search?api-version={API_VERSION}"
    for ch in letters:
        payload = {'search':'*','filter':f"startswith(copybook_name,'{ch}')", 'facets':['copybook_name,count:1000'], 'top':0}
        r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=60)
        if r.status_code != 200:
            continue
        data = r.json().get('@search.facets', {})
        for item in data.get('copybook_name', []):
            all_names.add(item['value'])
    return all_names

def main():
    ap = argparse.ArgumentParser(description='List copybook inventory and usage stats')
    ap.add_argument('--top', type=int, default=20)
    args = ap.parse_args()
    load_local_settings()
    ep, key = endpoint_key()
    meta_count = count_index(ep, key, META_INDEX)
    usage_count = count_index(ep, key, USAGE_INDEX)
    print(f"Meta copybooks: {meta_count} usage docs: {usage_count}")
    facets = facet_usage(ep, key, args.top)
    if facets:
        print(f"Top {len(facets)} most-used copybooks:")
        for f in facets:
            print(f"  {f['value']}: {f['count']}")
    meta_names = list_all_copybooks(ep, key)
    usage_names = usage_copybooks(ep, key)
    zero = sorted(meta_names - usage_names)
    print(f"Copybooks with zero usage occurrences: {len(zero)}")
    for name in zero[:50]:
        print('  ', name)
    if len(zero) > 50:
        print('  ...')

if __name__ == '__main__':
    main()
