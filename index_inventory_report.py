"""Generate a consolidated inventory report for all base COBOL indexes (new_* pattern).

Outputs for each index:
  - Document count
  - Vector fields (name + dimensions)
  - Key field
  - Sample of selected fields (first 5 docs select subset)

Usage:
  python index_inventory_report.py --prefix new_

Relies on local.settings.json for endpoint/key if env vars not set.
"""
from __future__ import annotations
import os, sys, json, argparse, requests
from typing import List, Dict, Any

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def list_indexes(ep: str, key: str) -> List[str]:
    url = f"{ep}/indexes?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"List failed {r.status_code}: {r.text[:200]}")
    return [d['name'] for d in r.json().get('value', [])]

def get_index_def(ep: str, key: str, name: str) -> Dict[str,Any]:
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"Def failed {r.status_code}: {r.text[:200]}")
    return r.json()

def get_count(ep: str, key: str, name: str) -> int:
    r = requests.get(f"{ep}/indexes/{name}/docs/$count?api-version={API_VERSION}", headers={'api-key': key})
    if r.status_code != 200:
        raise RuntimeError(f"Count failed {r.status_code}: {r.text[:200]}")
    return int(r.text)

def sample_docs(ep: str, key: str, name: str, select: str, top: int = 5) -> List[Dict[str,Any]]:
    url = f"{ep}/indexes/{name}/docs?api-version={API_VERSION}&search=*&$top={top}&$select={select}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        return []
    return r.json().get('value', [])

def detect_vectors(index_def: Dict[str,Any]) -> List[Dict[str,Any]]:
    out = []
    for f in index_def.get('fields', []):
        if f.get('type','').startswith('Collection(Edm.Single)') and (f.get('dimensions') or f.get('searchDimensions')):
            out.append({'name': f.get('name'), 'dimensions': f.get('dimensions') or f.get('searchDimensions')})
    return out

def find_key_field(index_def: Dict[str,Any]) -> str:
    for f in index_def.get('fields', []):
        if f.get('key'):
            return f.get('name')
    return ''

def main():
    ap = argparse.ArgumentParser(description='Inventory report for new_* indexes.')
    ap.add_argument('--prefix', default='new_', help='Index name prefix filter')
    ap.add_argument('--sample-select', default='program_id,copybook_name,item_name,paragraph_name,summary', help='Comma list of candidate fields to try selecting (only those present will return)')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key()
    all_names = sorted(list_indexes(ep, key))
    targets = [n for n in all_names if n.startswith(args.prefix)]
    print(f"Inventory for prefix '{args.prefix}' — {len(targets)} indexes\n")
    for name in targets:
        try:
            idx_def = get_index_def(ep, key, name)
            count = get_count(ep, key, name)
            vectors = detect_vectors(idx_def)
            key_field = find_key_field(idx_def)
            print(f"Index: {name}")
            print(f"  Count: {count}")
            if vectors:
                for v in vectors:
                    print(f"  Vector Field: {v['name']} dim={v['dimensions']}")
            else:
                print("  Vector Field: (none)")
            print(f"  Key Field: {key_field}")
            sample = sample_docs(ep, key, name, args.sample_select)
            if sample:
                print(f"  Sample Fields (first doc): {sorted(sample[0].keys())[:12]}")
            print()
        except Exception as e:
            print(f"  [WARN] Failed index {name}: {e}\n")

if __name__ == '__main__':
    main()
