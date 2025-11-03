python .\\count_index_docs.py --index new-cobol-files"""Fetch and summarize an Azure AI Search index definition.

Usage:
  python verify_index.py --index new-cobol-files

Outputs:
  - Basic identity (name, fields count)
  - Vector field summary (name, dimensions, profile)
  - vectorSearch block (algorithms + profiles)
  - Semantic configuration overview

Relies on the same credential autoload logic expectation as other scripts:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY or SEARCH_ENDPOINT / SEARCH_KEY
If not present, attempts to read local.settings.json Values block.
"""
from __future__ import annotations
import os, sys, json, argparse, requests

LOCAL_SETTINGS_FILE = 'local.settings.json'
NEEDED = ['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']
AUTO_KEYS = set(NEEDED + ['SEARCH_ENDPOINT','SEARCH_KEY'])
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')


def _autoload():
    if all(os.getenv(k) for k in NEEDED):
        return
    path = LOCAL_SETTINGS_FILE
    if not os.path.exists(path):
        return
    try:
        data = json.loads(open(path,'r',encoding='utf-8').read())
        vals = data.get('Values',{})
    except Exception:
        return
    changed = 0
    for k,v in vals.items():
        if k in AUTO_KEYS and k not in os.environ and isinstance(v,str) and v.strip():
            os.environ[k] = v; changed += 1
    if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
        os.environ['AZURE_SEARCH_ENDPOINT'] = os.environ['SEARCH_ENDPOINT']; changed += 1
    if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
        os.environ['AZURE_SEARCH_KEY'] = os.environ['SEARCH_KEY']; changed += 1
    if changed:
        print(f"Autoloaded {changed} credential(s) from {path}")


def fetch_index(name: str):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        print('Missing credentials (AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY).', file=sys.stderr)
        sys.exit(2)
    url = f"{endpoint}/indexes/{name}?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        print(f"ERROR: {r.status_code}\n{r.text}")
        sys.exit(3)
    return r.json()


def summarize(idx: dict):
    print(f"Index: {idx.get('name')}\nFields: {len(idx.get('fields',[]))}")
    # Vector fields
    vec_fields = []
    for f in idx.get('fields',[]):
        if f.get('type') == 'Collection(Edm.Single)' and 'dimensions' in f:
            vec_fields.append({
                'name': f['name'],
                'dimensions': f.get('dimensions'),
                'profile': f.get('vectorSearchProfile'),
                'searchable': f.get('searchable')
            })
    if vec_fields:
        print('\nVector Fields:')
        for vf in vec_fields:
            print(f"  - {vf['name']} dims={vf['dimensions']} profile={vf['profile']} searchable={vf['searchable']}")
    vs = idx.get('vectorSearch') or {}
    if vs:
        print('\nVector Search:')
        algos = vs.get('algorithms') or []
        profiles = vs.get('profiles') or []
        for a in algos:
            print(f"  Algo: {a.get('name')} kind={a.get('kind')}")
        for p in profiles:
            print(f"  Profile: {p.get('name')} algo={p.get('algorithm')}")
    sem = idx.get('semantic') or {}
    if sem:
        print('\nSemantic Configurations:')
        for cfg in sem.get('configurations',[]):
            name = cfg.get('name')
            pf = cfg.get('prioritizedFields') or {}
            title = pf.get('titleField',{}).get('fieldName')
            contents = [c.get('fieldName') for c in pf.get('prioritizedContentFields',[])]
            keywords = [k.get('fieldName') for k in pf.get('prioritizedKeywordsFields',[])]
            print(f"  - {name}: title={title} content={contents} keywords={keywords}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True, help='Index name to verify')
    args = ap.parse_args()
    _autoload()
    idx = fetch_index(args.index)
    summarize(idx)

if __name__ == '__main__':
    main()
