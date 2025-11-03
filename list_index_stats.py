"""List Azure AI Search index stats including document counts and vector field info.

Usage:
  python list_index_stats.py

Relies on SEARCH_ENDPOINT + SEARCH_KEY in env or local.settings.json.
If AZURE_SEARCH_INDEXES is set, limits listing to those names; otherwise lists all.
"""
import os, json, sys
from pathlib import Path
import requests

API_VERSION = "2024-07-01"


def _load_local_settings():
    p = Path("local.settings.json")
    if p.exists():
        try:
            data = json.loads(p.read_text(encoding="utf-8"))
            vals = data.get("Values", {})
            for k in ["SEARCH_ENDPOINT","SEARCH_KEY","AZURE_SEARCH_INDEXES"]:
                if k in vals and k not in os.environ:
                    os.environ[k] = vals[k]
        except Exception:
            pass


def get_indexes(endpoint: str, key: str):
    url = f"{endpoint.rstrip('/')}/indexes?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    r.raise_for_status()
    return r.json().get('value', [])


def get_index_stats(endpoint: str, key: str, name: str):
    # stats endpoint
    stats_url = f"{endpoint.rstrip('/')}/indexes/{name}/stats?api-version={API_VERSION}"
    r = requests.get(stats_url, headers={"api-key": key})
    stats = None
    if r.status_code == 200:
        stats = r.json()
    # full index for vector field detection
    def_url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r2 = requests.get(def_url, headers={"api-key": key})
    r2.raise_for_status()
    definition = r2.json()
    vector_fields = []
    for f in definition.get('fields', []):
        # New API versions use 'vectorSearchDimensions' or 'vectorSearchProfile'
        if 'vectorSearchDimensions' in f or 'vectorSearchProfile' in f:
            vector_fields.append({
                'name': f.get('name'),
                'dimensions': f.get('vectorSearchDimensions'),
                'profile': f.get('vectorSearchProfile')
            })
        # Older preview shape
        elif isinstance(f.get('vectorSearchDimensions'), int):
            vector_fields.append({'name': f.get('name'), 'dimensions': f.get('vectorSearchDimensions'), 'profile': f.get('profile')})
    doc_count = stats.get('documentCount') if stats else None
    storage_size = stats.get('storageSize') if stats else None
    return {
        'name': name,
        'documentCount': doc_count,
        'storageSize': storage_size,
        'vectorFields': vector_fields,
    }


def main():
    _load_local_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not endpoint or not key:
        print("Missing SEARCH_ENDPOINT/SEARCH_KEY env vars.", file=sys.stderr)
        sys.exit(1)
    subset = None
    if os.environ.get('AZURE_SEARCH_INDEXES'):
        subset = [x.strip() for x in os.environ['AZURE_SEARCH_INDEXES'].split(',') if x.strip()]
    try:
        all_indexes = get_indexes(endpoint, key)
    except Exception as e:
        print(f"Failed listing indexes: {e}", file=sys.stderr)
        sys.exit(2)
    names = [i['name'] for i in all_indexes]
    if subset:
        target = [n for n in names if n in subset]
    else:
        target = names
    out = []
    for name in target:
        try:
            out.append(get_index_stats(endpoint, key, name))
        except Exception as e:
            out.append({'name': name, 'error': str(e)})
    # Nicely print summary table
    # Determine vectorSearch + expected field presence
    print("Index Name | Docs | Storage(bytes) | Vector Fields | vectorSearch | Status")
    print("-"*120)
    expected_map = {
        'cobol-symbols': ['name_vector'],
        'cobol-paragraphs': ['name_vector'],
        'cobol-xrefs': ['snippet_vector'],
        'cobol-calls': ['snippet_vector'],
        'code-chunks': ['text_vector'],
    }
    for row in out:
        if 'error' in row:
            print(f"{row['name']} | - | - | - | - | ERROR: {row['error']}")
            continue
        vf_desc = "; ".join([f"{vf['name']}({vf.get('dimensions')}d)" for vf in row['vectorFields']]) or '-'
        # quick vectorSearch detection
        status = 'PASS'
        try:
            # fetch minimal index definition again only if needed (avoid heavy extra calls by inferring) - here we assume earlier retrieval had fields (simplified)
            exp = expected_map.get(row['name'], [])
            present = [vf['name'] for vf in row['vectorFields']]
            missing = [e for e in exp if e not in present]
            if missing:
                status = 'MISSING:' + ','.join(missing)
        except Exception:
            status = 'UNKNOWN'
        vector_search_flag = 'yes' if row['vectorFields'] else 'no'
        print(f"{row['name']} | {row.get('documentCount')} | {row.get('storageSize')} | {vf_desc} | {vector_search_flag} | {status}")
    # Raw JSON output (optional)
    if '--json' in sys.argv:
        print("\nJSON:\n" + json.dumps(out, indent=2))

if __name__ == '__main__':
    main()
