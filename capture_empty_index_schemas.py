"""Capture current schemas for designated (currently empty) indexes and archive them.

Usage:
  python capture_empty_index_schemas.py --indexes cobol-symbols,cobol-paragraphs,cobol-xrefs,cobol-calls,code-chunks
If --indexes omitted, defaults to the above set.
Creates ./schema_archive/<index>.original.json (skips if exists unless --overwrite).
"""
import os, json, argparse, datetime, requests, pathlib

API_VERSION = "2024-07-01"
DEFAULT_INDEXES = [
    "cobol-symbols",
    "cobol-paragraphs",
    "cobol-xrefs",
    "cobol-calls",
    "code-chunks",
]

def load_settings():
    try:
        data = json.loads(pathlib.Path('local.settings.json').read_text(encoding='utf-8'))
        vals = data.get('Values', {})
        for key in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if key in vals and key not in os.environ:
                os.environ[key] = vals[key]
    except Exception:
        pass


def fetch_index(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 404:
        return None
    r.raise_for_status()
    return r.json()

def fetch_stats(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}/stats?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    return r.json() if r.status_code == 200 else {}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--indexes', help='Comma separated index names')
    parser.add_argument('--overwrite', action='store_true')
    args = parser.parse_args()

    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not endpoint or not key:
        print('Missing SEARCH_ENDPOINT / SEARCH_KEY')
        return 1

    indexes = DEFAULT_INDEXES
    if args.indexes:
        indexes = [x.strip() for x in args.indexes.split(',') if x.strip()]

    out_dir = pathlib.Path('schema_archive')
    out_dir.mkdir(exist_ok=True)
    ts = datetime.datetime.utcnow().isoformat() + 'Z'

    for name in indexes:
        schema = fetch_index(endpoint, key, name)
        stats = fetch_stats(endpoint, key, name)
        path = out_dir / f"{name}.original.json"
        if path.exists() and not args.overwrite:
            print(f"Skip {name}: archive exists (use --overwrite).")
            continue
        record = {
            'captured': ts,
            'endpoint': endpoint,
            'name': name,
            'documentCount': stats.get('documentCount'),
            'storageSize': stats.get('storageSize'),
            'schema': schema,
        }
        path.write_text(json.dumps(record, indent=2), encoding='utf-8')
        if schema is None:
            print(f"{name}: NOT FOUND (schema saved as null)")
        else:
            print(f"{name}: captured (docs={record['documentCount']})")
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
