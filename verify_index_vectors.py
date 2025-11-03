"""Verify Azure AI Search index vector configuration.

Checks:
- vectorSearch block existence
- Each expected vector field has vectorSearchDimensions and profile
- Reports PASS/FAIL summary

Usage:
  python verify_index_vectors.py [--only code-chunks,cobol-symbols]
"""
import os, json, argparse, requests, sys
from pathlib import Path

API_VERSION = "2024-07-01"
# Heuristic: expected vector fields by index (matching recreate script)
EXPECTED = {
    "cobol-symbols": ["name_vector"],
    "cobol-paragraphs": ["name_vector"],
    "cobol-xrefs": ["snippet_vector"],
    "cobol-calls": ["snippet_vector"],
    "code-chunks": ["text_vector"],
}

def load_settings():
    try:
        data = json.loads(Path('local.settings.json').read_text(encoding='utf-8'))
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def fetch_index(endpoint: str, key: str, name: str):
    url = f"{endpoint.rstrip('/')}/indexes/{name}?api-version={API_VERSION}"
    r = requests.get(url, headers={"api-key": key})
    if r.status_code == 404:
        return None
    r.raise_for_status()
    return r.json()

def analyze(name: str, index_json: dict):
    issues = []
    vs = index_json.get('vectorSearch') if index_json else None
    if not vs:
        issues.append('Missing vectorSearch block')
    fields = {f['name']: f for f in index_json.get('fields', [])} if index_json else {}
    expected_fields = EXPECTED.get(name, [])
    for field_name in expected_fields:
        f = fields.get(field_name)
        if not f:
            issues.append(f'Missing field {field_name}')
            continue
        if 'vectorSearchDimensions' not in f:
            issues.append(f'{field_name} missing vectorSearchDimensions')
        if 'vectorSearchProfile' not in f:
            issues.append(f'{field_name} missing vectorSearchProfile')
    status = 'PASS' if not issues else 'FAIL'
    return status, issues


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--only', help='Comma separated subset of indexes to check')
    args = parser.parse_args()
    load_settings()
    endpoint = os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not endpoint or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr)
        return 1
    # list indexes
    li_url = f"{endpoint.rstrip('/')}/indexes?api-version={API_VERSION}"
    r = requests.get(li_url, headers={"api-key": key})
    r.raise_for_status()
    names = [i['name'] for i in r.json().get('value', [])]
    if args.only:
        subset = [x.strip() for x in args.only.split(',') if x.strip()]
        names = [n for n in names if n in subset]
    print("Index | Status | Issues")
    print("-"*80)
    fail_count = 0
    for n in names:
        idx = fetch_index(endpoint, key, n)
        status, issues = analyze(n, idx)
        if status != 'PASS':
            fail_count += 1
        print(f"{n} | {status} | {'; '.join(issues) if issues else '-'}")
    print("-"*80)
    if fail_count:
        print(f"FAILURES: {fail_count}")
    else:
        print("ALL PASS")
    return 0 if fail_count == 0 else 2

if __name__ == '__main__':
    raise SystemExit(main())
