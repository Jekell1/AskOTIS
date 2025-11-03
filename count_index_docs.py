"""Count documents in an Azure AI Search index (default: new-cobol-files).

Usage:
  python count_index_docs.py                # counts default index
  python count_index_docs.py --index new_cobol_calls
  python count_index_docs.py --index new_cobol_program_meta
"""
import os, requests, env_autoload, argparse

DEFAULT_INDEX = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def main():
    ap = argparse.ArgumentParser(description='Count documents in a search index.')
    ap.add_argument('--index', default=DEFAULT_INDEX, help='Index name (default: %(default)s)')
    args = ap.parse_args()
    index = args.index
    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing search endpoint/key env vars.')
    url = f"{endpoint}/indexes/{index}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise SystemExit(f"Count failed {r.status_code}: {r.text[:300]}")
    print(f"Index {index} document count: {r.text.strip()}")

if __name__ == '__main__':
    main()
