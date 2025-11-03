"""Delete an Azure AI Search index by name.
Usage:
  python delete_index.py --index new-cobol-files
"""
from __future__ import annotations
import os, argparse, requests, env_autoload, sys

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    args = ap.parse_args()
    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint=(os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
    key=os.getenv('AZURE_SEARCH_KEY')
    api=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
    url=f"{endpoint}/indexes/{args.index}?api-version={api}"
    r=requests.delete(url, headers={'api-key':key})
    print('DELETE', args.index, '->', r.status_code)
    if r.status_code>=300:
        print(r.text[:400])
        sys.exit(1)

if __name__=='__main__':
    main()
