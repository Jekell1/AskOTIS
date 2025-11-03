"""Delete orphan (no local file) documents from new-cobol-files index.

Reads gap_orphan_ids.txt (one id per line) and issues a delete batch.
Usage:
  python delete_orphans.py
"""
from __future__ import annotations
import os, requests, env_autoload, sys, json

INDEX='new-cobol-files'
API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def main():
    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    if not os.path.exists('gap_orphan_ids.txt'):
        print('gap_orphan_ids.txt not found')
        return
    ids=[l.strip() for l in open('gap_orphan_ids.txt',encoding='utf-8') if l.strip()]
    if not ids:
        print('No orphan ids to delete')
        return
    endpoint=(os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
    key=os.getenv('AZURE_SEARCH_KEY')
    url=f"{endpoint}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    value=[{'@search.action':'delete','id':i} for i in ids]
    print(f'Deleting {len(value)} orphan docs...')
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':value})
    if r.status_code>=300:
        print('Delete failed', r.status_code, r.text[:400])
    else:
        print('Delete response OK', r.status_code)
        # Azure returns status objects; surface failures if any
        try:
            data=r.json()
            fails=[v for v in data.get('value',[]) if not v.get('status')]
            if fails:
                print(f"{len(fails)} individual deletes failed:")
                for f in fails[:10]:
                    print(' ', f.get('key'), f.get('errorMessage'))
            else:
                print('All deletes succeeded.')
        except Exception:
            pass

if __name__=='__main__':
    main()
