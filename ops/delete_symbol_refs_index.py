"""Force delete the new_cobol_symbol_refs index with retries.
Usage:
  python ops/delete_symbol_refs_index.py
"""
from __future__ import annotations
import os, json, time, sys, requests
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
RETRIES=5

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key'); sys.exit(2)
    return ep.rstrip('/'), key

def main():
    load(); ep,key=resolve()
    url=f"{ep}/indexes/{INDEX}?api-version={API}"
    for attempt in range(1,RETRIES+1):
        try:
            r=requests.delete(url,headers={'api-key':key},timeout=60)
            if r.status_code in (200,204,404):
                if r.status_code==404:
                    print('[OK] Index already absent (404).')
                else:
                    print('[OK] Deleted index status', r.status_code)
                break
            print(f"[WARN] Attempt {attempt} delete failed {r.status_code}: {r.text[:200]}")
        except KeyboardInterrupt:
            print('\n[ABORT] User interrupted.'); sys.exit(1)
        except Exception as e:
            print(f'[WARN] Attempt {attempt} exception: {e}')
        if attempt<RETRIES:
            time.sleep(1.5*attempt)
    else:
        print('[FATAL] Exhausted retries deleting index.'); sys.exit(1)

if __name__=='__main__':
    main()
