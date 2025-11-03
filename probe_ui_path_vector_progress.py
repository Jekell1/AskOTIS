"""Report embedding coverage for UI paths index.

Usage:
  python probe_ui_path_vector_progress.py

Outputs JSON with:
  total_docs
  with_vector
  without_vector
  percent_complete
  sample_without (up to 5 path_ids)
"""
from __future__ import annotations
import os, json, sys, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_ui_paths'

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search creds', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch_batch(ep,key,skip):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':1000,'skip':skip,'select':'path_id,has_vector'}
    r=requests.post(url,headers=headers,json=payload)
    if r.status_code!=200:
        raise RuntimeError(f"search {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def main():
    load_local_settings()
    ep,key=resolve()
    total=0
    with_vec=0
    without_ids=[]
    skip=0
    while True:
        batch=fetch_batch(ep,key,skip)
        if not batch:
            break
        for b in batch:
            total+=1
            if b.get('has_vector'):
                with_vec+=1
            else:
                if len(without_ids)<5:
                    without_ids.append(b.get('path_id'))
        if len(batch)<1000:
            break
        skip+=1000
    pct = (with_vec/total*100.0) if total else 0.0
    print(json.dumps({
        'total_docs': total,
        'with_vector': with_vec,
        'without_vector': total-with_vec,
        'percent_complete': round(pct,2),
        'sample_without': without_ids
    }, indent=2))

if __name__=='__main__':
    main()
