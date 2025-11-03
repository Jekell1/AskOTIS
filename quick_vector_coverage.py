#!/usr/bin/env python3
"""quick_vector_coverage.py
Simple coverage summary for file and chunk indexes.

Usage: python quick_vector_coverage.py --indexes cobol-files-v1 cobol-file-chunks-v1
If no --indexes given, defaults to those two names.
"""
import os, sys, json, argparse, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')

def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals=json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k]=v
        except Exception:
            pass


def count_docs(endpoint: str, key: str, index: str, flt: str|None=None):
    url=f"{endpoint}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt:
        body['filter']=flt
    r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code>=300:
        return {'error': r.text[:300]}
    js=r.json()
    return {'count': js.get('@odata.count',0)}


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--indexes', nargs='*', default=['cobol-files-v1','cobol-file-chunks-v1'])
    args=ap.parse_args()
    load_settings()
    ep=os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT')
    key=os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY')
    if not (ep and key):
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    ep=ep.rstrip('/')
    rows=[]
    for idx in args.indexes:
        total=count_docs(ep,key,idx)
        with_vec=count_docs(ep,key,idx,'has_vectors eq true') if idx=='cobol-files-v1' else count_docs(ep,key,idx,'has_vector eq true')
        rows.append({'index': idx, 'total': total.get('count'), 'with_vectors': with_vec.get('count'), 'error_total': total.get('error'), 'error_vec': with_vec.get('error')})
    print(json.dumps(rows, indent=2))

if __name__=='__main__':
    main()
