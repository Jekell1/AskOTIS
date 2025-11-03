"""Fetch sample documents from new_cobol_program_meta via REST for debugging root selection.

Usage:
  python quick_program_meta_sample.py --top 5
  python quick_program_meta_sample.py --filter "program_role eq 'UI'"
"""
import os, sys, json, argparse, requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_meta'

FIELDS = [
  'program_id','program_role','ui_flag','ui_path_participant','reach_out_size','reach_in_size',
  'centrality_score','risk_flag','external_callee_count'
]

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
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def fetch(ep, key, top:int, flt:str|None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':top,'queryType':'simple','select':','.join(FIELDS)}
    if flt:
        body['filter']=flt
    r=requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body)
    if r.status_code!=200:
        print('Error', r.status_code, r.text[:400])
        return []
    return r.json().get('value',[])

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--top', type=int, default=5)
    ap.add_argument('--filter')
    args=ap.parse_args()
    load_local_settings()
    ep,key=resolve()
    docs=fetch(ep,key,args.top,args.filter)
    for d in docs:
        print(json.dumps(d, indent=2)[:1000])

if __name__=='__main__':
    main()
