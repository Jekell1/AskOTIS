"""Probe top UI paths with new extended fields.

Usage:
  python probe_ui_paths.py --q "loan inquiry" --k 10
  python probe_ui_paths.py --common --k 5

If --common provided, sorts by frequency_score desc.
"""
import os, json, argparse, requests
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_ui_paths'

# Some fields may not exist yet if index not recreated; keep a conservative select.
SEL='path_id,root_program_id,leaf_program_id,program_sequence_json,length,ui_program_count'

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,q,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':q or '*','top':top,'select':SEL}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Search fail {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--q',default='*')
    ap.add_argument('--k',type=int,default=10)
    ap.add_argument('--common',action='store_true',help='Sort by frequency_score desc')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    rows=search(ep,key,args.q,args.k*2)
    if args.common:
        rows=sorted(rows,key=lambda r:(r.get('length') or 0, r.get('ui_program_count') or 0),reverse=True)
    rows=rows[:args.k]
    out=[]
    for r in rows:
        try:
            seq=json.loads(r.get('program_sequence_json') or '[]')
        except Exception:
            seq=[]
        preview=' -> '.join(seq[:8]) + (' ...' if len(seq)>8 else '')
        out.append({
            'path_id': r.get('path_id'),
            'start': r.get('root_program_id'),
            'end': r.get('leaf_program_id'),
            'length': r.get('length'),
            'ui_program_count': r.get('ui_program_count'),
            # frequency fields omitted (schema not yet extended)
            # guard_summary omitted if field not in index
            'preview': preview
        })
    print(json.dumps(out,indent=2))

if __name__=='__main__':
    main()
