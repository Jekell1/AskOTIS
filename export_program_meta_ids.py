"""Export program IDs from new_cobol_program_meta to JSON and TXT lists.

Usage:
  python export_program_meta_ids.py [--out-base program_ids]
Produces program_ids.json and program_ids.txt (one per line).
"""
from __future__ import annotations
import os, json, requests, argparse, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def fetch_program_ids(ep,key):
    out=[]; skip=0
    while True:
        body={'search':'*','top':1000,'skip':skip,'select':'program_id'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",
                        headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200:
            raise SystemExit(f"Search failed {r.status_code}: {r.text[:160]}")
        vals=r.json().get('value',[])
        if not vals: break
        for v in vals:
            pid=(v.get('program_id') or '').upper()
            if pid: out.append(pid)
        if len(vals)<1000: break
        skip+=1000
    return sorted(dict.fromkeys(out))


def main():
    ap=argparse.ArgumentParser(description='Export program meta IDs')
    ap.add_argument('--out-base',default='program_ids')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    ids=fetch_program_ids(ep,key)
    print('Export count:', len(ids))
    json.dump(ids, open(f"{args.out_base}.json","w"), indent=2)
    with open(f"{args.out_base}.txt","w") as f:
        for pid in ids:
            f.write(pid+'\n')
    print('Wrote', f"{args.out_base}.json", 'and', f"{args.out_base}.txt")

if __name__=='__main__':
    main()
