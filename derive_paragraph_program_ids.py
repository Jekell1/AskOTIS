"""Derive distinct program IDs from new_cobol_paragraphs index.
Writes paragraph_program_ids.json / .txt.
"""
import os, json, requests, argparse, sys
API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_paragraphs'

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
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def fetch_program_ids(ep,key):
    out=set(); skip=0
    while True:
        body={'search':'*','top':1000,'skip':skip,'select':'program_id'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code!=200:
            print(f"[WARN] paragraph search failed {r.status_code} skip={skip}: {r.text[:160]}")
            break
        vals=r.json().get('value',[])
        if not vals: break
        for v in vals:
            pid=(v.get('program_id') or '').upper()
            if pid: out.add(pid)
        if len(vals)<1000: break
        skip+=1000
        if skip>=100000:
            print('[INFO] reached skip cap ~100k; stopping early')
            break
    return sorted(out)

def main():
    ap=argparse.ArgumentParser(description='Derive program IDs from paragraphs index')
    ap.add_argument('--out-base',default='paragraph_program_ids')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    ids=fetch_program_ids(ep,key)
    print('Paragraph-derived program count:', len(ids))
    json.dump(ids, open(f"{args.out_base}.json","w"), indent=2)
    with open(f"{args.out_base}.txt","w") as f:
        for pid in ids:
            f.write(pid+'\n')
    print('Wrote', f"{args.out_base}.json", 'and', f"{args.out_base}.txt")

if __name__=='__main__':
    main()
