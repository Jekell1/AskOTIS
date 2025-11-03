"""Add symbol_id_global field to new_cobol_data_items if missing and backfill values.

Process:
 1. Fetch index schema; if field absent, PUT updated schema adding the field.
 2. Page through data items to identify docs lacking symbol_id_global.
 3. Compute global id using item_name uppercased (heuristic) via make_global_symbol_id.
 4. Merge updates in batches.

Usage:
  python add_symbol_id_global_to_data_items.py [--limit 50000] [--batch 500]
"""
from __future__ import annotations
import os, json, sys, argparse, requests, time
from id_normalization import make_global_symbol_id

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_data_items'
PAGE=1000


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def ensure_schema(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    if r.status_code!=200:
        print('Index fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    schema=r.json(); fields=schema.get('fields',[])
    names={f['name'] for f in fields}
    if 'symbol_id_global' in names:
        print('[OK] symbol_id_global already present.')
        return
    print('[INFO] Adding symbol_id_global field to index schema.')
    fields.append({'name':'symbol_id_global','type':'Edm.String','searchable':False,'filterable':True,'facetable':True,'retrievable':True})
    put=requests.put(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json={'name':INDEX,'fields':fields,'vectorSearch':schema.get('vectorSearch'),'semantic':schema.get('semantic')})
    if put.status_code not in (200,201):
        print('[ERROR] Failed to update schema', put.status_code, put.text[:300]); sys.exit(1)
    print('[OK] Added symbol_id_global field.')


def fetch_batch(ep,key, skip, top):
    body={'search':'*','top':top,'skip':skip,'select':'item_id,item_name,symbol_id_global'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:240]); sys.exit(1)
    return r.json().get('value',[])


def upload(ep,key, docs):
    if not docs: return 0
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'merge','item_id':d['item_id'],'symbol_id_global':d['symbol_id_global']} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload)
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:240]); sys.exit(1)
    return len(docs)


def main():
    ap=argparse.ArgumentParser(description='Add & backfill symbol_id_global field for data items index.')
    ap.add_argument('--limit',type=int,default=0,help='Max docs to scan (0=all)')
    ap.add_argument('--batch',type=int,default=500)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    ensure_schema(ep,key)
    skip=0; updated=0; scanned=0; start=time.time()
    while True:
        rows=fetch_batch(ep,key, skip, PAGE)
        if not rows: break
        skip+=len(rows)
        todo=[]
        for r in rows:
            if args.limit and scanned>=args.limit: break
            scanned+=1
            if r.get('symbol_id_global'): continue
            name=(r.get('item_name') or '').strip().upper()
            if not name: continue
            gid=make_global_symbol_id(name)
            todo.append({'item_id':r['item_id'],'symbol_id_global':gid})
            if len(todo)>=args.batch:
                updated+=upload(ep,key,todo); todo.clear()
        if todo:
            updated+=upload(ep,key,todo); todo.clear()
        if args.limit and scanned>=args.limit: break
        if len(rows)<PAGE: break
    dur=time.time()-start
    print(f'Backfill complete updated={updated} scanned={scanned} in {dur:.1f}s')

if __name__=='__main__':
    main()
