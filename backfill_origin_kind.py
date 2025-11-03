"""Backfill origin_kind field in existing indexes without full re-ingest.

Strategy:
  * For each target index, page through documents selecting id + path (or name fields) needed to infer origin_kind.
  * Heuristic: if 'path' ends with .cpy/.copy OR if 'file_path' / 'path' contains such extension, origin_kind='copybook' else 'program'.
  * Batch mergeOrUpload documents setting origin_kind only if missing (unless --force provided).

Limitations:
  * Requires indexes expose a 'path' or similar field. Adjust FIELD_MAP if different.
  * For extremely large indexes consider adding --max-docs to limit run size.

Usage:
  python backfill_origin_kind.py --indexes new_cobol_calls,cobol-symbols --commit
"""
from __future__ import annotations
import os, json, argparse, requests

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

# Index -> (id_field, path_like_field)
FIELD_MAP={
  'new_cobol_calls': ('call_id','file_path'),
  'cobol-symbols': ('item_id','path'),
  'cobol-xrefs': ('xref_id','path'),
  'new_code_chunks': ('chunk_id','path'),
  'new_cobol_flow_edges_v2': ('edge_id','file_id'),  # may not have path; will skip if cannot infer
  'new_cobol_flow_edges': ('edge_id','file_id')
}

COPY_EXTS=('.cpy','.copy')
PAGE=1000

SELECT_TEMPLATE="{id_field},{path_field},origin_kind"

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search_page(ep,key,index,skip,top,select):
    body={'search':'*','top':top,'skip':skip,'select':select}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Search failed {index} {r.status_code} {r.text[:200]}")
    return r.json().get('value',[])

def upload(ep,key,index,docs):
    if not docs:
        return
    url=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload',**d} for d in docs]}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=120)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload failed {index} {r.status_code} {r.text[:200]}")

def infer_origin_kind(path_value:str|None):
    if not path_value:
        return None
    lower=path_value.lower()
    for ext in COPY_EXTS:
        if lower.endswith(ext):
            return 'copybook'
    return 'program'

def backfill_index(ep,key,index,commit:bool, force:bool, max_docs:int|None):
    if index not in FIELD_MAP:
        print(f"[WARN] {index} not in FIELD_MAP, skipping")
        return {'index':index,'processed':0,'updated':0,'skipped':0}
    id_field,path_field=FIELD_MAP[index]
    select=SELECT_TEMPLATE.format(id_field=id_field,path_field=path_field)
    skip=0; processed=0; updated=0; skipped=0; batch=[]
    while True:
        rows=search_page(ep,key,index,skip,PAGE,select)
        if not rows:
            break
        for r in rows:
            processed+=1
            if max_docs and processed>max_docs:
                break
            oid=r.get(id_field)
            path_val=r.get(path_field)
            existing=r.get('origin_kind')
            if existing and not force:
                skipped+=1
                continue
            origin=infer_origin_kind(path_val)
            if not origin:
                skipped+=1
                continue
            batch.append({id_field:oid,'origin_kind':origin})
            if len(batch)>=500:
                if commit:
                    upload(ep,key,index,batch)
                updated+=len(batch)
                batch.clear()
        if max_docs and processed>max_docs:
            break
        skip+=len(rows)
        if len(rows)<PAGE:
            break
    if batch:
        if commit:
            upload(ep,key,index,batch)
        updated+=len(batch)
        batch.clear()
    return {'index':index,'processed':processed,'updated':updated,'skipped':skipped,'committed':commit}

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--indexes',default=','.join(FIELD_MAP.keys()))
    ap.add_argument('--commit',action='store_true')
    ap.add_argument('--force',action='store_true',help='Overwrite existing origin_kind values')
    ap.add_argument('--max-docs',type=int)
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    results=[backfill_index(ep,key,i.strip(),args.commit,args.force,args.max_docs) for i in args.indexes.split(',') if i.strip()]
    print(json.dumps({'results':results},indent=2))
    if not args.commit:
        print('Dry-run (no uploads). Use --commit to apply.')

if __name__=='__main__':
    main()
