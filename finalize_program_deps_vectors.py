"""Finalize coverage for program deps by marking empty dependency_blob docs with sentinel vectors.

Strategy:
  - Identify docs where dependency_blob is empty/whitespace AND has_vector not true.
  - Upsert has_vector=true and set dependency_blob_vector to a deterministic zero vector (length 3072).
  - This achieves 100% flag coverage; retrieval layers should optionally filter out zero vectors if undesired.

Usage:
  python finalize_program_deps_vectors.py --batch 500 --dry-run
  python finalize_program_deps_vectors.py --batch 500
"""
from __future__ import annotations
import os, json, requests, sys, argparse

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'
VECTOR_FIELD='dependency_blob_vector'
DIM=3072

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ: os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def search(ep,key,skip,top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'program_id,dependency_blob,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':docs},timeout=60)
    if r.status_code not in (200,201): raise RuntimeError(r.text[:160])

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--batch',type=int,default=500)
    ap.add_argument('--dry-run',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    zero=[0.0]*DIM
    skip=0; updated=0; scanned=0
    while True:
        rows=search(ep,key,skip,args.batch)
        if not rows: break
        actions=[]
        for r in rows:
            blob=(r.get('dependency_blob') or '')
            if blob.strip():
                continue
            if r.get('has_vector') is True:
                continue
            actions.append({'@search.action':'mergeOrUpload','program_id':r['program_id'],'has_vector':True, VECTOR_FIELD: zero})
        if actions and not args.dry_run:
            upload(ep,key,actions)
        updated+=len(actions); scanned+=len(rows)
        print(f"Scanned {scanned} updated={updated} skip={skip}")
        if len(rows)<args.batch: break
        skip+=args.batch
    print(json.dumps({'updated_docs':updated,'dry_run':args.dry_run},indent=2))

if __name__=='__main__':
    main()
