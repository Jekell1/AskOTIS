"""Probe new_cobol_program_deps for empty dependency_blob documents.

Counts docs where dependency_blob is null/empty/whitespace and reports how many already have vectors.
Helps explain residual coverage gap.
"""
from __future__ import annotations
import os, json, requests, sys

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def fetch(ep,key, skip, top):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body={'search':'*','top':top,'skip':skip,'select':'program_id,dependency_blob,has_vector'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: raise RuntimeError(r.text[:200])
    return r.json().get('value',[])

def count_total(ep,key):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
    if r.status_code!=200: raise RuntimeError(r.text[:160])
    return r.json().get('@odata.count',0)

def main():
    load(); ep,key=resolve()
    total=count_total(ep,key)
    skip=0; page=512
    empties=0; empties_flagged=0; nonempty=0
    while True:
        rows=fetch(ep,key,skip,page)
        if not rows: break
        for r in rows:
            blob=(r.get('dependency_blob') or '')
            if not blob.strip():
                empties+=1
                if r.get('has_vector') is True:
                    empties_flagged+=1
            else:
                nonempty+=1
        if len(rows)<page: break
        skip+=page
    remaining=total-(empties+nonempty)
    print(json.dumps({
        'index':INDEX,
        'total_docs':total,
        'empty_blob_docs':empties,
        'empty_blob_with_vector':empties_flagged,
        'non_empty_docs':nonempty,
        'unaccounted_docs':remaining,
        'empty_share_pct': round(empties/total*100,2) if total else 0.0
    }, indent=2))

if __name__=='__main__':
    main()
