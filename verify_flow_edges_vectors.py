#!/usr/bin/env python3
"""Verify vector coverage for cobol-flow-edges-v2.

Reports:
  - Total docs
  - Docs with has_vector true
  - Docs missing vector (flag false or null vector field)
  - Sample of 3 embedded docs (edge_id + length)

Usage:
  python verify_flow_edges_vectors.py [--sample 5]
"""
import os, json, requests, argparse

API='2024-07-01'
INDEX='cobol-flow-edges-v2'
ID_FIELD='edge_id'
FLAG_FIELD='has_vector'
PAGE=1000

def load_env():
    vals={}
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception: pass
    ep=os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def count_docs(ep, key):
    url=f"{ep}/indexes/{INDEX}/stats?api-version={API}"
    r=requests.get(url, headers={'api-key':key})
    if r.status_code!=200:
        raise RuntimeError(f"Stats error {r.status_code}: {r.text[:200]}")
    return r.json().get('documentCount')

def stream_all(ep, key, select_fields: str):
    """Key-ordered pagination to avoid 100k skip cap."""
    headers={'api-key':key,'Content-Type':'application/json'}
    last_id=None
    use_order=True
    while True:
        filter_expr=None
        if last_id:
            safe=last_id.replace("'","''")
            filter_expr=f"{ID_FIELD} gt '{safe}'"
        body={'search':'*','select':select_fields,'top':PAGE}
        if use_order:
            body['orderby']=f'{ID_FIELD} asc'
        if filter_expr:
            body['filter']=filter_expr
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code==400 and use_order and 'orderby' in r.text.lower():
            # fallback to skip mode
            use_order=False
            yield from legacy_skip(ep, key, select_fields)
            return
        if r.status_code!=200:
            raise RuntimeError(f"Search error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch:
            break
        for d in batch:
            yield d
        last_id=batch[-1].get(ID_FIELD)
        if len(batch)<PAGE:
            break

def legacy_skip(ep, key, select_fields: str):
    headers={'api-key':key,'Content-Type':'application/json'}
    skip=0
    while True:
        body={'search':'*','select':select_fields,'top':PAGE,'skip':skip}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code!=200:
            raise RuntimeError(f"Skip search error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            yield d
        skip+=len(batch)
        if skip>=100000:
            print('Reached 100k skip limit early (legacy mode).')
            break
        if len(batch)<PAGE: break

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--sample', type=int, default=3)
    args=ap.parse_args()
    ep,key=load_env()
    total=count_docs(ep,key)
    embedded=missing=0
    scanned=0
    select_fields=f"{ID_FIELD},{FLAG_FIELD}"
    for d in stream_all(ep,key,select_fields):
        scanned+=1
        if d.get(FLAG_FIELD):
            embedded+=1
        else:
            missing+=1
    print(f"Total docs (stats): {total}")
    print(f"Scanned docs: {scanned}")
    print(f"Embedded (has_vector): {embedded}")
    print(f"Missing (flag false/absent): {missing}")
    if total:
        pct=embedded/total*100.0
        print(f"Coverage: {pct:.2f}%")
    # Sample embedded docs with vector length
    # Sample embedded docs (cannot retrieve vector field because it's non-retrievable)
    sample=[]
    # Use a filtered search to get some has_vector docs
    headers={'api-key':key,'Content-Type':'application/json'}
    body={'search':'*','filter':f"{FLAG_FIELD} eq true",'select':ID_FIELD,'top':args.sample}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
    if r.status_code==200:
        sample=r.json().get('value',[])
    else:
        print('Sample fetch error', r.status_code, r.text[:150])
    if sample:
        print(f"Sample {len(sample)} embedded doc IDs:")
        for d in sample:
            print(f"  {d.get(ID_FIELD)} (vector stored, not retrievable)")
    else:
        print('No embedded docs sampled.')

if __name__=='__main__':
    main()
