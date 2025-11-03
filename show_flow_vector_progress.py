#!/usr/bin/env python3
"""Quick progress report for flow edge vector coverage.

Shows:
  - Total docs (stats)
  - Approx embedded (flag=true) by sampling paged counts until exhaustion or limit
  - Percentage

Fast mode: key-ordered pagination limited by --max-scan (default 120000 to avoid long full scans).

Usage:
  python show_flow_vector_progress.py [--max-scan 500000]
"""
import os, json, requests, argparse

API='2024-07-01'
INDEX='cobol-flow-edges-v2'
ID_FIELD='edge_id'
FLAG_FIELD='has_vector'
PAGE=2000

def load_conf():
    vals={}
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception: pass
    ep=os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key=os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing search config')
    return ep.rstrip('/'), key

def total_docs(ep,key):
    r=requests.get(f"{ep}/indexes/{INDEX}/stats?api-version={API}", headers={'api-key':key})
    if r.status_code!=200:
        raise RuntimeError(r.text[:200])
    return r.json().get('documentCount')

def scan(ep,key,max_scan):
    headers={'api-key':key,'Content-Type':'application/json'}
    last=None; use_order=True
    embedded=0; scanned=0
    while scanned<max_scan:
        filt=None
        if last:
            safe=last.replace("'","''")
            filt=f"{ID_FIELD} gt '{safe}'"
        body={'search':'*','select':f"{ID_FIELD},{FLAG_FIELD}", 'top':PAGE}
        if use_order: body['orderby']=f'{ID_FIELD} asc'
        if filt: body['filter']=filt
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code==400 and use_order and 'orderby' in r.text.lower():
            use_order=False; return scan_skip(ep,key,max_scan)  # fallback
        if r.status_code!=200:
            raise RuntimeError(f"Scan error {r.status_code}: {r.text[:200]}")
        vals=r.json().get('value',[])
        if not vals: break
        for d in vals:
            scanned+=1
            if d.get(FLAG_FIELD): embedded+=1
            if scanned>=max_scan: break
        last=vals[-1].get(ID_FIELD)
        if len(vals)<PAGE: break
    return scanned, embedded

def scan_skip(ep,key,max_scan):
    headers={'api-key':key,'Content-Type':'application/json'}
    skip=0; embedded=0; scanned=0
    while scanned<max_scan:
        body={'search':'*','select':f"{ID_FIELD},{FLAG_FIELD}", 'top':PAGE,'skip':skip}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers=headers, json=body)
        if r.status_code!=200: raise RuntimeError(r.text[:200])
        vals=r.json().get('value',[])
        if not vals: break
        for d in vals:
            scanned+=1
            if d.get(FLAG_FIELD): embedded+=1
            if scanned>=max_scan: break
        skip+=len(vals)
        if skip>=100000 or len(vals)<PAGE: break
    return scanned, embedded

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--max-scan', type=int, default=120000)
    args=ap.parse_args()
    ep,key=load_conf()
    total=total_docs(ep,key)
    scanned, embedded = scan(ep,key,args.max_scan)
    pct = embedded/total*100 if total else 0
    print(f"Flow edge vectors progress")
    print(f"  Total docs:    {total:,}")
    print(f"  Scanned docs:  {scanned:,}")
    print(f"  Embedded flag: {embedded:,}")
    print(f"  Coverage est:  {pct:0.2f}%")
    if scanned<total:
        print("  (Estimation based on scanned subset; full scan requires more time.)")

if __name__=='__main__':
    main()
