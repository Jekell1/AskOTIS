"""Diagnose remaining vector embedding gaps for new_cobol_flow_edges_v2.

Outputs JSON summary:
  - total_docs
  - vectorized_docs (has_vector == true)
  - missing_docs (has_vector != true)
  - percent_missing
  - sample_missing (edge_id list)
  - empty_text_missing (count of missing with blank edge_text)
  - non_empty_missing (count of missing with non-empty edge_text)
  - first_char_distribution (of edge_id among missing)
  - avg_edge_text_len_non_empty_missing
  - elapsed_seconds

This mirrors style of diagnose_symbol_ref_gaps.py for consistent observability.
Pure read-only: no writes or mutations.
"""
from __future__ import annotations
import os, json, time, sys, requests, collections

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_flow_edges_v2'
PAGE=1000
SAMPLE_CAP=250

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing AZURE_SEARCH_ENDPOINT / KEY environment variables.', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def count(ep,key, flt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Count failed {r.status_code}: {r.text[:180]}")
    return r.json().get('@odata.count',0)

def scan_missing(ep,key, max_missing=None):
    """Yield docs where has_vector is not True (missing or false).

    max_missing: optional cap on number of missing docs to scan (for quick diagnostics).
    Prints periodic progress every few pages.
    """
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    skip=0; pages=0; yielded=0
    while True:
        body={'search':'*','top':PAGE,'skip':skip,'select':'edge_id,edge_text,has_vector'}
        r=requests.post(url,headers=headers,json=body,timeout=120)
        if r.status_code!=200:
            raise SystemExit(f"Scan failed {r.status_code}: {r.text[:200]}")
        vals=r.json().get('value',[])
        if not vals:
            break
        pages+=1
        for v in vals:
            if v.get('has_vector') is True:
                continue
            yield v
            yielded+=1
            if max_missing and yielded>=max_missing:
                return
        if pages % 25 == 0:
            print(f"[progress] pages={pages} skip={skip} missing_yielded={yielded}", file=sys.stderr)
        if len(vals)<PAGE:
            break
        skip+=len(vals)

def main():
    import argparse
    ap=argparse.ArgumentParser(description='Diagnose flow edge v2 vector gaps')
    ap.add_argument('--max-missing-scan',type=int,help='Cap missing docs scanned (fast mode)')
    ap.add_argument('--json-out',help='Write JSON to file instead of stdout')
    args=ap.parse_args()
    ep,key=resolve(); start=time.time()
    total=count(ep,key)
    vect=count(ep,key,'has_vector eq true') if total else 0
    missing_est= total - vect
    sample=[]
    first_dist=collections.Counter()
    empty=0; non_empty=0; total_len_non_empty=0
    try:
        for doc in scan_missing(ep,key, max_missing=args.max_missing_scan):
            eid=doc.get('edge_id','')
            if not eid:
                continue
            if len(sample)<SAMPLE_CAP:
                sample.append(eid)
            first_dist[eid[0]]+=1
            text=(doc.get('edge_text') or '').strip()
            if text:
                non_empty+=1
                total_len_non_empty+=len(text)
            else:
                empty+=1
    except KeyboardInterrupt:
        print('[warn] Interrupted during scan; partial results will be reported.', file=sys.stderr)
    # Recompute missing from actual scan (in case of race conditions)
    scanned_missing=empty+non_empty
    percent_missing=(scanned_missing/total*100.0) if total else 0.0
    avg_len=(total_len_non_empty/non_empty) if non_empty else 0.0
    summary={
        'index': INDEX,
        'total_docs': total,
        'vectorized_docs': vect,
        'missing_docs': scanned_missing,
        'percent_missing': round(percent_missing,6),
        'sample_missing': sample,
        'empty_text_missing': empty,
        'non_empty_missing': non_empty,
        'avg_edge_text_len_non_empty_missing': round(avg_len,2),
        'first_char_distribution': first_dist.most_common(),
        'elapsed_seconds': round(time.time()-start,2)
    }
    out=json.dumps(summary,indent=2)
    if args.json_out:
        with open(args.json_out,'w',encoding='utf-8') as f: f.write(out)
    else:
        print(out)

if __name__=='__main__':
    main()
