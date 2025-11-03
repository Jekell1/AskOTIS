"""Diagnose remaining symbol reference embedding gaps.

Outputs JSON summary of:
  - total_docs
  - missing_docs (has_vector false or null)
  - percent_missing
  - sample_missing (first N ref_ids)
  - first_char_distribution for missing set
  - non_hex_first_char_count / list (chars outside 0-9a-f)
  - average_excerpt_length_missing

No writes/embeddings are performed. Pure read-only diagnostics.
"""
from __future__ import annotations
import os, json, requests, math, sys, collections, time

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
HAS_FIELD='has_vector'

HEX=set('0123456789abcdef')

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing AZURE_SEARCH_ENDPOINT / KEY environment variables.')
        sys.exit(1)
    return ep.rstrip('/'), key

def count_docs(ep,key,flt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('[ERROR] Count failed',r.status_code,r.text[:200])
        sys.exit(2)
    return r.json().get('@odata.count',0)

def fetch_missing(ep,key, top=1000, limit=None):
    """Return iterator of missing docs (ref_id, excerpt_text)."""
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    missing_filter=f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))"
    skip=0
    fetched=0
    while True:
        body={'search':'*','top':top,'skip':skip,'filter':missing_filter,'select':'ref_id,excerpt_text','orderby':'ref_id asc'}
        r=requests.post(url,headers=headers,json=body,timeout=60)
        if r.status_code!=200:
            raise SystemExit(f"Fetch missing failed {r.status_code}: {r.text[:200]}")
        vals=r.json().get('value',[])
        if not vals:
            break
        for v in vals:
            yield v
            fetched+=1
            if limit and fetched>=limit:
                return
        skip += len(vals)


def main():
    ep,key=resolve()
    start=time.time()
    total=count_docs(ep,key)
    missing=count_docs(ep,key,f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))")
    percent=(missing/total*100) if total else 0.0

    sample_cap=200
    sample=[]
    dist=collections.Counter()
    non_hex=set()
    total_len=0
    counted=0

    for doc in fetch_missing(ep,key, top=1000, limit=None):
        rid=doc.get('ref_id','')
        if not rid:
            continue
        first=rid[0].lower()
        dist[first]+=1
        if first not in HEX:
            non_hex.add(first)
        if len(sample)<sample_cap:
            sample.append(rid)
        text=(doc.get('excerpt_text') or '')
        total_len+=len(text)
        counted+=1

    avg_len = (total_len/counted) if counted else 0
    summary={
        'total_docs': total,
        'missing_docs': missing,
        'percent_missing': round(percent,6),
        'avg_excerpt_length_missing': round(avg_len,2),
        'first_char_distribution': dist.most_common(),
        'non_hex_first_chars': sorted(non_hex),
        'non_hex_first_char_count': sum(dist[c] for c in non_hex),
        'sample_missing': sample,
        'elapsed_seconds': round(time.time()-start,2)
    }
    print(json.dumps(summary,indent=2))

if __name__=='__main__':
    main()
