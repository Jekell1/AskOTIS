"""Diagnostics: Verify embedding coverage for new_cobol_copybook_meta.

Outputs summary counts:
  - total documents
  - embedded (has_vector True)
  - not embedded
  - not embedded with EMPTY summary
  - not embedded with NON-EMPTY summary (actionable gap)
  - distinct copybook names vs total (duplicate detection)

Also writes a file diagnostics/copybook_meta_embedding_gaps.txt listing copybook_names
that have a non-empty summary but still lack embeddings (has_vector False).

Usage:
  python diagnostics/verify_copybook_meta_embeddings.py [--limit 0]
    --limit N   Only scan first N docs (pagination) for faster spot checks (0 = all)
"""
from __future__ import annotations
import os, json, sys, requests, math
from typing import List, Dict

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_copybook_meta'
PAGE = 1000
GAPS_PATH = os.path.join('diagnostics','copybook_meta_embedding_gaps.txt')

def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals=json.load(f).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except FileNotFoundError:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY'); sys.exit(2)
    return ep.rstrip('/'), key

def count(ep,key, flt: str | None=None):
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Count failed {r.status_code}: {r.text[:200]}")
    return r.json().get('@odata.count',0)

def fetch_all(ep,key, limit: int=0):
    docs: List[Dict]={}
    total=count(ep,key)
    pulled=0; skip=0
    while skip < total:
        remaining = (limit - pulled) if limit else PAGE
        if limit and remaining <=0: break
        page_size = min(PAGE, remaining) if limit else PAGE
        # summary_vector may be non-retrievable; omit to avoid 400 errors
        body={'search':'*','top':page_size,'skip':skip,'select':'copybook_name,has_vector,summary'}
        r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=120)
        if r.status_code!=200:
            raise RuntimeError(f"Fetch failed {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch: break
        if not isinstance(docs,list): docs=[]
        docs.extend(batch)
        pulled += len(batch)
        skip += page_size
        if limit and pulled >= limit: break
    return docs, total

def main():
    import argparse
    ap=argparse.ArgumentParser()
    ap.add_argument('--limit', type=int, default=0, help='Optional limit for faster spot check (0=all)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve_search()

    total=count(ep,key)
    embedded=count(ep,key,'has_vector eq true')
    not_embedded=total-embedded

    docs, _ = fetch_all(ep,key, args.limit)
    dup_check=set(); dup_count=0
    empty_summary=0
    actionable=[]
    vector_dims=set(); vector_retrievable=True
    for d in docs:
        name=d.get('copybook_name')
        if name in dup_check: dup_count +=1
        else: dup_check.add(name)
        summ=(d.get('summary') or '').strip()
        hv=d.get('has_vector') is True
        if hv:
            # summary_vector not retrievable in current schema; detect absence once
            if 'summary_vector' not in d:
                vector_retrievable=False
            else:
                vec=d.get('summary_vector') or []
                if isinstance(vec,list): vector_dims.add(len(vec))
        if not hv:
            if not summ:
                empty_summary +=1
            else:
                actionable.append(name)

    with_summary_not_embedded = len(actionable)
    gap_pct = (with_summary_not_embedded / total * 100.0) if total else 0.0
    empty_pct = (empty_summary / total * 100.0) if total else 0.0

    summary = {
        'index': INDEX,
        'total_docs': total,
        'embedded_docs': embedded,
        'not_embedded_docs': not_embedded,
        'not_embedded_empty_summary': empty_summary,
        'not_embedded_empty_summary_pct': round(empty_pct,2),
        'not_embedded_with_summary': with_summary_not_embedded,
        'not_embedded_with_summary_pct': round(gap_pct,2),
        'distinct_copybook_names': len(dup_check),
        'duplicate_name_count': dup_count,
    'vector_dimension_values_sample': (sorted(vector_dims)[:5] if vector_retrievable else ['(not retrievable)']),
        'scanned_docs': len(docs),
        'limit_mode': bool(args.limit)
    }

    print(json.dumps(summary, indent=2))

    if actionable:
        os.makedirs('diagnostics', exist_ok=True)
        with open(GAPS_PATH,'w',encoding='utf-8') as f:
            for name in sorted(actionable):
                f.write(name+'\n')
        print(f"Wrote {len(actionable)} actionable (non-empty summary but missing vector) names to {GAPS_PATH}")
        print("Next step suggestion: target just these with ->")
        print("  python search/backfill/backfill_embeddings_copybook_meta.py --force --ids " + ' '.join(sorted(actionable[:10])) + (" ..." if len(actionable)>10 else ''))
    else:
        print('No actionable non-embedded docs with non-empty summary. Remaining gaps are empty summaries.')

if __name__=='__main__':
    main()
