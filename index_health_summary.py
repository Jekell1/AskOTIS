"""Produce a consolidated health summary for key COBOL search indexes.

Indexes covered:
  - new_cobol_copybook_usage
  - new_cobol_copybook_meta
  - new_cobol_symbol_refs
  - new_cobol_variable_usage

Metrics:
  total_docs, vectorized_docs (if has_vector field), missing_vectors, coverage_pct
  selected key fields presence

Usage:
  python index_health_summary.py
  python index_health_summary.py --json
"""
from __future__ import annotations
import os, json, requests, argparse
from secrets_loader import load_secrets
from typing import Dict, Any

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEXES=[
    'new_cobol_copybook_usage',
    'new_cobol_copybook_meta',
    'new_cobol_symbol_refs',
    'new_cobol_variable_usage',
    'new_cobol_screen_nodes',
    'new_cobol_ui_paths'
]

SELECT='*'

def load():
    # Backwards compatibility wrapper; prefer load_secrets directly.
    load_secrets()

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def count(ep,key,index,flt=None):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return None
    return r.json().get('@odata.count',0)

def fetch_fields(ep,key,index):
    r=requests.get(f"{ep}/indexes/{index}?api-version={API_VERSION}",headers={'api-key':key},timeout=60)
    if r.status_code!=200: return set()
    return {f.get('name') for f in r.json().get('fields',[])}

def summarize(ep,key,index):
    fields=fetch_fields(ep,key,index)
    total=count(ep,key,index)
    has_vector_field='has_vector' in fields
    vectorized=count(ep,key,index,'has_vector eq true') if has_vector_field else None
    result={
        'index': index,
        'total_docs': total,
        'vectorized_docs': vectorized if has_vector_field else 'N/A',
        'missing_vectors': (total - vectorized) if (has_vector_field and total is not None and vectorized is not None) else 'N/A',
        'coverage_pct': (round(vectorized/total*100,2) if (has_vector_field and total and vectorized is not None) else 'N/A'),
        'key_fields_present': sorted(list(fields & {'has_vector','summary_vector','context_vector','excerpt_vector','usage_summary_vector'}))
    }
    return result

def main():
    ap=argparse.ArgumentParser(description='Index health summary')
    ap.add_argument('--json',action='store_true')
    args=ap.parse_args(); load(); ep,key=resolve()
    reports=[summarize(ep,key,i) for i in INDEXES]
    if args.json:
        print(json.dumps({'indexes':reports},indent=2))
        return
    print('INDEX HEALTH SUMMARY')
    for r in reports:
        print(f"- {r['index']}: total={r['total_docs']} vectors={r['vectorized_docs']} missing={r['missing_vectors']} coverage={r['coverage_pct']} fields={','.join(r['key_fields_present'])}")

if __name__=='__main__':
    main()
