#!/usr/bin/env python3
"""audit_vector_coverage.py
Enumerate ALL Azure Cognitive Search indexes and report embedding/vector coverage.

For each index:
  - Total document count
  - Detected vector fields (fields carrying vectorSearchDimensions)
  - Detected boolean marker field (has_vectors / has_vector / has_embeddings / has_embedding / has_vec)
  - Count of documents where marker == true (if marker present)
  - Percent coverage (if applicable)
  - Any errors encountered

Output formats:
  * Human-readable table
  * Optional JSON (--json) for downstream automation

Credential discovery order (same pattern as other scripts):
  SEARCH_ENDPOINT / AZURE_SEARCH_ENDPOINT
  SEARCH_KEY / AZURE_SEARCH_KEY
  (Falls back to local.settings.json Values if env vars absent)

Usage:
  python audit_vector_coverage.py
  python audit_vector_coverage.py --json > vector_coverage.json

Notes:
  - If an index has vector fields but no obvious boolean marker, coverage% is reported as 'unknown'.
  - Counting docs with a vector purely from the vector field is not practical via filters; a boolean flag is recommended in ingest.
  - Errors per index do not stop the scan; they are reported inline.
"""
from __future__ import annotations
import os, sys, json, argparse, requests, math, time
from typing import List, Dict, Any, Optional

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
MARKER_CANDIDATES = [
    'has_vectors','has_vector','has_embeddings','has_embedding','has_vec'
]

# ---------------- Credential Helpers ----------------

def load_local_settings_values() -> Dict[str,str]:
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json','r',encoding='utf-8') as f:
                return json.load(f).get('Values',{}) or {}
        except Exception:
            return {}
    return {}

def get_credential():
    vals = load_local_settings_values()
    endpoint = os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT')
    key = os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY')
    if not endpoint or not key:
        print('ERROR: Missing SEARCH_ENDPOINT/SEARCH_KEY (or AZURE_* variants).', file=sys.stderr)
        sys.exit(2)
    return endpoint.rstrip('/'), key

# ---------------- REST Helpers ----------------

def list_indexes(endpoint: str, key: str) -> List[Dict[str,Any]]:
    url=f"{endpoint}/indexes?api-version={API_VERSION}"
    r=requests.get(url, headers={'api-key':key}, timeout=30)
    if r.status_code>=300:
        raise SystemExit(f"Failed to list indexes {r.status_code}: {r.text[:300]}")
    return r.json().get('value',[])

def count_docs(endpoint: str, key: str, index: str, flt: Optional[str]=None) -> Dict[str,Any]:
    url=f"{endpoint}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    try:
        r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=45)
        if r.status_code>=300:
            return {'error': f"{r.status_code} {r.text[:180]}"}
        js=r.json()
        return {'count': js.get('@odata.count',0)}
    except Exception as ex:
        return {'error': str(ex)[:200]}

# ---------------- Analysis ----------------

def detect_marker_field(fields: List[Dict[str,Any]]) -> Optional[str]:
    names = {f.get('name') for f in fields}
    for c in MARKER_CANDIDATES:
        if c in names:
            return c
    return None

def collect_index_report(endpoint: str, key: str, idx_meta: Dict[str,Any]) -> Dict[str,Any]:
    name = idx_meta.get('name') or 'UNKNOWN'
    fields = idx_meta.get('fields', [])
    vector_fields = []
    for f in fields:
        # A true vector field must declare dimensions (older preview used 'vectorSearchDimensions', newer stable may use 'dimensions')
        # and type must be a float collection (Collection(Edm.Single)). Some SDK exports include 'vectorSearchProfile': null for ALL
        # fields; we explicitly ignore that key alone to avoid false positives that previously caused every field to appear.
        dims = f.get('vectorSearchDimensions') or f.get('dimensions')
        ftype = f.get('type','')
        if dims and ftype.startswith('Collection(Edm.Single)'):
            vector_fields.append(f.get('name'))
    marker = detect_marker_field(fields)

    total = count_docs(endpoint,key,name)
    total_count = total.get('count')
    total_error = total.get('error')

    vec_count = None
    vec_error = None
    coverage = None
    if marker and not total_error:
        with_vec = count_docs(endpoint,key,name,f"{marker} eq true")
        if 'count' in with_vec:
            vec_count = with_vec['count']
            if total_count not in (None,0):
                coverage = (vec_count / total_count)*100 if total_count else 0
        else:
            vec_error = with_vec.get('error')
    else:
        if vector_fields and not marker:
            coverage = 'unknown-no-marker'

    return {
        'index': name,
        'total_docs': total_count,
        'total_error': total_error,
        'marker_field': marker,
        'vector_fields': vector_fields,
        'vector_docs': vec_count,
        'vector_error': vec_error,
        'coverage_pct': round(coverage,2) if isinstance(coverage,(int,float)) else coverage,
    }

# ---------------- Presentation ----------------

def print_table(rows: List[Dict[str,Any]]):
    # Determine column widths
    headers = ['INDEX','TOTAL','VECTORS','COVERAGE','MARKER','VEC_FIELDS','ERRORS']
    def fmt(r):
        errs = []
        if r.get('total_error'): errs.append('total')
        if r.get('vector_error'): errs.append('vector')
        return ','.join(errs) if errs else ''
    data = []
    for r in rows:
        data.append([
            r.get('index'),
            str(r.get('total_docs','')),
            str(r.get('vector_docs','')),
            (str(r.get('coverage_pct'))+'%') if isinstance(r.get('coverage_pct'),(int,float)) else str(r.get('coverage_pct','')),
            r.get('marker_field') or '',
            ','.join(r.get('vector_fields') or []),
            fmt(r)
        ])
    colw=[len(h) for h in headers]
    for row in data:
        for i,v in enumerate(row):
            colw[i]=max(colw[i], len(v))
    def line(parts):
        return '  '.join(p.ljust(colw[i]) for i,p in enumerate(parts))
    print(line(headers))
    print(line(['-'*w for w in colw]))
    for row in data:
        print(line(row))

# ---------------- Main ----------------

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--json', action='store_true', help='Emit JSON instead of table (or in addition if --both)')
    ap.add_argument('--both', action='store_true', help='Emit both table and JSON')
    args=ap.parse_args()

    endpoint, key = get_credential()
    print(f"Scanning indexes at: {endpoint}")
    start=time.time()
    try:
        indexes = list_indexes(endpoint,key)
    except Exception as ex:
        print(f"Failed to list indexes: {ex}", file=sys.stderr)
        sys.exit(3)
    print(f"Found {len(indexes)} indexes. Collecting stats...")
    rows=[]
    for idx in indexes:
        rows.append(collect_index_report(endpoint,key,idx))
    elapsed=time.time()-start

    if args.json and not args.both:
        print(json.dumps({'endpoint':endpoint,'indexes':rows,'elapsed_seconds':round(elapsed,2)}, indent=2))
    elif args.both:
        print_table(rows)
        print('\nJSON:\n')
        print(json.dumps({'endpoint':endpoint,'indexes':rows,'elapsed_seconds':round(elapsed,2)}, indent=2))
    else:
        print_table(rows)
        print(f"\nElapsed {elapsed:.2f}s")

if __name__=='__main__':
    main()
