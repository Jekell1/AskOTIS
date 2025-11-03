"""Consolidated embedding health report across all vector-enabled indexes.

For each index in the search service:
  1. Fetch index definition to detect vector fields (fields with 'vectorSearchDimensions' or 'dimensions').
  2. Detect presence of a boolean coverage flag field (defaults searched: 'has_vector', '<vector_field>_has_vector').
  3. If coverage flag present, issue two count queries (has_vector eq true / false) to compute coverage.
  4. Estimate storage footprint: (vector_true * dim * 4 bytes) per vector field (float32 assumption).
  5. Emit per-index summary + overall totals.

Limitations:
  - If no has_vector-like flag is present, coverage is reported as unknown.
  - Does not attempt to infer coverage via sampling (can add later with --sample-mode).
  - Vector fields cannot currently be filtered directly; relies on explicit flags.

Usage:
  python probe_embedding_health_all.py --pretty
  python probe_embedding_health_all.py --index-filter new_cobol_ --min-coverage 100

Exit codes:
  0 success (report printed)
  2 if --fail-on-missing and one or more vector-enabled indexes have unknown coverage
  3 if --min-coverage specified and any index with measurable coverage is below threshold
"""
from __future__ import annotations
import os, json, argparse, time, math, sys, requests
from typing import Dict, Any, List

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals = json.load(f).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
    except Exception:
        pass

def resolve_search():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing SEARCH endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def list_indexes(ep, key) -> List[Dict[str,Any]]:
    url=f"{ep}/indexes?api-version={API_VERSION}"
    r=requests.get(url, headers={'api-key':key})
    if r.status_code!=200:
        raise RuntimeError(f"List indexes failed {r.status_code}: {r.text[:200]}")
    data=r.json().get('value',[])
    return data

def get_index(ep,key,name):
    url=f"{ep}/indexes/{name}?api-version={API_VERSION}"
    r=requests.get(url,headers={'api-key':key})
    if r.status_code!=200:
        raise RuntimeError(f"Get index {name} failed {r.status_code}: {r.text[:200]}")
    return r.json()

def count_filter(ep,key,index, filt):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True,'filter':filt}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        # propagate but with short text
        raise RuntimeError(f"Count {index} {filt} failed {r.status_code}: {r.text[:160]}")
    return r.json().get('@odata.count',0)

def detect_vector_fields(index_def: Dict[str,Any]) -> List[Dict[str,Any]]:
    fields=index_def.get('fields',[])
    vectors=[]
    for f in fields:
        # Azure AI Search vector fields documented with 'vectorSearchDimensions' (new) or 'dimensions' (legacy) keys
        dim = f.get('vectorSearchDimensions') or f.get('dimensions')
        if dim and (f.get('type','').startswith('Collection(Edm.Single)') or f.get('type','')=='Collection(Edm.Single)'):
            vectors.append({'name':f['name'],'dimensions':dim})
    return vectors

def find_has_vector_field(index_def: Dict[str,Any]) -> str | None:
    # Common naming patterns
    candidates={'has_vector'}
    for vf in detect_vector_fields(index_def):
        candidates.add(f"{vf['name']}_has_vector")
    field_names={f['name'] for f in index_def.get('fields',[])}
    for c in candidates:
        if c in field_names:
            return c
    return None

def human_bytes(n: float) -> str:
    if n < 1024: return f"{n:.0f}B"
    for unit in ['KB','MB','GB','TB']:
        n/=1024.0
        if n<1024: return f"{n:.2f}{unit}"
    return f"{n:.2f}PB"

def parse_assert(expr: str):
    """Parse assertion expressions of the form:
       index_name>=95   (coverage percent >= 95)
       index_name==100  (exact match)
       index_name>80
    Returns dict {index, op, value} or raises ValueError.
    """
    for op in ('>=','==','>','<=','<'):
        if op in expr:
            idx, val = expr.split(op,1)
            idx=idx.strip(); val=val.strip()
            try:
                num=float(val)
            except ValueError:
                raise ValueError(f'Invalid numeric value in assert: {expr}')
            return {'index':idx,'op':op,'value':num}
    raise ValueError(f'Unsupported assert expression: {expr}')

def check_assert(op, actual, expected):
    if actual is None:
        return False
    if op=='==': return abs(actual-expected) < 1e-6
    if op=='>=': return actual >= expected
    if op=='>': return actual > expected
    if op=='<=': return actual <= expected
    if op=='<': return actual < expected
    return False

def main():
    ap=argparse.ArgumentParser(description='Consolidated embedding health report')
    ap.add_argument('--index-filter', help='Substring filter for index names (optional)')
    ap.add_argument('--pretty', action='store_true')
    ap.add_argument('--fail-on-missing', action='store_true', help='Non-zero exit if any vector index lacks coverage flag')
    ap.add_argument('--min-coverage', type=float, help='Require at least this coverage percent (indexes with measurable coverage)')
    ap.add_argument('--assert', dest='asserts', action='append', help='Assertion expression index>=pct (repeatable)')
    args=ap.parse_args()
    load_settings(); ep,key=resolve_search()

    report={'generated_at_utc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()), 'api_version': API_VERSION, 'indexes': []}
    idx_defs=list_indexes(ep,key)
    unknown_flag_indexes=[]
    below_threshold=[]
    total_vectors=0
    total_storage_bytes=0
    for meta in idx_defs:
        name=meta.get('name') or meta.get('name','')
        if args.index_filter and args.index_filter not in name:
            continue
        try:
            full=get_index(ep,key,name)
        except Exception as e:
            report['indexes'].append({'name':name,'error':str(e)})
            continue
        vector_fields=detect_vector_fields(full)
        if not vector_fields:
            # skip if no vectors
            continue
        has_field=find_has_vector_field(full)
        entry={'name':name,'vector_fields':vector_fields,'has_vector_field':has_field}
        coverage=None
        try:
            total=count_filter(ep,key,name,'*')  # Actually need total doc count: we can do top=0 search without filter
        except Exception:
            # fallback total via search * no filter
            url=f"{ep}/indexes/{name}/docs/search?api-version={API_VERSION}"
            r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'search':'*','top':0,'count':True})
            total = r.json().get('@odata.count',0) if r.status_code==200 else None
        entry['total_docs']=total
        if has_field and total not in (None,0):
            try:
                with_v=count_filter(ep,key,name,f"{has_field} eq true")
                without_v=count_filter(ep,key,name,f"{has_field} eq false")
                coverage= (with_v/ (with_v+without_v) * 100.0) if (with_v+without_v)>0 else 0.0
                entry['with_vector']=with_v
                entry['without_vector']=without_v
                entry['coverage_pct']=round(coverage,2)
                # storage estimate (sum all vector fields) - assume each has same coverage
                for vf in vector_fields:
                    est_vecs = with_v  # assume one vector per doc per field
                    est_bytes = est_vecs * vf['dimensions'] * 4
                    total_vectors += est_vecs
                    total_storage_bytes += est_bytes
                if args.min_coverage is not None and coverage < args.min_coverage:
                    below_threshold.append(name)
            except Exception as e:
                entry['coverage_error']=str(e)
        else:
            entry['coverage_pct']='unknown'
            unknown_flag_indexes.append(name)
        report['indexes'].append(entry)

    report['totals']={
        'approx_total_vectors': total_vectors,
        'approx_storage_bytes': total_storage_bytes,
        'approx_storage_human': human_bytes(total_storage_bytes)
    }
    # Determine exit code conditions
    exit_code=0
    if args.fail_on_missing and unknown_flag_indexes:
        report['fail_reason']='missing_coverage_flag_for_indexes'
        report['missing_flag_indexes']=unknown_flag_indexes
        exit_code=2
    if args.min_coverage is not None and below_threshold:
        report.setdefault('fail_reason','')
        report['below_min_coverage_indexes']=below_threshold
        exit_code=3 if exit_code==0 else exit_code
    # Handle assertions
    if args.asserts:
        assertions=[]; failures=[]
        # Build lookup of coverage by index name
        coverage_map={e['name']: (e.get('coverage_pct') if isinstance(e.get('coverage_pct'), (int,float)) else None) for e in report['indexes']}
        for expr in args.asserts:
            try:
                parsed=parse_assert(expr)
            except ValueError as e:
                failures.append({'expr':expr,'error':str(e)})
                continue
            actual=coverage_map.get(parsed['index'])
            ok=check_assert(parsed['op'], actual, parsed['value'])
            assertions.append({'expr':expr,'actual':actual,'ok':ok})
            if not ok:
                failures.append({'expr':expr,'actual':actual,'expected_op':parsed['op'],'expected_value':parsed['value']})
        report['assertions']=assertions
        if failures:
            report['assert_failures']=failures
            exit_code = 4 if exit_code==0 else exit_code
    print(json.dumps(report, indent=2 if args.pretty else None))
    sys.exit(exit_code)

if __name__=='__main__':
    main()
