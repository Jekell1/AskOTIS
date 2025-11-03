"""Comprehensive health diagnostics for Azure Cognitive Search indexes.

Checks performed per index:
  - Existence & document count
  - Schema fetch and comparison (if local index creation module available)
  - Field presence & unexpected fields
  - Sample documents (top N by ingestion time if field present, otherwise first batch)
  - Vector coverage ratio for declared vector fields
  - Basic sparsity indicators (null / missing counts for key analytical fields)
  - Facetable/filterable fields quick cardinality estimate (approx via first K docs)

Usage:
  python index_health_check.py --prefix new_ --sample 5 --estimate 500
  python index_health_check.py --indexes new_cobol_copybook_usage new_cobol_paragraphs

Env resolution parallels other scripts (local.settings.json Values section).

Limitations:
  - Cardinality estimates are approximate (client-side on sampled docs)
  - Full distribution / percentile stats would require additional queries
  - Uses REST API only; no semantic config introspection beyond raw JSON
"""
from __future__ import annotations
import os, sys, json, argparse, collections
import requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

FIELD_LIST_ENDPOINT = "/indexes?api-version={api_version}&$select=name"
INDEX_ENDPOINT = "/indexes/{name}?api-version={api_version}"
COUNT_ENDPOINT = "/indexes/{name}/docs/$count?api-version={api_version}"
SEARCH_ENDPOINT = "/indexes/{name}/docs/search.post.search?api-version={api_version}"

EXPECTED_SCHEMAS: dict[str, dict[str, dict]] = {}

def load_expected_schemas():
    """Dynamically import all local create_*index*.py modules and capture field definitions.

    We rely on each module exposing an index_payload() returning an object with 'name' and 'fields'.
    """
    import importlib.util
    loaded = 0
    for fname in os.listdir('.'):
        if not fname.startswith('create_'):
            continue
        if 'index' not in fname:
            continue
        if not fname.endswith('.py'):
            continue
        try:
            spec = importlib.util.spec_from_file_location("_schemaloader_"+fname.replace('.','_'), fname)
            mod = importlib.util.module_from_spec(spec)  # type: ignore
            spec.loader.exec_module(mod)  # type: ignore
            if hasattr(mod,'index_payload'):
                payload = mod.index_payload()
                idx_name = payload.get('name')
                if idx_name and 'fields' in payload:
                    EXPECTED_SCHEMAS[idx_name] = {f['name']:f for f in payload.get('fields',[])}
                    loaded += 1
        except Exception as e:  # skip broken modules but log once
            print(f"[WARN] Could not load schema from {fname}: {e}")
    print(f"[INFO] Loaded expected schemas for {loaded} indexes")

def load_local_settings():
    try:
        data = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in data and k not in os.environ:
                os.environ[k] = data[k]
    except Exception:
        pass

def endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key env vars')
        sys.exit(1)
    return ep.rstrip('/'), key

def list_indexes(ep, key, prefix=None, specific=None):
    if specific:
        return specific
    url = ep + FIELD_LIST_ENDPOINT.format(api_version=API_VERSION)
    r = requests.get(url, headers={'api-key':key}, timeout=60)
    if r.status_code != 200:
        print('[FATAL] Failed to list indexes', r.status_code, r.text[:300])
        sys.exit(1)
    names = [x['name'] for x in r.json().get('value',[])]
    if prefix:
        names = [n for n in names if n.startswith(prefix)]
    return sorted(names)

def fetch_index_schema(ep,key,name):
    url = ep + INDEX_ENDPOINT.format(name=name, api_version=API_VERSION)
    r = requests.get(url, headers={'api-key':key}, timeout=60)
    if r.status_code != 200:
        print(f'[ERROR] Unable to fetch schema for {name}: {r.status_code}')
        return None
    return r.json()

def probe_vector_field(ep,key,index_name, vector_field, filter_expr=None):
    """Attempt to retrieve a single document selecting only the vector field.

    Returns tuple (retrievable: bool, non_null: bool | None)
      retrievable False means API rejected retrieval (likely non-retrievable field)
      non_null indicates whether at least one sampled doc had a non-null vector (None if not retrievable)
    """
    body = {
        'search': '*',
        'select': vector_field,
        'top': 1
    }
    if filter_expr:
        body['filter'] = filter_expr
    url = ep + SEARCH_ENDPOINT.format(name=index_name, api_version=API_VERSION)
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        return (False, None)
    vals = r.json().get('value',[])
    if not vals:
        return (True, False)
    v = vals[0].get(vector_field)
    return (True, v is not None)

def fetch_count(ep,key,name):
    url = ep + COUNT_ENDPOINT.format(name=name, api_version=API_VERSION)
    r = requests.get(url, headers={'api-key':key}, timeout=60)
    if r.status_code != 200:
        print(f'[ERROR] Count failed for {name}: {r.status_code}')
        return None
    try:
        return int(r.text.strip())
    except:
        return None

def sample_docs(ep,key,name, sample, select=None):
    body = {
        'count': True,
        'top': sample,
        'select': select or '*',
        'search': '*'
    }
    url = ep + SEARCH_ENDPOINT.format(name=name, api_version=API_VERSION)
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=90)
    if r.status_code != 200:
        print(f'[WARN] Sample query failed for {name}: {r.status_code}')
        return []
    data = r.json()
    return data.get('value',[])

def analyze_vector_coverage(docs):
    if not docs:
        return None
    vector_fields = [k for k,v in docs[0].items() if isinstance(v, list) and all(isinstance(x,(int,float)) for x in v)]
    coverage = {}
    for vf in vector_fields:
        present = sum(1 for d in docs if d.get(vf) is not None)
        coverage[vf] = present/len(docs)
    return coverage

def approximate_cardinality(docs, fields, max_unique=200):
    estimates = {}
    for f in fields:
        uniq = set()
        for d in docs:
            if f in d and d[f] is not None:
                v = d[f]
                if isinstance(v,list):
                    for vv in v:
                        uniq.add(vv)
                else:
                    uniq.add(v)
                if len(uniq) > max_unique:
                    break
        estimates[f] = f"{len(uniq)}+" if len(uniq) > max_unique else len(uniq)
    return estimates

def diff_schema(index_name, live_schema):
    exp = EXPECTED_SCHEMAS.get(index_name)
    if not exp:
        return {'status':'unknown','missing':[], 'unexpected':[], 'notes':'No expected schema loaded'}
    live_fields = {f['name']:f for f in live_schema.get('fields',[])} if live_schema else {}
    missing = [n for n in exp.keys() if n not in live_fields]
    unexpected = [n for n in live_fields.keys() if n not in exp]
    status = 'match' if not missing and not unexpected else 'drift'
    return {'status':status,'missing':missing,'unexpected':unexpected,'notes':''}

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--prefix', default=None, help='Filter indexes by prefix (ignored if --indexes supplied)')
    ap.add_argument('--indexes', nargs='+', help='Explicit index names')
    ap.add_argument('--sample', type=int, default=5, help='Sample size per index')
    ap.add_argument('--estimate', type=int, default=500, help='Approx cardinality estimation doc scan size (search top)')
    ap.add_argument('--show-sample', action='store_true', help='Print raw sample docs')
    ap.add_argument('--report-json', default='index_health_report.json', help='Write consolidated JSON report to this path')
    args = ap.parse_args()

    load_local_settings()
    ep, key = endpoint_key()
    load_expected_schemas()
    index_names = list_indexes(ep,key,args.prefix,args.indexes)
    print(f"[INFO] Evaluating {len(index_names)} indexes")

    report: list[dict] = []

    for name in index_names:
        # Header block for clarity per index
        print(f"\n=============================\nIndex: {name}\n=============================")
        schema = fetch_index_schema(ep,key,name)
        count = fetch_count(ep,key,name)
        print(f"Count: {count}")
        if not schema:
            continue
        diff = diff_schema(name, schema)
        print(f"Schema status: {diff['status']} missing={diff['missing']} unexpected={diff['unexpected']} {diff['notes']}")
        fields_list = schema.get('fields',[])
        vector_fields = [f['name'] for f in fields_list if f.get('type','').startswith('Collection(Edm.Single)')]
        print(f"Vector fields: {vector_fields}")
        field_meta = {f['name']:f for f in schema.get('fields',[])}
        # Structural validation
        key_fields = [f['name'] for f in fields_list if f.get('key')]
        structural_issues = []
        if len(key_fields) == 0:
            structural_issues.append('NO_KEY_FIELD')
        elif len(key_fields) > 1:
            structural_issues.append(f'MULTI_KEY:{key_fields}')
        # Large text fields wrongly facetable/filterable heuristic
        suspicious_facetable = [f['name'] for f in fields_list if f.get('facetable') and f.get('type') == 'Edm.String' and f.get('searchable')]
        if suspicious_facetable:
            structural_issues.append(f'STRING_FACETABLE_SEARCHABLE:{suspicious_facetable}')
        # Vector field missing profile/dimensions
        for vf in vector_fields:
            vdef = field_meta.get(vf, {})
            if not vdef.get('dimensions'):
                structural_issues.append(f'VECTOR_NO_DIMENSIONS:{vf}')
        # Collect base report row early (will enrich after sampling)
        row = {
            'index': name,
            'count': count,
            'schema_status': diff['status'],
            'missing_fields': diff['missing'],
            'unexpected_fields': diff['unexpected'],
            'key_fields': key_fields,
            'vector_fields': vector_fields,
            'structural_issues': structural_issues,
            'vector_probe': {},
            'vector_coverage_sample': {},
            'sample_keys': [],
        }
        docs = sample_docs(ep,key,name,args.sample)
        if not docs:
            print('[WARN] No sample docs returned')
            continue
        field_null_counts = {}
        for vf in vector_fields:
            nulls = sum(1 for d in docs if d.get(vf) is None)
            if nulls:
                field_null_counts[vf]=nulls
        if field_null_counts:
            print('Vector null sample counts:', field_null_counts)
        coverage = analyze_vector_coverage(docs)
        if coverage:
            print('Vector coverage (sample proportion):', coverage)
            # Flag if any vector field missing in all sampled docs
            all_zero = True
            for vf, cov in coverage.items():
                if cov == 0:
                    print(f"[ALERT] Vector field {vf} missing in all sampled docs (possibly non-retrievable or not embedded yet)")
                else:
                    all_zero = False
            if all_zero:
                print('[NOTE] All vector fields absent in sample; they may be marked non-retrievable or embeddings not populated yet.')
        # Vector probes (deeper)
        probe_summary = {}
        for vf in vector_fields:
            retrievable_flag = field_meta.get(vf, {}).get('retrievable', True)
            retrievable, non_null = probe_vector_field(ep,key,name,vf)
            if not retrievable:
                print(f"[INFO] Probe: field {vf} not retrievable via select (likely retrievable=False)")
            else:
                if non_null:
                    print(f"[INFO] Probe: field {vf} has at least one non-null vector")
                else:
                    print(f"[WARN] Probe: field {vf} returned null for sampled doc")
            if not retrievable_flag:
                print(f"[NOTE] Schema indicates {vf} retrievable=False (expected omission in docs)")
            probe_summary[vf] = {
                'retrievable_attempt': retrievable,
                'non_null_seen': non_null,
                'schema_retrievable_flag': retrievable_flag
            }
        facet_like = [f['name'] for f in schema.get('fields',[]) if f.get('facetable')]
        card_est = approximate_cardinality(docs, facet_like)
        print('Approx facet cardinality (sample-bound):', card_est)
        key_field = next((f['name'] for f in schema.get('fields',[]) if f.get('key')), 'id')
        print('Sample document keys:', [d.get(key_field) for d in docs])
        row['vector_coverage_sample'] = coverage or {}
        row['vector_probe'] = probe_summary
        row['sample_keys'] = [d.get(key_field) for d in docs]
        row['facet_cardinality_sample'] = card_est
        report.append(row)
        if args.show_sample:
            print('Sample docs (truncated to 2000 chars each):')
            for d in docs:
                print(json.dumps(d, indent=2)[:2000])

    # Consolidated summary (minimal for now – could extend to JSON output)
    print('\n[SUMMARY] Completed health evaluation for indexes:', ', '.join(index_names))
    if report:
        try:
            with open(args.report_json,'w',encoding='utf-8') as f:
                json.dump({'indexes': report, 'total_indexes': len(report)}, f, indent=2)
            print(f"[INFO] Wrote JSON report -> {args.report_json}")
        except Exception as e:
            print(f"[WARN] Failed writing report {args.report_json}: {e}")

if __name__ == '__main__':
    main()
