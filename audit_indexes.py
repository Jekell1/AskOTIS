import os, json, sys, time, re
import requests
from typing import Any, Dict, List

"""
Audit all Azure AI Search indexes for potential malformation / data quality issues.

Requires env vars:
  AZURE_SEARCH_ENDPOINT or SEARCH_ENDPOINT
  AZURE_SEARCH_KEY or SEARCH_KEY

Outputs:
 - Human readable summary to stdout
 - JSON dump (full details) to audit_indexes_output.json

Heuristics applied:
 - Empty index (0 docs)
 - Very small index (< 5 docs) flagged as LOW_VOLUME
 - Single-program skew for *facts* style indexes (if top program > 0.90 of docs and total docs >= 10)
 - Missing expected fields (based on name patterns: facts, symbols, xrefs)
 - Vector index with no vector fields
 - Field declared but never populated (sample doc all None / missing) -> POSSIBLY_UNPOPULATED

Safe to extend further.
"""

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION', '2024-05-01-preview')

# Attempt autoload from local.settings.json (same pattern as ask_question)
def _autoload_local_settings(path: str = 'local.settings.json'):
    try:
        import json, os, pathlib
        p = pathlib.Path(path)
        if not p.exists():
            return 0
        data = json.loads(p.read_text(encoding='utf-8'))
        vals = data.get('Values', {}) or {}
        changed = 0
        for k, v in vals.items():
            if k in {'AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY'} and k not in os.environ and isinstance(v, str) and v.strip():
                os.environ[k] = v
                changed += 1
        # Aliases
        if 'AZURE_SEARCH_ENDPOINT' not in os.environ and 'SEARCH_ENDPOINT' in os.environ:
            os.environ['AZURE_SEARCH_ENDPOINT'] = os.environ['SEARCH_ENDPOINT']; changed += 1
        if 'AZURE_SEARCH_KEY' not in os.environ and 'SEARCH_KEY' in os.environ:
            os.environ['AZURE_SEARCH_KEY'] = os.environ['SEARCH_KEY']; changed += 1
        return changed
    except Exception:
        return 0

_autoload_local_settings()

ENDPOINT = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
KEY = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

if not ENDPOINT or not KEY:
    print('Missing AZURE_SEARCH_ENDPOINT/KEY (or SEARCH_* aliases). Set env vars then re-run.', file=sys.stderr)
    sys.exit(1)

SESSION = requests.Session()
SESSION.headers.update({'api-key': KEY, 'Content-Type': 'application/json'})


def _get(url: str) -> dict | None:
    try:
        r = SESSION.get(url, timeout=30)
        if r.status_code == 200:
            return r.json()
        print(f"WARN GET {url} -> {r.status_code}: {r.text[:160]}")
    except Exception as ex:
        print(f"ERROR GET {url}: {ex}")
    return None


def _post(url: str, body: dict) -> dict | None:
    try:
        r = SESSION.post(url, json=body, timeout=45)
        if r.status_code == 200:
            return r.json()
        print(f"WARN POST {url} -> {r.status_code}: {r.text[:200]}")
    except Exception as ex:
        print(f"ERROR POST {url}: {ex}")
    return None


def list_indexes() -> List[str]:
    url = f"{ENDPOINT}/indexes?api-version={API_VERSION}"
    data = _get(url)
    if not data:
        return []
    names = [d['name'] for d in data.get('value', []) if 'name' in d]
    return names


def get_index_schema(name: str) -> dict | None:
    url = f"{ENDPOINT}/indexes/{name}?api-version={API_VERSION}"
    return _get(url)


def get_index_stats(name: str) -> dict | None:
    url = f"{ENDPOINT}/indexes/{name}/stats?api-version={API_VERSION}"
    return _get(url)


def sample_docs(name: str, top: int = 3, select: str | None = None) -> List[dict]:
    url = f"{ENDPOINT}/indexes/{name}/docs/search?api-version={API_VERSION}"
    body: Dict[str, Any] = {"search": "*", "top": top}
    if select:
        body['select'] = select
    data = _post(url, body)
    return data.get('value', []) if data else []


def facet_program(name: str) -> Dict[str, int]:
    # Only if field exists and is facetable
    # Attempt facet even if unsure; handle errors gracefully
    url = f"{ENDPOINT}/indexes/{name}/docs/search?api-version={API_VERSION}"
    body = {"search": "*", "facets": ["program_id,count:50"], "top": 0}
    data = _post(url, body)
    if not data:
        return {}
    aggs = data.get('@search.facets', {})
    prog = aggs.get('program_id') or []
    dist: Dict[str, int] = {}
    for entry in prog:
        v = entry.get('value')
        c = entry.get('count')
        if v is not None and c is not None:
            dist[str(v)] = int(c)
    return dist

EXPECTED_FIELDS_MAP = {
    'facts': {'program_id', 'fact_text', 'fact_vector'},
    'symbols': {'program_id', 'qualified_name', 'name'},
    'xrefs': {'program_id', 'direction', 'kind', 'line'},
}

VECTOR_FIELD_CANDIDATE_SUFFIXES = {'_vector', '_embedding'}


def infer_expected(name: str) -> set[str]:
    name_l = name.lower()
    for key, fields in EXPECTED_FIELDS_MAP.items():
        if key in name_l:
            return fields
    return set()


def detect_vector_fields(schema: dict) -> List[dict]:
    fields = schema.get('fields', []) if schema else []
    vecs = []
    for f in fields:
        if f.get('type') == 'Collection(Edm.Single)' and (f.get('searchIndexes') or f.get('vectorSearchDimensions') or any(f.get('name', '').endswith(s) for s in VECTOR_FIELD_CANDIDATE_SUFFIXES)):
            vecs.append(f)
        elif f.get('vectorSearchDimensions') or f.get('vectorSearchProfile'):  # newer preview syntax
            vecs.append(f)
        else:
            # Heuristic on name only
            if any(f.get('name','').endswith(s) for s in VECTOR_FIELD_CANDIDATE_SUFFIXES):
                vecs.append(f)
    return vecs


def field_present(schema: dict, fname: str) -> bool:
    return any(f.get('name') == fname for f in (schema.get('fields') or []))


def analyze_index(name: str) -> dict:
    schema = get_index_schema(name)
    stats = get_index_stats(name) or {}
    doc_count = stats.get('documentCount') or 0
    storage_size = stats.get('storageSize') or 0
    expected = infer_expected(name)
    vector_fields = detect_vector_fields(schema or {})

    missing_expected = {f for f in expected if not field_present(schema or {}, f)}

    # gather samples (only select subset to limit payload)
    sample = sample_docs(name, top=3)
    # Quick field population check for expected fields
    possibly_unpopulated = []
    for e in expected:
        if e in missing_expected:
            continue
        values = [doc.get(e) for doc in sample if e in doc]
        if values and all(v in (None, '', [], {}) for v in values):
            possibly_unpopulated.append(e)

    program_facet = {}
    if field_present(schema or {}, 'program_id'):
        program_facet = facet_program(name)

    anomalies: List[str] = []
    if doc_count == 0:
        anomalies.append('EMPTY_INDEX')
    elif doc_count < 5:
        anomalies.append('LOW_VOLUME')
    if expected and missing_expected:
        anomalies.append('MISSING_EXPECTED_FIELDS')
    if expected and not vector_fields and any(f.endswith('_vector') for f in expected):
        anomalies.append('NO_VECTOR_FIELDS_DETECTED')
    if possibly_unpopulated:
        anomalies.append('POSSIBLY_UNPOPULATED_FIELDS')
    # Program skew heuristic for facts-like indexes
    if 'facts' in name.lower() and program_facet:
        total_prog_docs = sum(program_facet.values())
        if total_prog_docs > 0:
            top_prog, top_count = max(program_facet.items(), key=lambda x: x[1])
            if total_prog_docs >= 10 and top_count / total_prog_docs >= 0.9:
                anomalies.append('PROGRAM_SKEW_>90%')
    # Symbol index hashed program ids detection
    if 'symbol' in name.lower() and program_facet:
        # Heuristic: if all program_ids look like hex (length >= 8, only 0-9a-f) mark as HASHED_PROGRAM_IDS
        values = list(program_facet.keys())
        if values and all(re.fullmatch(r'[0-9a-fA-F]{8,}', v or '') for v in values):
            anomalies.append('HASHED_PROGRAM_IDS')

    return {
        'name': name,
        'doc_count': doc_count,
        'storage_size': storage_size,
        'expected_fields': sorted(expected),
        'missing_expected_fields': sorted(missing_expected),
        'vector_field_names': [f.get('name') for f in vector_fields],
        'program_distribution': program_facet,
        'sample_docs': sample,
        'possibly_unpopulated_fields': possibly_unpopulated,
        'anomalies': anomalies,
    }


def main():
    start = time.time()
    names = list_indexes()
    if not names:
        print('No indexes returned (or access failure).')
        return
    results = []
    for n in sorted(names):
        try:
            res = analyze_index(n)
        except Exception as ex:
            res = {'name': n, 'error': str(ex), 'anomalies': ['ANALYSIS_EXCEPTION']}
        results.append(res)

    # Print human summary
    print('Index Audit Summary')
    print('===================')
    for r in results:
        line = f"{r['name']}: docs={r.get('doc_count')} anomalies={','.join(r.get('anomalies', [])) or 'NONE'}"
        if r.get('program_distribution'):
            dist = r['program_distribution']
            top_items = sorted(dist.items(), key=lambda x: x[1], reverse=True)[:3]
            pd_summary = ' '.join(f"{k}:{v}" for k,v in top_items)
            line += f" prog_top={pd_summary}"
        print(line)
    malformed = [r for r in results if r.get('anomalies')]
    print('\nMalformed / Anomalous Index Count:', len(malformed))
    for r in malformed:
        print(f" - {r['name']} -> {','.join(r['anomalies'])}")

    out = {
        'endpoint': ENDPOINT,
        'api_version': API_VERSION,
        'generated_utc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        'results': results
    }
    with open('audit_indexes_output.json', 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=2)
    print('\nFull JSON written to audit_indexes_output.json')
    print(f"Done in {time.time()-start:.2f}s")

if __name__ == '__main__':
    main()
