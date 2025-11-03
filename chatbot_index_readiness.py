#!/usr/bin/env python3
"""Assess readiness of Azure AI Search indexes for the COBOL RAG chatbot.

For each target index, the script reports:
  - Exists (Y/N)
  - Document count (from stats API)
  - Vector field detected (first Collection(Edm.Single))
  - Vector dimensions
  - has_vector field present (Y/N)
  - Vector coverage (True docs / total) via facet on has_vector (if available)
  - VectorSearch profiles / algorithms summary
  - Primary key field name
  - Representative text field (first searchable Edm.String preferring names containing 'text'/'content'/'snippet')
  - Readiness classification: READY | PARTIAL | ACTION
  - Rationale

Classification rules (heuristic):
  * READY   : Required vector index has vector field AND coverage >= 95% (or no has_vector flag but vector field present and docs > 0 and not flagged missing). Non-vector-secondary indexes (rare) also READY if intentionally lexical only.
  * PARTIAL : Vector schema present but coverage between 1% and 95% OR coverage unknown (no has_vector) while embedding likely still running.
  * ACTION  : Required index missing OR vector field missing OR coverage == 0 for required vectorized index.

Usage:
  python chatbot_index_readiness.py              # default target set
  python chatbot_index_readiness.py --indexes code-chunks,cobol-paragraphs
  python chatbot_index_readiness.py --json > readiness.json

Environment (env vars or local.settings.json Values):
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
"""
from __future__ import annotations
import os, json, argparse, sys, math, textwrap
from typing import Dict, Any, List, Tuple
import requests

API_VERSION = "2024-07-01"

# Expected indexes & whether vectorization is required for optimal chatbot retrieval
EXPECTED_INDEXES = {
    "code-chunks": True,
    "cobol-paragraphs": True,
    "cobol-symbols": True,
    "cobol-calls": True,
    "cobol-xrefs": True,
    "cobol-flow-edges-v2": True,
    "cobol-facts": True,
    # Optional / supportive indexes (mark vector desired but not strictly blocking answer synthesis)
    "cobol-copybooks": False,
    "cobol-files": False,
    "cobol-routine-aliases": False,
}


def load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    ep = first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT')
    key = first('AZURE_SEARCH_KEY','SEARCH_KEY')
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def get_index_list(ep: str, key: str) -> List[str]:
    r = requests.get(f"{ep}/indexes?api-version={API_VERSION}", headers={'api-key':key}, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Index list failed {r.status_code}: {r.text[:300]}")
    return [it.get('name') for it in r.json().get('value', [])]


def get_index(ep: str, key: str, name: str) -> Dict[str,Any]:
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key':key}, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Fetch index {name} failed {r.status_code}: {r.text[:200]}")
    return r.json()


def get_index_stats(ep: str, key: str, name: str) -> Dict[str,Any]:
    r = requests.get(f"{ep}/indexes/{name}/stats?api-version={API_VERSION}", headers={'api-key':key}, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Stats {name} failed {r.status_code}: {r.text[:200]}")
    return r.json()


def facet_has_vector(ep: str, key: str, name: str) -> Tuple[int,int]:
    # Returns (true_count, doc_count_seen_via_facet) or ( -1, -1 ) if not applicable
    body = {
        "search": "*",
        "top": 0,
        "facets": ["has_vector,count:2"]
    }
    r = requests.post(f"{ep}/indexes/{name}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code != 200:
        return -1, -1
    facets = r.json().get('facets', {})
    hv = facets.get('has_vector')
    if not hv:
        return -1, -1
    true_count = 0
    total_bucket = 0
    for bucket in hv:
        val = bucket.get('value')
        cnt = bucket.get('count',0)
        total_bucket += cnt
        if val is True:
            true_count = cnt
    return true_count, total_bucket


def analyze_index(defn: Dict[str,Any]) -> Dict[str,Any]:
    fields = defn.get('fields', [])
    vector_field = None; vector_dims = None
    has_vector_flag = any(f.get('name')=='has_vector' and f.get('type')=='Edm.Boolean' for f in fields)
    key_field = None
    text_field = None
    # Choose text field pref with heuristics
    def is_text_candidate(f):
        return f.get('type')=='Edm.String' and f.get('searchable')
    for f in fields:
        if f.get('key'):
            key_field = f.get('name')
        if f.get('type') == 'Collection(Edm.Single)' and f.get('searchable') and not vector_field:
            vector_field = f.get('name')
            vector_dims = f.get('dimensions')
    # text field heuristic
    prioritized = sorted([f for f in fields if is_text_candidate(f)], key=lambda x: (0 if any(t in x.get('name','').lower() for t in ('text','content','snippet')) else 1, len(x.get('name',''))))
    if prioritized:
        text_field = prioritized[0].get('name')
    return {
        'vector_field': vector_field,
        'vector_dims': vector_dims,
        'has_vector_flag': has_vector_flag,
        'key_field': key_field,
        'text_field': text_field,
        'vector_profiles': defn.get('vectorSearch', {}).get('profiles', []) or defn.get('vectorSearch', {}).get('algorithmConfigurations', []),
        'vector_algorithms': defn.get('vectorSearch', {}).get('algorithms', []) or defn.get('vectorSearch', {}).get('algorithmConfigurations', [])
    }


def classify(name: str, required: bool, doc_count: int, vector_field: str|None, coverage_ratio: float|None) -> Tuple[str,str]:
    if doc_count == 0:
        return ("ACTION" if required else "PARTIAL", "No documents yet")
    if required and not vector_field:
        return ("ACTION", "Missing vector field for required index")
    if vector_field:
        if coverage_ratio is None:
            # Unknown coverage; treat as partial while embedding in progress
            return ("PARTIAL", "Vector coverage unknown (no has_vector facet) – verify embeddings")
        if coverage_ratio == 0:
            return ("ACTION", "0% vector coverage")
        if coverage_ratio < 0.95:
            return ("PARTIAL", f"Vector coverage {coverage_ratio*100:.1f}% (<95%)")
        return ("READY", f"Vector coverage {coverage_ratio*100:.1f}%")
    # Not required and no vector field: acceptable
    return ("READY" if not required else "ACTION", "Lexical-only acceptable" if not required else "Missing vector field")


def sample_vector_coverage(ep: str, key: str, name: str, vector_field: str, sample_size: int = 120) -> float | None:
    """Sample documents to estimate vector coverage if no has_vector facet/flag usable.
    Returns ratio or None if sampling failed."""
    if not vector_field:
        return None
    headers={'api-key':key,'Content-Type':'application/json'}
    taken=0; with_vec=0; page=50; last_id=None; order_supported=True
    while taken < sample_size:
        filt=None
        body={'search':'*','top':page,'select':vector_field}
        if order_supported:
            body['orderby']='*'  # attempt stable ordering (may be rejected)
        if last_id:
            # Best-effort skip via continuation approach not supported; rely on random-ish distribution
            pass
        r=requests.post(f"{ep}/indexes/{name}/docs/search?api-version={API_VERSION}",headers=headers,json=body,timeout=45)
        if r.status_code!=200:
            return None
        vals=r.json().get('value',[])
        if not vals:
            break
        for v in vals:
            vec=v.get(vector_field)
            if isinstance(vec,list) and len(vec)>0:
                with_vec+=1
            taken+=1
            if taken>=sample_size:
                break
        if len(vals)<page:
            break
    if taken==0:
        return None
    return with_vec/ taken


def filtered_has_vector_count(ep: str, key: str, name: str) -> int | None:
    """Perform a count of docs where has_vector eq true if the field is filterable.
    Returns count or None on failure."""
    body={'search':'*','filter':'has_vector eq true','top':0,'count':True}
    r=requests.post(f"{ep}/indexes/{name}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=45)
    if r.status_code!=200:
        return None
    return r.json().get('@odata.count')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--indexes', default='auto', help='Comma list or auto (expected set) or all (service-wide)')
    ap.add_argument('--json', action='store_true', help='Output JSON report')
    ap.add_argument('--deep', action='store_true', help='Attempt filtered count and/or sampling to compute vector coverage when facet unavailable')
    ap.add_argument('--sample-size', type=int, default=150, help='Sample size for deep sampling coverage estimation')
    args = ap.parse_args()
    ep,key = load_config()

    service_indexes = set(get_index_list(ep,key))
    if args.indexes == 'all':
        target = sorted(service_indexes)
    elif args.indexes == 'auto':
        target = [ix for ix in EXPECTED_INDEXES.keys() if ix in service_indexes]
    else:
        target = [x.strip() for x in args.indexes.split(',') if x.strip()]

    report: List[Dict[str,Any]] = []
    for name in target:
        exists = name in service_indexes
        if not exists:
            report.append({
                'index': name,
                'exists': False,
                'required': EXPECTED_INDEXES.get(name, False),
                'status': 'ACTION' if EXPECTED_INDEXES.get(name, False) else 'ACTION',
                'rationale': 'Index missing',
            })
            continue
        try:
            defn = get_index(ep,key,name)
            stats = get_index_stats(ep,key,name)
            doc_count = stats.get('documentCount',0)
            analysis = analyze_index(defn)
            true_count = bucket_total = -1
            coverage_ratio = None
            if analysis['has_vector_flag']:
                # First attempt facet
                true_count, bucket_total = facet_has_vector(ep,key,name)
                if true_count >=0 and bucket_total>0:
                    denom = doc_count if doc_count>0 else bucket_total
                    coverage_ratio = true_count / denom if denom else 0
                elif args.deep:
                    # Try filtered count (requires filterable has_vector)
                    fc = filtered_has_vector_count(ep,key,name)
                    if isinstance(fc,int) and doc_count>0:
                        true_count = fc; coverage_ratio = fc / doc_count
            if coverage_ratio is None and args.deep:
                # Fall back to sampling vector field if present
                sampling_ratio = sample_vector_coverage(ep,key,name,analysis['vector_field'], sample_size=args.sample_size)
                if sampling_ratio is not None:
                    coverage_ratio = sampling_ratio
            status, rationale = classify(name, EXPECTED_INDEXES.get(name, False), doc_count, analysis['vector_field'], coverage_ratio)
            report.append({
                'index': name,
                'exists': True,
                'required': EXPECTED_INDEXES.get(name, False),
                'doc_count': doc_count,
                'vector_field': analysis['vector_field'],
                'vector_dims': analysis['vector_dims'],
                'has_vector_flag': analysis['has_vector_flag'],
                'vector_coverage_true': true_count if true_count>=0 else None,
                'vector_coverage_ratio': coverage_ratio,
                'key_field': analysis['key_field'],
                'text_field': analysis['text_field'],
                'vector_profiles_count': len(analysis['vector_profiles']),
                'vector_algorithms_count': len(analysis['vector_algorithms']),
                'status': status,
                'rationale': rationale,
            })
        except Exception as ex:
            report.append({
                'index': name,
                'exists': True,
                'required': EXPECTED_INDEXES.get(name, False),
                'status': 'ACTION',
                'rationale': f'Inspection error: {ex}'
            })

    if args.json:
        json.dump(report, sys.stdout, indent=2)
        return

    # Pretty table output
    headers = ["Index","Req","Docs","Vector","Dims","has_flag","Cov%","Status","Rationale"]
    rows = []
    for r in report:
        cov = r.get('vector_coverage_ratio')
        cov_str = f"{cov*100:.1f}" if cov is not None else ('-' if not r.get('has_vector_flag') else '?')
        rows.append([
            r['index'],
            'Y' if r['required'] else 'N',
            str(r.get('doc_count','-') if r.get('exists') else '-'),
            r.get('vector_field') or '-',
            str(r.get('vector_dims') or '-'),
            'Y' if r.get('has_vector_flag') else 'N',
            cov_str,
            r['status'],
            r['rationale'][:60]
        ])
    # Determine column widths
    col_w = [max(len(h), *(len(row[i]) for row in rows)) for i,h in enumerate(headers)]
    def fmt_row(parts):
        return " | ".join(p.ljust(col_w[i]) for i,p in enumerate(parts))
    print(fmt_row(headers))
    print("-+-".join('-'*w for w in col_w))
    for row in rows:
        print(fmt_row(row))

    # Summary counts
    summary = {}
    for r in report:
        summary[r['status']] = summary.get(r['status'],0)+1
    print("\nSummary:")
    for k,v in sorted(summary.items()):
        print(f"  {k}: {v}")

    # Remediation suggestions
    print("\nRemediation Suggestions:")
    for r in report:
        if r['status'] == 'READY':
            continue
        idx = r['index']
        rat = r['rationale']
        if 'missing' in rat.lower():
            print(f"- {idx}: Index missing – create it or adjust EXPECTED_INDEXES if obsolete.")
        elif '0% vector coverage' in rat:
            print(f"- {idx}: Run embedding script for this index (add has_vector flag + vector field).")
        elif 'Vector coverage' in rat and '%' in rat:
            print(f"- {idx}: Continue embedding until coverage >=95%. Resume existing streaming script if interrupted.")
        elif 'unknown' in rat.lower():
            print(f"- {idx}: Lacks has_vector facet; ensure ingestion sets has_vector Boolean or implement coverage sampling.")
        elif 'No documents' in rat:
            print(f"- {idx}: Populate documents before chatbot queries rely on it.")
        else:
            print(f"- {idx}: {rat}")

if __name__ == '__main__':
    main()
