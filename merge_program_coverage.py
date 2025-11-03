"""Merge coverage audit metrics into new_cobol_program_meta index.

Reads coverage JSON produced by program_coverage_audit.py (coverage_full.json) and issues
mergeOrUpload operations to enrich existing program meta docs with coverage + classification.

Usage:
  python merge_program_coverage.py --coverage coverage_full.json [--endpoint ... --key ...] [--batch 500]

Environment fallback: AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT and AZURE_SEARCH_KEY / SEARCH_KEY

Fields merged (must exist in index schema):
  total_lines, covered_lines, coverage_pct, paragraph_count,
  avg_paragraph_length, median_paragraph_length, max_paragraph_length,
  gap_count, largest_gap_length, classification, coverage_ingested_at

Classification heuristic mirrors build_program_meta.py classify_program.
"""
from __future__ import annotations
import os, json, argparse, sys, time, requests
from typing import Dict, Any, List

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
META_INDEX = 'new_cobol_program_meta'


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def classify_program(cov: Dict[str, Any]) -> str:
    para_count = cov.get('paragraph_count',0)
    coverage_pct = cov.get('coverage_pct',0.0)
    total_lines = cov.get('total_lines',0)
    if total_lines > 0 and para_count == 0:
        return 'NO_PARAS'
    if coverage_pct < 10.0:
        return 'LOW_COVERAGE'
    return 'NORMAL'

def load_coverage(path: str) -> List[Dict[str, Any]]:
    data = json.load(open(path,'r',encoding='utf-8'))
    progs = data.get('programs') or []
    # augment largest_gap_length
    for p in progs:
        largest_gap_len = 0
        try:
            if p.get('largest_gaps'):
                largest_gap_len = p['largest_gaps'][0]['length']
        except Exception:
            largest_gap_len = 0
        p['largest_gap_length'] = largest_gap_len
    return progs

def upload(ep: str, key: str, docs: List[Dict[str, Any]]):
    if not docs:
        return
    url = f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API_VERSION}"
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")

def main():
    ap = argparse.ArgumentParser(description='Merge coverage metrics into program meta index.')
    ap.add_argument('--coverage', required=True, help='Path to coverage_full.json')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--batch', type=int, default=500)
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    coverage = load_coverage(args.coverage)
    print(f"Loaded coverage for {len(coverage)} programs")
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    batch: List[Dict[str, Any]] = []
    uploaded = 0
    for rec in coverage:
        pid = rec.get('program_id')
        if not pid:
            continue
        doc = {
            'program_id': pid.upper(),
            'total_lines': rec.get('total_lines'),
            'covered_lines': rec.get('covered_lines'),
            'coverage_pct': rec.get('coverage_pct'),
            'paragraph_count': rec.get('paragraph_count'),
            'avg_paragraph_length': rec.get('avg_paragraph_length'),
            'median_paragraph_length': rec.get('median_paragraph_length'),
            'max_paragraph_length': rec.get('max_paragraph_length'),
            'gap_count': rec.get('gap_count'),
            'largest_gap_length': rec.get('largest_gap_length'),
            'classification': classify_program(rec),
            'coverage_ingested_at': ts
        }
        batch.append(doc)
        if len(batch) >= args.batch:
            upload(ep, key, batch)
            uploaded += len(batch)
            print(f"Uploaded coverage docs: {uploaded}")
            batch.clear()
    if batch:
        upload(ep, key, batch)
        uploaded += len(batch)
        print(f"Uploaded coverage docs: {uploaded}")
    print('Coverage merge complete.')

if __name__ == '__main__':
    main()
