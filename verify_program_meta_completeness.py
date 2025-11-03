"""Verify completeness of new_cobol_program_meta vs coverage_full.json.

Steps:
  * Enumerate program_ids in coverage JSON
  * Page through meta index retrieving only program_id field
  * Compute missing IDs
  * Optionally repair: upload placeholder docs (coverage fields if available)

Usage:
  python verify_program_meta_completeness.py --coverage coverage_full.json
  python verify_program_meta_completeness.py --coverage coverage_full.json --repair

Env fallback: AZURE_SEARCH_ENDPOINT/SEARCH_ENDPOINT and AZURE_SEARCH_KEY/SEARCH_KEY
"""
from __future__ import annotations
import os, sys, json, argparse, requests, time
from typing import List, Dict, Any, Set

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
META_INDEX = 'new_cobol_program_meta'
PAGE = 1000


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


def load_coverage_ids(path: str) -> Dict[str, Dict[str, Any]]:
    data = json.load(open(path,'r',encoding='utf-8'))
    out = {}
    for p in data.get('programs', []):
        pid = p.get('program_id')
        if pid:
            out[pid.upper()] = p
    return out


def fetch_all_meta_ids(ep: str, key: str) -> Set[str]:
    ids: Set[str] = set()
    skip = 0
    while True:
        body = {"search": "*", "top": PAGE, "skip": skip, "select": "program_id"}
        url = f"{ep}/indexes/{META_INDEX}/docs/search.post.search?api-version={API_VERSION}"
        r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body)
        if r.status_code != 200:
            raise SystemExit(f"Search failed {r.status_code}: {r.text[:200]}")
        batch = r.json().get('value', [])
        for b in batch:
            pid = b.get('program_id')
            if pid:
                ids.add(pid.upper())
        if len(batch) < PAGE:
            break
        skip += PAGE
    return ids


def upload_repairs(ep: str, key: str, docs: List[Dict[str, Any]]):
    if not docs: return
    url = f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API_VERSION}"
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise SystemExit(f"Repair upload failed {r.status_code}: {r.text[:300]}")


def main():
    ap = argparse.ArgumentParser(description='Verify and optionally repair program meta completeness.')
    ap.add_argument('--coverage', required=True)
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--repair', action='store_true')
    ap.add_argument('--batch-size', type=int, default=500)
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    coverage_map = load_coverage_ids(args.coverage)
    print(f"Coverage programs: {len(coverage_map)}")
    meta_ids = fetch_all_meta_ids(ep, key)
    print(f"Index program meta docs: {len(meta_ids)}")
    missing = sorted([pid for pid in coverage_map.keys() if pid not in meta_ids])
    print(f"Missing in index: {len(missing)}")
    if not missing:
        return
    if not args.repair:
        # show a small sample
        for pid in missing[:25]:
            print(f"  MISSING {pid}")
        print("Run with --repair to insert placeholder docs.")
        return
    print("Repairing missing docs...")
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    batch: List[Dict[str, Any]] = []
    uploaded = 0
    for pid in missing:
        cov = coverage_map.get(pid)
        doc = {
            'program_id': pid,
            'total_lines': cov.get('total_lines') if cov else None,
            'covered_lines': cov.get('covered_lines') if cov else None,
            'coverage_pct': cov.get('coverage_pct') if cov else None,
            'paragraph_count': cov.get('paragraph_count') if cov else None,
            'avg_paragraph_length': cov.get('avg_paragraph_length') if cov else None,
            'median_paragraph_length': cov.get('median_paragraph_length') if cov else None,
            'max_paragraph_length': cov.get('max_paragraph_length') if cov else None,
            'gap_count': cov.get('gap_count') if cov else None,
            'largest_gap_length': (cov.get('largest_gaps')[0]['length'] if cov and cov.get('largest_gaps') else None),
            'classification': 'UNKNOWN',
            'coverage_ingested_at': ts,
            'ingested_at': None
        }
        batch.append(doc)
        if len(batch) >= args.batch_size:
            upload_repairs(ep, key, batch)
            uploaded += len(batch)
            print(f"Uploaded repair docs: {uploaded}")
            batch.clear()
    if batch:
        upload_repairs(ep, key, batch)
        uploaded += len(batch)
        print(f"Uploaded repair docs: {uploaded}")
    print('Repair complete.')

if __name__ == '__main__':
    main()
