"""Merge re-audit coverage deltas back into program meta index.

Input: re_audit_zero_coverage.json produced by re_audit_zero_coverage.py
For each program where new_paragraph_count > orig_paragraph_count or new_coverage_pct > orig_coverage_pct
it updates coverage-related fields in new_cobol_program_meta.

Usage:
  python merge_reaudit_deltas.py --deltas re_audit_zero_coverage.json --endpoint ... --key ...
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

def upload(ep: str, key: str, docs: List[Dict[str, Any]]):
    if not docs:
        return
    url = f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API_VERSION}"
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")

def main():
    ap = argparse.ArgumentParser(description='Merge re-audit delta coverage into program meta index.')
    ap.add_argument('--deltas', required=True)
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--batch', type=int, default=500)
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    data = json.load(open(args.deltas,'r',encoding='utf-8'))
    progs = data.get('programs', [])
    print(f"Loaded {len(progs)} re-audit delta records")
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    batch: List[Dict[str, Any]] = []
    uploaded = 0
    for rec in progs:
        if rec.get('new_paragraph_count',0) > rec.get('orig_paragraph_count',0) or rec.get('new_coverage_pct',0.0) > rec.get('orig_coverage_pct',0.0):
            doc = {
                'program_id': rec['program_id'].upper(),
                'paragraph_count': rec.get('new_paragraph_count'),
                'coverage_pct': rec.get('new_coverage_pct'),
                'covered_lines': rec.get('new_covered_lines'),
                'coverage_ingested_at': ts
            }
            batch.append(doc)
        if len(batch) >= args.batch:
            upload(ep, key, batch)
            uploaded += len(batch)
            print(f"Uploaded delta docs: {uploaded}")
            batch.clear()
    if batch:
        upload(ep, key, batch)
        uploaded += len(batch)
        print(f"Uploaded delta docs: {uploaded}")
    print('Re-audit delta merge complete.')

if __name__ == '__main__':
    main()
