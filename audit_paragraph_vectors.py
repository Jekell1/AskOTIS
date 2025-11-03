"""Audit new_cobol_paragraphs vector completeness & optionally repair.

Checks:
  * Count docs
  * Sample pages (or all) retrieving has_vector + para_vector length
  * Estimate missing ratio
  * Optionally repair: fetch docs missing vectors (has_vector==false), embed source_excerpt, re-upload.

Usage:
  python audit_paragraph_vectors.py --endpoint ... --key ... --sample-pages 10
  python audit_paragraph_vectors.py --repair-missing --embed-batch-size 64

Env fallback: AZURE_SEARCH_ENDPOINT/SEARCH_ENDPOINT and AZURE_SEARCH_KEY/SEARCH_KEY.
"""
from __future__ import annotations
import os, json, argparse, sys, requests, random, time
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_paragraphs'
PAGE = 1000
MAX_SKIP = 100000  # Azure Cognitive Search service limit


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def count_index(ep: str, key: str) -> int:
    url = f"{ep}/indexes/{INDEX}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise SystemExit(f"Count failed {r.status_code}: {r.text[:200]}")
    return int(r.text)


def page_docs(ep: str, key: str, skip: int, select: str) -> List[Dict[str, Any]]:
    # Note: para_vector is not retrievable, so we only request IDs and has_vector.
    body = {"search":"*","top":PAGE,"skip":skip,"select":select}
    url = f"{ep}/indexes/{INDEX}/docs/search.post.search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body)
    if r.status_code != 200:
        raise SystemExit(f"Search page failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value', [])


def upload_batch(ep: str, key: str, docs: List[Dict[str, Any]]):
    if not docs: return
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    payload = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs]}
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=payload)
    if r.status_code not in (200,201):
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:200]}")


def audit(ep: str, key: str, sample_pages: int | None) -> Dict[str, Any]:
    total = count_index(ep, key)
    examined = 0
    missing = 0
    pages = 0
    skip = 0
    # Strategy:
    #  * If sample_pages is None -> full scan until all docs or skip hits limit (then warn)
    #  * If sample_pages provided -> scan sequentially first N pages (simpler, avoids large random skips)
    max_pages = None if not sample_pages else sample_pages
    while skip < total and skip < MAX_SKIP:
        if max_pages is not None and pages >= max_pages:
            break
        docs = page_docs(ep, key, skip, 'para_id,has_vector')
        if not docs:
            break
        for d in docs:
            examined += 1
            if not d.get('has_vector'):
                missing += 1
        pages += 1
        skip += PAGE
    truncated = skip < total and skip >= MAX_SKIP
    return {
        'total_docs': total,
        'examined_docs': examined,
        'missing_vectors': missing,
        'missing_pct': round((missing / examined * 100.0),2) if examined else 0.0,
        'pages_scanned': pages,
        'truncated': truncated
    }


def collect_missing_docs(ep: str, key: str, max_docs: int) -> List[Dict[str, Any]]:
    out: List[Dict[str,Any]] = []
    skip = 0
    while True:
        docs = page_docs(ep, key, skip, 'para_id,has_vector,source_excerpt')
        for d in docs:
            if not d.get('has_vector'):
                out.append(d)
                if len(out) >= max_docs:
                    return out
        skip += PAGE
        if skip >= count_index(ep, key):
            break
    return out


def main():
    ap = argparse.ArgumentParser(description='Audit paragraph vectors and optionally repair missing.')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--sample-pages', type=int, help='Sample this many random pages; omit for full scan (can be expensive).')
    ap.add_argument('--repair-missing', action='store_true')
    ap.add_argument('--repair-limit', type=int, default=2000)
    ap.add_argument('--embed-batch-size', type=int, default=64)
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    report = audit(ep, key, args.sample_pages)
    print('PARAGRAPH VECTOR AUDIT')
    for k,v in report.items():
        print(f"  {k}: {v}")
    if not args.repair_missing:
        return
    if report['missing_vectors'] == 0:
        print('No repairs needed.')
        return
    print('Collecting docs needing repair...')
    targets = collect_missing_docs(ep, key, args.repair_limit)
    if not targets:
        print('No target docs found for repair (race condition).')
        return
    print(f'Found {len(targets)} docs needing repair (capped). Embedding... Provider={provider_info()}')
    texts = [t.get('source_excerpt','') for t in targets]
    vecs = batch_embed(texts, target_dim=None, batch_size=args.embed_batch_size)
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    updates = []
    for t,v in zip(targets, vecs):
        updates.append({'para_id': t['para_id'], 'para_vector': v, 'has_vector': True, 'ingested_at': ts})
    for i in range(0, len(updates), 500):
        upload_batch(ep, key, updates[i:i+500])
    print(f'Repaired {len(updates)} paragraph docs.')

if __name__ == '__main__':
    main()
