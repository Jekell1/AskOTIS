#!/usr/bin/env python3
"""migrate_facts_to_v3l.py

Copy documents from `cobol-facts-v3` into `cobol-facts-v3l` (3072-dim) resetting vector fields.

Only copies core fields: fact_id, program_id, action_role, posting_type, gating_cond,
fact_confidence, sources, fact_text. Sets has_vector = False and omits fact_vector.

Usage:
  python migrate_facts_to_v3l.py --source cobol-facts-v3 --dest cobol-facts-v3l
"""
import os, sys, json, argparse, requests, time

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
PAGE = 1000

FIELDS = ["fact_id","program_id","action_role","posting_type","gating_cond","fact_confidence","sources","fact_text"]

def load_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def search_page(ep, key, index, skip):
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body = {"search":"*", "top": PAGE, "skip": skip, "select": ",".join(FIELDS)}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body, timeout=120)
    if r.status_code >= 300:
        raise SystemExit(f"Search error {r.status_code}: {r.text[:400]}")
    return r.json().get('value', [])

def upload(ep, key, index, docs):
    if not docs: return
    url = f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload, timeout=120)
    if r.status_code >= 300:
        raise SystemExit(f"Upload error {r.status_code}: {r.text[:400]}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--source', default='cobol-facts-v3')
    ap.add_argument('--dest', default='cobol-facts-v3l')
    args = ap.parse_args()
    load_settings()
    ep = os.environ.get('SEARCH_ENDPOINT'); key = os.environ.get('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)
    ep = ep.rstrip('/')
    total_uploaded = 0
    skip = 0
    while True:
        page = search_page(ep, key, args.source, skip)
        if not page:
            break
        out_docs = []
        for d in page:
            nd = {k: d.get(k) for k in FIELDS if k in d}
            nd['fact_id'] = d['fact_id']
            nd['has_vector'] = False
            nd['@search.action'] = 'mergeOrUpload'
            out_docs.append(nd)
        upload(ep, key, args.dest, out_docs)
        total_uploaded += len(out_docs)
        print(f"Migrated batch skip={skip} count={len(out_docs)} (total {total_uploaded})")
        if len(page) < PAGE:
            break
        skip += PAGE
        time.sleep(0.2)
    print(f"Migration complete: {total_uploaded} documents migrated to {args.dest}")

if __name__ == '__main__':
    main()
