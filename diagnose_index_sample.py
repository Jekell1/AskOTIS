#!/usr/bin/env python3
"""diagnose_index_sample.py
Fetch a few sample docs from specified Azure AI Search indexes to inspect field presence.
Usage:
  python diagnose_index_sample.py cobol-paragraphs-v2 cobol-copybooks-v2 cobol-facts-v2 --top 3
Env required: AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY
"""
import os, sys, json, argparse, requests

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
ENDPOINT = os.environ.get("AZURE_SEARCH_ENDPOINT") or os.environ.get("SEARCH_ENDPOINT")
KEY = os.environ.get("AZURE_SEARCH_KEY") or os.environ.get("SEARCH_KEY")

session = requests.Session()


def sample(index: str, top: int, term: str="*"):
    url = f"{ENDPOINT}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body = {"search": term or "*", "top": top}
    r = session.post(url, headers={"api-key": KEY, "Content-Type": "application/json"}, json=body, timeout=60)
    if r.status_code >= 300:
        print(f"Index {index} error {r.status_code}: {r.text[:300]}")
        return []
    return r.json().get('value', [])


def summarize(doc):
    keys = sorted(doc.keys())
    # Hide large vector fields
    redacted = {}
    for k,v in doc.items():
        if k.startswith('@search.'):
            continue
        if isinstance(v, list) and v and isinstance(v[0], float) and len(v) > 32:
            redacted[k] = f"<vector len={len(v)}>"
        else:
            if isinstance(v, str) and len(v) > 160:
                redacted[k] = v[:160] + "..."
            else:
                redacted[k] = v
    return redacted


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('indexes', nargs='+')
    ap.add_argument('--top', type=int, default=2)
    ap.add_argument('--search', default='*', help='Optional search term')
    args = ap.parse_args()
    if not ENDPOINT or not KEY:
        print('Missing endpoint or key.', file=sys.stderr)
        sys.exit(2)
    for idx in args.indexes:
        print(f"=== Index: {idx} ===")
        docs = sample(idx, args.top, args.search)
        print(f"Sample count: {len(docs)}")
        for i, d in enumerate(docs):
            print(f"-- Doc {i+1} --")
            print(json.dumps(summarize(d), indent=2))

if __name__ == '__main__':
    main()
