"""Example vector (or hybrid) query against new-cobol-files using vectorQueries.

Usage:
  python query_vector_example.py --program SAMPLE1 --query "hello"

The script:
 1. Loads env
 2. Fetches the document back (optional) or re-embeds query text
 3. Issues POST /search with vectorQueries
 4. Prints top results

If Azure OpenAI embedding vars are missing, uses fallback deterministic vector (same function as ingestion) so scores will be synthetic but pipeline is validated.
"""
from __future__ import annotations
import os, sys, json, argparse, requests
import env_autoload
from ingest_sample_cobol_file import generate_vector, VECTOR_DIMS, INDEX_NAME, API_VERSION


def vector_for_query(text: str):
    return generate_vector(text)


def search(vector, k: int, select: str = 'programId,path,summary'):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    body = {
        "vectorQueries": [
            {
                "kind": "vector",
                "vector": vector,
                "fields": "contentVector",
                "k": k
            }
        ],
        "select": select,
        # "search": "optional lexical terms"  # can add for hybrid
    }
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, data=json.dumps(body).encode('utf-8'))
    if r.status_code != 200:
        raise RuntimeError(f"Search failed {r.status_code}: {r.text[:400]}")
    return r.json()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--query', required=True, help='Natural language / keywords')
    ap.add_argument('--k', type=int, default=5)
    args = ap.parse_args()
    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    vec = vector_for_query(args.query)
    print(f"Query vector dims={len(vec)} (expected {VECTOR_DIMS})")
    res = search(vec, args.k)
    hits = res.get('value', [])
    print(f"\nTop {len(hits)} results:")
    for h in hits:
        print(f" - {h.get('programId')} path={h.get('path')} score={h.get('@search.score'):.4f}")
        if 'summary' in h:
            s = h['summary']
            print('   summary:', (s[:140] + '...') if len(s) > 160 else s)

if __name__ == '__main__':
    main()
