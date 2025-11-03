"""Hybrid vector + lexical + optional semantic query demo for new-cobol-files.

Demonstrates:
 - Vector similarity on contentVector
 - Simultaneous lexical match with 'search'
 - Optional filter (e.g. kind eq 'program' and hasSQL eq true)
 - Optional semantic ranking (if semantic configuration is enabled on index)

Usage:
  python query_hybrid_demo.py --query "customer loan processing" --k 8
  python query_hybrid_demo.py --query "sql cursor" --filter "hasSQL eq true" --k 5
  python query_hybrid_demo.py --query "update screen" --semantic --k 6

Environment:
  Requires AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY. For real vectors, also Azure OpenAI embedding deployment vars; otherwise falls back to deterministic vector.
"""
from __future__ import annotations
import os, json, argparse, requests
import env_autoload
from ingest_sample_cobol_file import generate_vector, VECTOR_DIMS, INDEX_NAME, API_VERSION


def build_body(query: str, vector, k: int, filter_expr: str | None, semantic: bool, select: str):
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
        "queryLanguage": "en-us",
    }
    if query:
        body["search"] = query  # lexical component
    if filter_expr:
        body["filter"] = filter_expr
    if semantic:
        # semantic config name assumed in canonical schema; change if different
        body["semanticConfiguration"] = "semantics"
        body["answers"] = {"answerType": "extractive", "count": 3}
        body["captions"] = {"captionType": "extractive", "highlightEnabled": True}
    return body


def run_search(body: dict):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, data=json.dumps(body).encode('utf-8'))
    if r.status_code != 200:
        raise RuntimeError(f"Search failed {r.status_code}: {r.text[:400]}")
    return r.json()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--query', required=True)
    ap.add_argument('--k', type=int, default=5)
    ap.add_argument('--filter', help="OData filter expression e.g. kind eq 'program' and hasSQL eq true")
    ap.add_argument('--semantic', action='store_true', help='Enable semantic ranking (requires semantic config)')
    ap.add_argument('--select', default='programId,path,kind,hasSQL,hasCICS,summary')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    vec = generate_vector(args.query)
    print(f"Generated query vector dims={len(vec)} (expected {VECTOR_DIMS})")
    body = build_body(args.query, vec, args.k, args.filter, args.semantic, args.select)
    res = run_search(body)

    hits = res.get('value', [])
    print(f"\nTop {len(hits)} results (hybrid):")
    for h in hits:
        score = h.get('@search.score')
        pid = h.get('programId')
        path = h.get('path')
        kind = h.get('kind')
        sql = h.get('hasSQL')
        cics = h.get('hasCICS')
        summary = h.get('summary','')
        print(f" - {pid} [{kind}] score={score:.4f} sql={sql} cics={cics}")
        if summary:
            print("   ", (summary[:140] + '...') if len(summary) > 160 else summary)

    if args.semantic:
        if 'answers' in res:
            print('\nSemantic Answers:')
            for a in res['answers']:
                print(f" * {a.get('text','')[:240]}")
        if 'captions' in res:
            print('\nSemantic Captions:')
            for c in res['captions']:
                txt = c.get('text','')
                print(f" # {txt[:160]}" )

if __name__ == '__main__':
    main()
