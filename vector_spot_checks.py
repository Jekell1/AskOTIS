"""Vector spot checks to validate embedding quality and retrieval diversity.

For each supplied (or default) query:
 1. Embed the query using Azure OpenAI embedding deployment.
 2. Issue a vector search (k=5) against the contentVector field of the index.
 3. Print top doc ids with scores and first 120 chars of content.

If embeddings are not yet present for docs, results may be random or low quality. This helps confirm vectors exist.
"""
from __future__ import annotations
import os, sys, argparse
import requests
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VECTOR_FIELD = 'contentVector'

DEFAULT_QUERIES = [
    'update customer account balance',
    'cics map handling and send/receive',
    'sql cursor open fetch close loop',
    'copybook defining transaction detail record',
    'screen map definition for account inquiry',
    'file read write error handling routine',
    'batch job control logic for end of day processing'
]

def fetch_embedding(endpoint: str, key: str, deployment: str, text: str):
    url = f"{endpoint}/openai/deployments/{deployment}/embeddings?api-version=2024-08-01-preview"
    payload = {"input": text}
    r = requests.post(url, headers={"api-key": key, "Content-Type": "application/json"}, json=payload, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    return data['data'][0]['embedding']


def vector_search(search_endpoint: str, search_key: str, vector, k: int):
    url = f"{search_endpoint}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    body = {
        "vectorQueries": [
            {"kind": "vector", "vector": vector, "k": k, "fields": VECTOR_FIELD}
        ],
        "select": "id,content"
    }
    r = requests.post(url, headers={"api-key": search_key, "Content-Type": "application/json"}, json=body, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"Vector search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value', [])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--queries', nargs='*', help='Override default queries')
    ap.add_argument('--k', type=int, default=5)
    ap.add_argument('--embed-deployment', required=False, help='Azure OpenAI embedding deployment name')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY'])
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    search_key = os.getenv('AZURE_SEARCH_KEY')
    aoai_endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    aoai_key = os.getenv('AZURE_OPENAI_KEY')
    deployment = args.embed_deployment or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT')
    if not deployment:
        print('Embedding deployment name not provided (use --embed-deployment or env).', file=sys.stderr)
        sys.exit(1)

    queries = args.queries if args.queries else DEFAULT_QUERIES

    for q in queries:
        print('\n=== Query:', q)
        try:
            emb = fetch_embedding(aoai_endpoint, aoai_key, deployment, q)
            docs = vector_search(search_endpoint, search_key, emb, args.k)
            if not docs:
                print('  No results')
                continue
            for i, d in enumerate(docs, 1):
                snippet = (d.get('content','') or '').strip().replace('\n',' ')[:120]
                score = d.get('@search.score')
                score_fmt = f"{score:.4f}" if isinstance(score, (int, float)) else str(score)
                print(f"  {i}. {d.get('id')} score={score_fmt} snippet={snippet}")
        except Exception as e:
            print(f'  Error: {e}')

if __name__ == '__main__':
    main()
