"""Verify required Azure Search + (optional) Azure OpenAI environment and basic connectivity.

Checks:
 1. Presence of AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY
 2. Optional embedding deployment vars (AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT)
 3. Performs a HEAD request to the search service indexes endpoint
 4. Lists (top) indexes names to confirm key works
 5. If embedding vars present, performs a trivial embedding call ("test") and validates dimension 3072.

Usage:
  python verify_credentials.py
"""
from __future__ import annotations
import os, sys, requests, json
import env_autoload

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
EXPECTED_EMBED_DIMS = 3072

def check_search():
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        print('ERROR: Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
        return False
    # Use GET directly (some services disallow HEAD and return 405)
    list_url = f"{endpoint}/indexes?api-version={API_VERSION}"
    r = requests.get(list_url, headers={'api-key': key})
    if r.status_code != 200:
        print(f"ERROR: List indexes failed {r.status_code}: {r.text[:200]}")
        return False
    data = r.json()
    names = [idx['name'] for idx in data.get('value', [])]
    print(f"Search OK. Found {len(names)} indexes: {', '.join(names[:8])}{'...' if len(names)>8 else ''}")
    return True


def have_embeddings():
    return all(os.getenv(k) for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'])


def check_embeddings():
    if not have_embeddings():
        print('Embedding variables not all present; skipping embedding test.')
        return True
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_OPENAI_KEY')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'input': ['test credential embedding']}, timeout=30)
    if r.status_code != 200:
        print(f"ERROR: Embedding request failed {r.status_code}: {r.text[:200]}")
        return False
    vec = r.json()['data'][0]['embedding']
    print(f"Embedding OK. Dimension={len(vec)}")
    if len(vec) != EXPECTED_EMBED_DIMS:
        print(f"WARNING: Expected dimension {EXPECTED_EMBED_DIMS} but got {len(vec)}. Index vectors must match.")
    return True


def main():
    env_autoload.ensure([])  # load local.settings.json if present
    ok_search = check_search()
    ok_embed = check_embeddings()
    if ok_search and ok_embed:
        print('All credential checks passed.')
    else:
        print('One or more checks failed.')
        sys.exit(1)

if __name__ == '__main__':
    main()
