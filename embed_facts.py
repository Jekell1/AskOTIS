#!/usr/bin/env python3
"""embed_facts.py

Embed fact_text for facts (default: cobol-facts-v3) that lack vectors (has_vector != true) and
store results in fact_vector + set has_vector true.

Environment variables expected (reuse same as other scripts):
  SEARCH_ENDPOINT / SEARCH_KEY
  (optional) AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY / AZURE_OPENAI_EMBED_MODEL
Fallback embedding provider: OpenAI-compatible endpoint defined by OPENAI_API_KEY + OPENAI_API_BASE + EMBED_MODEL

We batch by 32 texts. Skips facts without fact_text.

Usage:
    python embed_facts.py --index cobol-facts-v3 --program ORDERS --dry
    python embed_facts.py --index cobol-facts-v3 --program INVENTORY
"""
import os, sys, json, argparse, time, math
from typing import List, Dict
import requests

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION","2024-07-01")
EMBED_BATCH = 32
DEFAULT_VECTOR_DIM = 1536  # fallback for fake/dry-run if we cannot infer

def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def get_search():
    ep = (os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY env vars')
    return ep, key

# --- Embedding helpers ---

def embed_texts(texts: List[str], provider: str="auto", skip_azure: bool=False):
    """Obtain embeddings with resilient fallback.

    Order:
      1. Azure OpenAI (if endpoint/key present). If 404 DeploymentNotFound -> fallback.
      2. Generic OpenAI-compatible (OPENAI_API_KEY required).
    """
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    # Accept multiple possible variable names for the Azure embedding deployment/model.
    azure_model = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or
                   os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or
                   os.getenv('EMBED_MODEL') or
                   'text-embedding-3-small')
    openai_key = os.getenv('OPENAI_API_KEY')
    # Try Azure
    if provider in ("auto","azure") and not skip_azure and azure_ep and azure_key:
        try:
            url = f"{azure_ep.rstrip('/')}/openai/deployments/{azure_model}/embeddings?api-version=2024-02-15-preview"
            headers = {'api-key': azure_key, 'Content-Type':'application/json'}
            resp = requests.post(url, headers=headers, json={'input': texts})
            if resp.status_code == 404 and openai_key:
                # Deployment missing -> fallback
                pass
            elif resp.status_code >= 300:
                # Other Azure error
                if not openai_key:
                    raise SystemExit(f"Azure embedding error {resp.status_code}: {resp.text[:400]}")
                # fallback if generic available
            else:
                data = resp.json().get('data',[])
                return [d['embedding'] for d in data]
        except Exception as e:
            if not openai_key:
                raise
            # else fallback to generic
    # Generic provider
    if provider in ("auto","openai","azure"):
        base = os.getenv('OPENAI_API_BASE') or 'https://api.openai.com/v1'
        if not openai_key:
            raise SystemExit('No embedding provider credentials found (neither Azure nor generic)')
        # Generic provider model selection (allow reuse of azure vars for convenience)
        model = (os.getenv('EMBED_MODEL') or
                 os.getenv('AZURE_OPENAI_EMBED_MODEL') or
                 os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or
                 'text-embedding-3-small')
        url = f"{base.rstrip('/')}/embeddings"
        headers = {'Authorization': f'Bearer {openai_key}', 'Content-Type':'application/json'}
        resp = requests.post(url, headers=headers, json={'model': model, 'input': texts})
        if resp.status_code >= 300:
            raise SystemExit(f"Embedding error {resp.status_code}: {resp.text[:400]}")
        data = resp.json().get('data', [])
        return [d['embedding'] for d in data]
    raise SystemExit(f"Unsupported provider selection: {provider}")

# --- Search fetch/update ---

def fetch_candidate_facts(ep: str, key: str, index: str, program: str|None, top: int=1000):
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    filter_parts = ["has_vector ne true"]
    if program:
        filter_parts.append(f"program_id eq '{program}'")
    flt = ' and '.join(filter_parts)
    body = {"search":"*", "filter": flt, "top": top, "select":"fact_id,fact_text"}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Search fetch error {r.status_code}: {r.text[:400]}")
    vals = r.json().get('value', [])
    # Filter out docs without fact_text
    return [v for v in vals if v.get('fact_text')]

def upload_vectors(ep: str, key: str, index: str, batch_docs: List[Dict]):
    if not batch_docs:
        return
    url = f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": batch_docs}
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload, timeout=120)
    if r.status_code >= 300:
        raise SystemExit(f"Vector upload error {r.status_code}: {r.text[:400]}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', default='cobol-facts-v3', help='Facts index name')
    ap.add_argument('--program', help='Optional program_id filter')
    ap.add_argument('--batch', type=int, default=EMBED_BATCH)
    ap.add_argument('--dry', action='store_true')
    ap.add_argument('--limit', type=int, default=0, help='Limit number of facts to embed (0 = all)')
    ap.add_argument('--fake', action='store_true', help='Generate zero vectors (for offline testing)')
    ap.add_argument('--vector-dim', type=int, default=0, help='Override vector dimension (0=auto from embedding or default)')
    ap.add_argument('--provider', choices=['auto','azure','openai'], default='auto', help='Embedding provider preference order')
    ap.add_argument('--skip-azure', action='store_true', help='Skip Azure even if credentials present')
    args = ap.parse_args()
    load_settings()
    ep, key = get_search()
    candidates = fetch_candidate_facts(ep, key, args.index, args.program, top=2000)
    if args.limit:
        candidates = candidates[:args.limit]
    print(f"Found {len(candidates)} facts needing embeddings")
    if not candidates:
        return
    total = len(candidates)
    inferred_dim = None
    for i in range(0, total, args.batch):
        chunk = candidates[i:i+args.batch]
        texts = [c['fact_text'][:8000] for c in chunk]  # safety truncate
        vecs = []
        if args.fake:
            dim = args.vector_dim or DEFAULT_VECTOR_DIM
            vecs = [[0.0]*dim for _ in texts]
        else:
            if not args.dry:
                vecs = embed_texts(texts, provider=args.provider, skip_azure=args.skip_azure)
            else:
                dim = args.vector_dim or DEFAULT_VECTOR_DIM
                vecs = [[0.0]*dim for _ in texts]
        if not inferred_dim and vecs:
            inferred_dim = len(vecs[0])
            print(f"Using vector dimension {inferred_dim}")
        updates = []
        for c, v in zip(chunk, vecs):
            updates.append({
                '@search.action': 'mergeOrUpload',
                'fact_id': c['fact_id'],
                'fact_vector': v,
                'has_vector': True
            })
        if args.dry:
            print(f"DRY: would upload {len(updates)} vectors (batch starting at {i})")
        else:
            upload_vectors(ep, key, args.index, updates)
            print(f"Embedded batch {i//args.batch + 1} ({len(updates)} docs)")
            time.sleep(0.2)

if __name__ == '__main__':
    main()
