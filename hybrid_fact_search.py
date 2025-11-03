#!/usr/bin/env python3
"""hybrid_fact_search.py

Perform hybrid (keyword + vector) retrieval over facts index and fuse results.

Requires:
  SEARCH_ENDPOINT / SEARCH_KEY
  Azure vector-enabled facts index (e.g., cobol-facts-v3l) with fact_vector and profile configured.
  Embedding capability (Azure OpenAI) for question embedding.

Fusion Method: Reciprocal Rank Fusion (RRF)
  score = sum( 1 / (k_rank + rrf_k) ) across sources
  Default rrf_k = 50 (stabilizes tail) – configurable via --rrf-k

Outputs JSON list to stdout.

Usage examples:
    python hybrid_fact_search.py --index cobol-facts-v3l --question "How does ORDERS handle reversals?" --program ORDERS
    python hybrid_fact_search.py --index cobol-facts-v3l --question "inventory adjustment logic" --program INVENTORY --top 15
"""
import os, sys, json, argparse, requests, math, hashlib

API_VERSION = os.environ.get("AZURE_SEARCH_API_VERSION", "2024-07-01")
VECTOR_SEARCH_PROFILE = os.environ.get("FACTS_VECTOR_PROFILE", "facts-profile")
VECTOR_SEARCH_ALGO = os.environ.get("FACTS_VECTOR_ALGO", "facts-hnsw")

def load_settings():
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r')).get('Values',{})
            for k,v in vals.items():
                if k not in os.environ:
                    os.environ[k] = v
        except Exception:
            pass

def embed_question(text: str):
    # Azure embedding (large model) – reuse logic similar to embed_facts
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    model = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large')
    if not azure_ep or not azure_key:
        raise SystemExit('Missing Azure embedding credentials for question embedding')
    url = f"{azure_ep.rstrip('/')}/openai/deployments/{model}/embeddings?api-version=2024-02-15-preview"
    r = requests.post(url, headers={'api-key': azure_key, 'Content-Type':'application/json'}, json={'input': [text]}, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Embedding error {r.status_code}: {r.text[:500]}")
    data = r.json().get('data',[])
    if not data:
        raise SystemExit('No embedding returned for question')
    return data[0]['embedding']

def keyword_search(ep, key, index, query, program, top):
    ep = ep.rstrip('/')
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    flt_parts = []
    if program:
        flt_parts.append(f"program_id eq '{program}'")
    flt = ' and '.join(flt_parts) if flt_parts else None
    body = {
        'search': query if query.strip() else '*',
        'top': top,
        'select': 'fact_id,fact_text,program_id,action_role,posting_type,gating_cond,fact_confidence',
    }
    if flt:
        body['filter'] = flt
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Keyword search error {r.status_code}: {r.text[:400]}")
    vals = r.json().get('value', [])
    out = []
    for rank, v in enumerate(vals, start=1):
        out.append({'fact_id': v.get('fact_id'), 'rank': rank, 'source': 'keyword', 'raw': v})
    return out

def vector_search(ep, key, index, query_vec, program, top):
    ep = ep.rstrip('/')
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    flt_parts = []
    if program:
        flt_parts.append(f"program_id eq '{program}'")
    flt = ' and '.join(flt_parts) if flt_parts else None
    body = {
        'vectorQueries': [
            {
                'kind': 'vector',
                'vector': query_vec,
                'k': top,
                'fields': 'fact_vector'
            }
        ],
        'top': top,
        'select': 'fact_id,fact_text,program_id,action_role,posting_type,gating_cond,fact_confidence'
    }
    if flt:
        body['filter'] = flt
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code >= 300:
        raise SystemExit(f"Vector search error {r.status_code}: {r.text[:400]}")
    vals = r.json().get('value', [])
    out = []
    for rank, v in enumerate(vals, start=1):
        out.append({'fact_id': v.get('fact_id'), 'rank': rank, 'source': 'vector', 'raw': v})
    return out

def rrf_fuse(keyword_results, vector_results, kparam):
    fused = {}
    def add(res_list):
        for r in res_list:
            fid = r['fact_id']
            if not fid:
                continue
            entry = fused.setdefault(fid, {'fact_id': fid, 'sources': {}, 'raw': r['raw']})
            entry['sources'][r['source']] = r['rank']
    add(keyword_results)
    add(vector_results)
    # compute score
    for fid, entry in fused.items():
        score = 0.0
        for src, rank in entry['sources'].items():
            score += 1.0 / (rank + kparam)
        entry['score'] = score
    return list(fused.values())

def enrich(entry):
    raw = entry['raw']
    return {
        'fact_id': entry['fact_id'],
        'score': entry['score'],
        'rank_sources': entry['sources'],
        'program_id': raw.get('program_id'),
        'fact_confidence': raw.get('fact_confidence'),
        'action_role': raw.get('action_role'),
        'posting_type': raw.get('posting_type'),
        'gating_cond': raw.get('gating_cond'),
        'fact_text': raw.get('fact_text')
    }

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', default='cobol-facts-v3l')
    ap.add_argument('--question', required=True)
    ap.add_argument('--program', help='Optional program_id filter')
    ap.add_argument('--top', type=int, default=25)
    ap.add_argument('--rrf-k', type=int, default=50)
    ap.add_argument('--no-vector', action='store_true', help='Disable vector component')
    ap.add_argument('--no-keyword', action='store_true', help='Disable keyword component')
    ap.add_argument('--raw', action='store_true', help='Print raw fused entries (debug)')
    args = ap.parse_args()

    load_settings()
    ep = os.getenv('SEARCH_ENDPOINT'); key = os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing SEARCH_ENDPOINT/SEARCH_KEY', file=sys.stderr); sys.exit(2)

    keyword_results = []
    vector_results = []
    if not args.no_keyword:
        keyword_results = keyword_search(ep, key, args.index, args.question, args.program, args.top)
    if not args.no_vector:
        qvec = embed_question(args.question)
        vector_results = vector_search(ep, key, args.index, qvec, args.program, args.top)

    fused = rrf_fuse(keyword_results, vector_results, args.rrf_k)
    fused_sorted = sorted(fused, key=lambda x: -x['score'])[:args.top]
    final = [enrich(e) for e in fused_sorted]
    print(json.dumps(final, indent=2))

if __name__ == '__main__':
    main()
