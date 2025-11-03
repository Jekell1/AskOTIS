#!/usr/bin/env python3
"""Sample/backfill embeddings into a retrievable variant vector field (contentVector_r).

Purpose:
  - Enable spot validation of actual stored embeddings (length, norm distribution, similarity) since original field is not retrievable.
  - Supports incremental sampling (random or sequential) without re-embedding entire corpus.

Features:
  * Select N candidate docs via search * with $top paging (sequential) or random IDs (if --random-sample provided via list file).
  * Build embedding input: summary + two newlines + first 4000 chars of contentShort (mirrors original embedding logic).
  * Generate embeddings with Azure OpenAI (AZURE_OPENAI_ENDPOINT/KEY/EMBED_DEPLOYMENT).
  * Write vectors to contentVector_r using merge action.
  * Optional quality report: vector count, dimension check, mean/min/max norm, pairwise cosine sample.
  * Idempotent: skips docs already having contentVector_r unless --overwrite.

Env required:
  AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Usage examples:
  python backfill_retrievable_vectors_sample.py --index new-cobol-files --limit 200 --page-size 80
  python backfill_retrievable_vectors_sample.py --index new-cobol-files --limit 100 --overwrite --quality-report

"""
from __future__ import annotations
import os, sys, json, math, time, random, argparse
from typing import List, Dict, Tuple
import requests
import statistics

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
VECTOR_DIMS_DEFAULT = 3072

# ---------------- Credential helpers ----------------

def load_local_settings_values():
    path='local.settings.json'
    if not os.path.exists(path):
        return {}
    try:
        with open(path,'r',encoding='utf-8') as f:
            return json.load(f).get('Values',{}) or {}
    except Exception:
        return {}

def ensure_env(keys: List[str]):
    missing = [k for k in keys if not os.getenv(k)]
    if not missing:
        return
    vals = load_local_settings_values()
    for k in missing:
        if vals.get(k):
            os.environ[k] = vals[k]
    # Alias support: allow AZURE_SEARCH_ENDPOINT/KEY if SEARCH_ENDPOINT/SEARCH_KEY absent
    if 'SEARCH_ENDPOINT' in keys and not os.getenv('SEARCH_ENDPOINT') and os.getenv('AZURE_SEARCH_ENDPOINT'):
        os.environ['SEARCH_ENDPOINT'] = os.getenv('AZURE_SEARCH_ENDPOINT')
    if 'SEARCH_KEY' in keys and not os.getenv('SEARCH_KEY') and os.getenv('AZURE_SEARCH_KEY'):
        os.environ['SEARCH_KEY'] = os.getenv('AZURE_SEARCH_KEY')
    still = [k for k in keys if not os.getenv(k)]
    if still:
        raise SystemExit(f"Missing required env vars: {', '.join(still)} (set in env or local.settings.json Values)")

# ---------------- Azure Search helpers ----------------

def search_docs(endpoint: str, key: str, index: str, skip: int, top: int, select: str) -> List[Dict]:
    params = {
        'api-version': API_VERSION,
        '$skip': str(skip),
        '$top': str(top),
        '$select': select,
    }
    url = f"{endpoint}/indexes/{index}/docs"
    r = requests.get(url, headers={'api-key':key}, params=params, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"search page failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value',[])

def upload_vectors(endpoint: str, key: str, index: str, docs: List[Dict]):
    if not docs:
        return
    url = f"{endpoint}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {'value': docs}
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, data=json.dumps(payload).encode('utf-8'), timeout=120)
    if r.status_code >= 300:
        size = len(json.dumps(payload))
        snippet = r.text[:200].replace('\n',' ')
        raise RuntimeError(f"upload failed {r.status_code}: {snippet} (payload size {size} bytes)")
    jr = r.json()
    failures = [x for x in jr.get('value',[]) if not x.get('status')]
    if failures:
        raise RuntimeError(f"{len(failures)} merges failed; sample {failures[0]}")

# ---------------- Embedding helpers ----------------

def embed_batch(texts: List[str], dims: int) -> List[List[float]]:
    ensure_env(['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'])
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    key = os.getenv('AZURE_OPENAI_KEY')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'input':texts}, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"embedding request failed {r.status_code}: {r.text[:180]}")
    data = r.json().get('data',[])
    out = [d['embedding'] for d in data]
    for v in out:
        if len(v)!=dims:
            raise RuntimeError(f"dimension mismatch expected {dims} got {len(v)}")
    return out

# ---------------- Quality metrics ----------------

def vector_norm(vec: List[float]) -> float:
    return math.sqrt(sum(x*x for x in vec))

def cosine(a: List[float], b: List[float]) -> float:
    dot = sum(x*y for x,y in zip(a,b))
    na = vector_norm(a) or 1.0
    nb = vector_norm(b) or 1.0
    return dot/(na*nb)

# ---------------- Main logic ----------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--vector-field', default='contentVector_r')
    ap.add_argument('--source-summary-field', default='summary')
    ap.add_argument('--source-content-field', default='contentShort')
    ap.add_argument('--dims', type=int, default=VECTOR_DIMS_DEFAULT)
    ap.add_argument('--limit', type=int, default=200, help='Number of docs to sample/backfill')
    ap.add_argument('--page-size', type=int, default=100)
    ap.add_argument('--batch-size', type=int, default=32)
    ap.add_argument('--overwrite', action='store_true', help='Re-embed even if retrievable vector already present')
    ap.add_argument('--quality-report', action='store_true')
    ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()

    ensure_env(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])
    endpoint = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY')

    select = f"id,{args.source_summary_field},{args.source_content_field},{args.vector_field}"  # attempt retrieval of variant field

    collected: List[Tuple[str,str]] = []  # (id, text)
    skip = 0
    start=time.time()

    while len(collected) < args.limit:
        page = search_docs(endpoint,key,args.index,skip,args.page_size,select)
        if not page: break
        skip += len(page)
        for d in page:
            if len(collected) >= args.limit: break
            vec = d.get(args.vector_field)
            if vec and not args.overwrite:
                continue  # already present
            summary = d.get(args.source_summary_field,'') or ''
            content = (d.get(args.source_content_field,'') or '')[:4000]
            text = (summary + '\n\n' + content).strip() or summary or content
            if not text:
                continue
            collected.append((d['id'], text))
    print(f"Selected {len(collected)} docs for embedding (target {args.limit})")

    # Embed and upload in batches
    embedded_vectors: Dict[str,List[float]] = {}
    for i in range(0,len(collected), args.batch_size):
        batch = collected[i:i+args.batch_size]
        texts = [t for _,t in batch]
        vecs = []
        if not args.dry_run:
            vecs = embed_batch(texts, args.dims)
        else:
            vecs = [[0.0]*args.dims for _ in batch]
        for (doc_id,_), v in zip(batch, vecs):
            embedded_vectors[doc_id] = v
        if not args.dry_run:
            merge_docs = [{ '@search.action':'merge', 'id': doc_id, args.vector_field: embedded_vectors[doc_id]} for doc_id,_ in batch]
            upload_vectors(endpoint,key,args.index, merge_docs)
        elapsed = time.time()-start
        print(f"Batch {(i//args.batch_size)+1}: embedded {len(batch)} (total {len(embedded_vectors)}) elapsed {elapsed:.1f}s")

    if args.quality_report and embedded_vectors:
        vecs = list(embedded_vectors.values())
        norms = [vector_norm(v) for v in vecs]
        mean_norm = statistics.mean(norms)
        min_norm = min(norms)
        max_norm = max(norms)
        # sample up to 40 pairs for cosine stats
        pairs = []
        ids = list(embedded_vectors.keys())
        random.shuffle(ids)
        for a,b in zip(ids[0::2], ids[1::2]):
            if len(pairs) >= 40: break
            pairs.append(cosine(embedded_vectors[a], embedded_vectors[b]))
        cos_mean = statistics.mean(pairs) if pairs else 0.0
        cos_min = min(pairs) if pairs else 0.0
        cos_max = max(pairs) if pairs else 0.0
        report = {
            'sample_count': len(vecs),
            'dims': args.dims,
            'norm': {'mean': mean_norm, 'min': min_norm, 'max': max_norm},
            'pairwise_cosine': {'mean': cos_mean, 'min': cos_min, 'max': cos_max, 'samples': len(pairs)}
        }
        print('QUALITY_REPORT\n'+json.dumps(report, indent=2))

    print('DONE')

if __name__=='__main__':
    main()
