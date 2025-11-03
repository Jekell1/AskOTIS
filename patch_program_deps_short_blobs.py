"""Patch short placeholder dependency blobs in new_cobol_program_deps.

Goal:
  Close the remaining coverage gap where docs have a very short placeholder
  `dependency_blob` (e.g. "ROLE: ; OUT: ; IN: ; COPY:") and still lack a
  vector (has_vector != true). These blobs contain almost no real semantic
  signal; you can either:

    1. Embed them normally (--embed-short) so downstream similarity still
       works uniformly.
    2. Assign a deterministic sentinel vector (--sentinel) to distinguish
       them from fully empty blobs (which may already be handled elsewhere).

Defaults:
  - By default (no --embed-short and no --sentinel), we will embed the short
    blobs (safer, ensures correct dimensionality even if model changes).

Sentinel Strategy:
  Produces a zero vector of length DIM (defaults 3072). Only use if you are
  comfortable treating these docs as effectively content‐less.

Usage Examples (PowerShell):
  python patch_program_deps_short_blobs.py --dry-run
  python patch_program_deps_short_blobs.py --max-len 60 --embed-short --batch 200
  python patch_program_deps_short_blobs.py --sentinel --max-len 55 --batch 400

Exit Codes:
  0 success (even if no docs patched)
  1 fatal configuration or HTTP errors
"""
from __future__ import annotations
import os, json, sys, time, argparse, requests, hashlib
from typing import List

API = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_deps'
VECTOR_FIELD = 'dependency_blob_vector'
HAS_FIELD = 'has_vector'
DIM = int(os.getenv('PROGRAM_DEPS_VECTOR_DIM','3072'))  # Keep consistent with index definition

def load_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
    except Exception:
        pass

def resolve_search():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

def resolve_embedding():
    """Resolve embeddings endpoint + deployment for optional embedding of short blobs.

    We support Azure OpenAI (preferred). If not configured and OPENAI_API_KEY
    is present, attempt public OpenAI.
    """
    from importlib import import_module
    model = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    try:
        openai_mod = import_module('openai')
    except Exception:
        openai_mod = None
    if azure_ep and azure_key and openai_mod is not None:
        try:
            from openai import AzureOpenAI  # type: ignore
            client = AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
            def _embed(texts:List[str]):
                resp = client.embeddings.create(model=model, input=texts)
                return [d.embedding for d in resp.data]
            return model, _embed, 'azure'
        except Exception:
            pass
    # Public OpenAI fallback
    public_key = os.getenv('OPENAI_API_KEY')
    if public_key and openai_mod is not None:
        openai_mod.api_key = public_key
        def _embed(texts:List[str]):
            resp = openai_mod.embeddings.create(model=model, input=texts)
            return [d.embedding for d in resp.data]
        return model, _embed, 'public'
    return None, None, 'none'

def fetch_page(ep,key, skip, top):
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    body = {'search':'*','top':top,'skip':skip,'select':f'program_id,dependency_blob,{HAS_FIELD}'}
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        print('Fetch error', r.status_code, r.text[:220], file=sys.stderr)
        sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key, docs):
    if not docs: return
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':docs}, timeout=60)
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:220], file=sys.stderr)
        sys.exit(1)

def deterministic_sentinel(program_id:str) -> List[float]:
    # Distinguish from pure zero vector: hashed seed writes a tiny non-zero at several spots
    h = hashlib.sha256(program_id.encode('utf-8')).digest()
    # Build sparse vector: mostly zeros, 8 hashed positions.
    vec = [0.0]*DIM
    for i in range(0, min(32,len(h)),4):
        pos = int.from_bytes(h[i:i+2],'big') % DIM
        mag_raw = int.from_bytes(h[i+2:i+4],'big') / 65535.0
        vec[pos] = (mag_raw*0.02)  # very small magnitude so retrieval weight minimal
    return vec

def main():
    ap = argparse.ArgumentParser(description='Patch short placeholder dependency blobs with vectors')
    ap.add_argument('--max-len', type=int, default=60, help='Maximum blob length considered a short placeholder')
    ap.add_argument('--batch', type=int, default=400, help='Docs scanned per fetch page')
    ap.add_argument('--limit', type=int, help='Stop after patching this many docs')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--embed-short', action='store_true', help='Actually embed short blobs instead of sentinel vector')
    ap.add_argument('--sentinel', action='store_true', help='Force sentinel vector (overrides --embed-short)')
    ap.add_argument('--show-sample', type=int, default=5, help='Print first N candidate examples')
    args = ap.parse_args()
    load_settings(); ep,key = resolve_search()

    model, embed_fn, mode = resolve_embedding()
    if args.embed_short and not embed_fn:
        print('Embedding requested but no embedding credentials resolved; falling back to sentinel.', file=sys.stderr)
        args.sentinel = True

    # If neither embed_short nor sentinel specified, default to embedding path when available
    if not args.embed_short and not args.sentinel:
        if embed_fn:
            args.embed_short = True
        else:
            args.sentinel = True

    print(json.dumps({'mode':'embedding' if args.embed_short and not args.sentinel else 'sentinel',
                      'embed_backend':mode,
                      'model':model,
                      'max_len':args.max_len,
                      'dry_run':args.dry_run}, indent=2))

    skip = 0; patched = 0; scanned = 0; sample_printed = 0
    start = time.time()
    while True:
        rows = fetch_page(ep,key,skip,args.batch)
        if not rows: break
        actions = []
        short_texts = []
        id_for_text = []
        for r in rows:
            if r.get(HAS_FIELD) is True:
                continue
            blob = (r.get('dependency_blob') or '')
            blen = len(blob)
            if blen == 0:
                # Empty blobs likely handled by finalize script; skip here.
                continue
            if blen <= args.max_len:
                if sample_printed < args.show_sample:
                    print(f"Candidate program_id={r['program_id']} len={blen} text={blob[:120]}")
                    sample_printed += 1
                if args.embed_short and not args.sentinel:
                    short_texts.append(blob)
                    id_for_text.append(r['program_id'])
                else:
                    vec = deterministic_sentinel(r['program_id']) if args.sentinel else [0.0]*DIM
                    actions.append({'@search.action':'mergeOrUpload','program_id':r['program_id'],HAS_FIELD:True, VECTOR_FIELD:vec})
        # Perform embedding for the collected short texts (if any)
        if short_texts:
            # Simple chunking to avoid large payloads
            for i in range(0,len(short_texts),64):
                chunk = short_texts[i:i+64]
                ids = id_for_text[i:i+64]
                embs = embed_fn(chunk) if embed_fn else []
                for pid, vec in zip(ids, embs):
                    actions.append({'@search.action':'mergeOrUpload','program_id':pid, HAS_FIELD: True, VECTOR_FIELD: vec})
        if actions and not args.dry_run:
            upload(ep,key,actions)
        patched += len(actions)
        scanned += len(rows)
        print(f"Scanned {scanned} patched={patched} skip={skip} (page={len(rows)})")
        if args.limit and patched >= args.limit:
            print('Patch limit reached'); break
        if len(rows) < args.batch:
            break
        skip += args.batch
    elapsed = time.time()-start
    print(json.dumps({'patched':patched,'scanned':scanned,'elapsed_sec':round(elapsed,2),'mode':'embed' if args.embed_short and not args.sentinel else 'sentinel'}, indent=2))

if __name__ == '__main__':
    main()
