#!/usr/bin/env python3
"""Targeted gap filler for Azure AI Search indexes: only embed docs where has_vector is missing.

Usage:
  python gap_fill_missing_vectors.py --indexes cobol-paragraphs,cobol-xrefs --batch 64

Requirements:
  local.settings.json with SEARCH + OPENAI credentials (same as backfill_vectors.py)

Notes:
  - Uses filter has_vector ne true to find missing vectors (works because has_vector is filterable & retrievable).
  - Idempotent: mergeOrUpload with has_vector=True.
  - Stops when a page returns zero results.
  - Skips documents with empty text source fields.
"""
import os, json, time, argparse, random, requests
from typing import List, Dict, Any

API_VERSION = "2024-07-01"
DEFAULT_EMBED_DEPLOYMENT = (
    os.getenv("AZURE_OPENAI_EMBED_DEPLOYMENT") or
    os.getenv("EMBED_DEPLOYMENT") or
    os.getenv("EMBED_MODEL") or
    "text-embedding-3-large"
)

INDEX_MAP = {
    "cobol-paragraphs": {"key": "para_id", "text_fields": ["name"], "vector": "name_vector"},
    "cobol-xrefs": {"key": "xref_id", "text_fields": ["snippet"], "vector": "snippet_vector"},
    "code-chunks": {"key": "chunk_id", "text_fields": ["text"], "vector": "text_vector"},
    "cobol-symbols": {"key": "item_id", "text_fields": ["qualified_name", "name"], "vector": "name_vector"},
    "cobol-calls": {"key": "call_id", "text_fields": ["snippet"], "vector": "snippet_vector"},
}

def load_settings():
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    search_ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
    search_key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    openai_ep = vals.get('AZURE_OPENAI_ENDPOINT') or vals.get('OPENAI_ENDPOINT')
    openai_key = vals.get('AZURE_OPENAI_KEY') or vals.get('OPENAI_API_KEY')
    if not (search_ep and search_key and openai_ep and openai_key):
        raise SystemExit("Missing one of required settings: SEARCH endpoint/key and OPENAI endpoint/key")
    return search_ep, search_key, openai_ep.rstrip('/'), openai_key

def embed_texts(openai_ep: str, openai_key: str, texts: List[str], deployment: str, max_attempts: int) -> List[List[float]]:
    url = f"{openai_ep}/openai/deployments/{deployment}/embeddings?api-version=2024-02-15-preview"
    payload = {"input": texts}
    base_backoff = 1.4
    last_status = None; last_body = None
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key': openai_key, 'Content-Type':'application/json'}, json=payload, timeout=90)
            last_status = r.status_code; last_body = r.text[:300]
            if r.status_code == 200:
                data = r.json(); return [d['embedding'] for d in data['data']]
            if r.status_code == 404 and 'DeploymentNotFound' in r.text:
                raise SystemExit(f"Embedding deployment '{deployment}' not found at endpoint '{openai_ep}'.")
            retriable = r.status_code in (429,408) or r.status_code >= 500
            if not retriable: break
            if attempt < max_attempts:
                wait = (base_backoff ** attempt) + random.random()
                print(f"embed: transient {r.status_code} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = (base_backoff ** attempt) + random.random()
                print(f"embed: network {type(e).__name__} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait); continue
            last_body = str(e); break
    raise RuntimeError(f"Embedding failed after {max_attempts} attempts: status={last_status} body={last_body}")

def search_missing(search_ep: str, search_key: str, index: str, select: str, top: int) -> List[Dict[str,Any]]:
    url = f"{search_ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body = {"search":"*","top":top,"select":select,"filter":"has_vector ne true"}
    max_attempts = 6; backoff = 1.5
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key':search_key,'Content-Type':'application/json'}, json=body, timeout=60)
            if r.status_code == 200:
                return r.json().get('value',[])
            # If vector field not retrievable and selected, service returns 400; bubble up for disabling fast flag
            if r.status_code == 400 and 'retrievable' in r.text.lower():
                raise RuntimeError(f"retrievable_error {r.text[:160]}")
            retriable = r.status_code >= 500 or r.status_code in (429,408)
            if not retriable:
                raise RuntimeError(f"search_missing {index} {r.status_code}: {r.text[:180]}")
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: transient search {r.status_code} attempt {attempt}/{max_attempts} retry {wait:.1f}s")
                time.sleep(wait); continue
            raise RuntimeError(f"search_missing failed after retries {r.status_code}")
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: network search {type(e).__name__} attempt {attempt}/{max_attempts} retry {wait:.1f}s")
                time.sleep(wait); continue
            raise
    return []

def count_total_and_missing(search_ep: str, search_key: str, index: str):
    url = f"{search_ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    def req(filter_expr=None):
        body = {"search":"*","top":0,"count":True}
        if filter_expr: body['filter']=filter_expr
        r = requests.post(url, headers={'api-key':search_key,'Content-Type':'application/json'}, json=body, timeout=60)
        if r.status_code != 200:
            raise RuntimeError(f"count {index} {r.status_code}: {r.text[:160]}")
        return r.json().get('@odata.count',0)
    total = req(); missing = req("has_vector ne true")
    return total, missing, total-missing

def upload_vectors(search_ep: str, search_key: str, index: str, docs: List[Dict[str,Any]]):
    if not docs: return
    url = f"{search_ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    max_attempts = 6; backoff = 1.5
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key':search_key,'Content-Type':'application/json'}, json=payload, timeout=120)
            if r.status_code < 300: return
            retriable = r.status_code == 429 or r.status_code >= 500
            if retriable and attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: upload transient {r.status_code} attempt {attempt}/{max_attempts} retry {wait:.1f}s")
                time.sleep(wait); continue
            raise RuntimeError(f"upload {index} failed {r.status_code}: {r.text[:180]}")
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: network upload {type(e).__name__} attempt {attempt}/{max_attempts} retry {wait:.1f}s")
                time.sleep(wait); continue
            raise

def process_index(search_ep: str, search_key: str, openai_ep: str, openai_key: str, index: str, batch: int, embed_deployment: str, embed_retries: int, max_per_index: int | None, fast_flag_existing: bool, page_size: int, progress_every: int):
    meta = INDEX_MAP.get(index)
    if not meta:
        print(f"Skipping unsupported index {index}"); return
    key_field = meta['key']; text_fields = meta['text_fields']; vector_field = meta['vector']
    # Page size influences payload size if vector field is retrieved; default lowered via arg for safety
    start_total, start_missing, start_have = count_total_and_missing(search_ep, search_key, index)
    print(f"{index}: start total={start_total} have={start_have} missing={start_missing} coverage={(start_have/start_total*100 if start_total else 0):.2f}%")
    # Tracking for progress percentage
    flagged_total = 0  # number of docs where we only set has_vector flag
    precise_flushes = 0
    last_precise_time = time.time()
    if start_missing == 0:
        return
    embedded = 0
    t0 = time.time()
    first_page = True
    vector_retrievable = True
    while True:
        if max_per_index and embedded >= max_per_index:
            print(f"{index}: reached max-per-index limit {max_per_index}")
            break
        base_fields = [key_field] + text_fields + ['has_vector']
        if fast_flag_existing and vector_retrievable:
            base_fields.append(vector_field)
        # de-dupe
        seen = set(); ordered = []
        for f in base_fields:
            if f not in seen:
                seen.add(f); ordered.append(f)
        select = ",".join(ordered)
        try:
            docs = search_missing(search_ep, search_key, index, select, page_size)
        except RuntimeError as e:
            msg = str(e)
            if 'retrievable_error' in msg and fast_flag_existing and first_page:
                print(f"{index}: vector field '{vector_field}' not retrievable; disabling fast-flag mode.")
                vector_retrievable = False
                fast_flag_existing = False
                first_page = False
                continue  # retry without vector field
            raise
        first_page = False
        if not docs:
            break

        texts = []
        keys = []
        flag_updates: List[Dict[str,Any]] = []

        for d in docs:
            # choose first non-empty text field
            txt = ''
            for tf in text_fields:
                val = d.get(tf)
                if isinstance(val,str) and val.strip():
                    txt = val; break
            key_val = d.get(key_field)
            if fast_flag_existing and vector_retrievable:
                v = d.get(vector_field)
                if isinstance(v, list) and len(v) > 10:
                    if d.get('has_vector') is not True:
                        flag_updates.append({"@search.action":"mergeOrUpload", key_field: key_val, "has_vector": True})
                    continue  # skip embedding
            if not txt:
                continue  # can't embed
            keys.append(key_val)
            texts.append(txt[:7000])
            if len(texts) == batch:
                vecs = embed_texts(openai_ep, openai_key, texts, embed_deployment, embed_retries)
                out = [{"@search.action":"mergeOrUpload", key_field:k, vector_field:v, "has_vector": True} for k,v in zip(keys,vecs)]
                upload_vectors(search_ep, search_key, index, out)
                embedded += len(out)
                rate = embedded / (time.time()-t0)
                print(f"{index}: embedded={embedded} rate={rate:,.1f} docs/s (batch embed)")
                texts.clear(); keys.clear()
                if max_per_index and embedded >= max_per_index:
                    break
            if len(flag_updates) >= 256:
                upload_vectors(search_ep, search_key, index, flag_updates)
                print(f"{index}: flagged {len(flag_updates)} existing vectors (no embed)")
                flag_updates.clear()

        if texts:
            vecs = embed_texts(openai_ep, openai_key, texts, embed_deployment, embed_retries)
            out = [{"@search.action":"mergeOrUpload", key_field:k, vector_field:v, "has_vector": True} for k,v in zip(keys,vecs)]
            upload_vectors(search_ep, search_key, index, out)
            embedded += len(out)
            rate = embedded / (time.time()-t0)
            print(f"{index}: embedded={embedded} rate={rate:,.1f} docs/s (final partial batch)")
        if flag_updates:
            upload_vectors(search_ep, search_key, index, flag_updates)
            flagged_total += len(flag_updates)
            print(f"{index}: flagged {len(flag_updates)} existing vectors (final flush)")

        # Progress (estimated) after each page
        current_have_est = start_have + embedded + flagged_total
        pct_est = (current_have_est / start_total * 100) if start_total else 0.0
        print(f"{index}: progress est {current_have_est}/{start_total} ({pct_est:.2f}%)")
        precise_flushes += 1
        if progress_every > 0 and precise_flushes % progress_every == 0:
            # Precise recount (can be relatively expensive; keep frequency moderate)
            _, missing_now, have_now = count_total_and_missing(search_ep, search_key, index)
            pct_now = (have_now / start_total * 100) if start_total else 0.0
            print(f"{index}: precise coverage {have_now}/{start_total} ({pct_now:.2f}%) missing={missing_now}")

    end_total, end_missing, end_have = count_total_and_missing(search_ep, search_key, index)
    print(f"{index}: end total={end_total} have={end_have} missing={end_missing} coverage={(end_have/end_total*100 if end_total else 0):.2f}% (added {end_have-start_have} vectors this run)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--indexes', required=True, help='Comma-separated list of indexes to gap fill.')
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--embed-deployment', help='Embedding deployment name.')
    ap.add_argument('--embed-retries', type=int, default=7)
    ap.add_argument('--max-per-index', type=int, help='Optional limit for testing.')
    ap.add_argument('--fast-flag-existing', action='store_true', help='If vector field retrievable, set has_vector=True without re-embedding when vector already present.')
    ap.add_argument('--page-size', type=int, default=128, help='Search page size (reduce if payload too large).')
    ap.add_argument('--progress-every', type=int, default=15, help='After this many pages do a precise count (0=disable precise periodic recount).')
    args = ap.parse_args()
    search_ep, search_key, openai_ep, openai_key = load_settings()
    embed_dep = args.embed_deployment or DEFAULT_EMBED_DEPLOYMENT
    if not embed_dep:
        raise SystemExit('No embedding deployment resolved.')
    print(f"Using embedding deployment: {embed_dep}")
    for ix in [x.strip() for x in args.indexes.split(',') if x.strip()]:
        process_index(
            search_ep, search_key, openai_ep, openai_key,
            ix, args.batch, embed_dep, args.embed_retries,
            args.max_per_index, args.fast_flag_existing, args.page_size, args.progress_every
        )

if __name__ == '__main__':
    main()
