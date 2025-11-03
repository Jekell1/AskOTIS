#!/usr/bin/env python3
"""Backfill vector embeddings for code-chunks.text and cobol-symbols.(qualified_name or name).

Process:
 1. Adds/ensures vectorSearch configuration exists on service (if not already) – user must have created indexes with new fields first.
 2. Pages through documents (in batches) pulling required text fields.
 3. Calls Azure OpenAI embedding endpoint (text-embedding-3-large by default) to obtain vectors.
 4. Uploads partial documents with only key + vector field using @search.action=mergeOrUpload (idempotent).

Usage examples:
  python backfill_vectors.py --indexes code-chunks,cobol-symbols --batch 64 --max 1000

Prerequisites:
  local.settings.json Values must include:
    AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY
    AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY (or OPENAI_API_KEY with model override if using non-Azure)
  The indexes must already contain vector fields (text_vector, name_vector) as per modified schema.

Notes:
  - Uses simple retry for embedding calls.
  - Skips docs that already appear to have the vector field (unless --force specified).
  - You can resume safely; progress printed every flush.
"""
import os, json, sys, time, argparse, requests, math, random
from typing import List, Dict, Any

API_VERSION = "2024-07-01"
DEFAULT_EMBED_DEPLOYMENT = (
    os.getenv("AZURE_OPENAI_EMBED_DEPLOYMENT") or
    os.getenv("EMBED_DEPLOYMENT") or
    os.getenv("EMBED_MODEL") or  # backward compatibility if user previously set this
    "text-embedding-3-large"
)

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
    last_status = None
    last_body = None
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(
                url,
                headers={'api-key': openai_key, 'Content-Type':'application/json'},
                json=payload,
                timeout=90
            )
            last_status = r.status_code
            last_body = r.text[:300]
            if r.status_code == 200:
                data = r.json()
                return [d['embedding'] for d in data['data']]
            if r.status_code == 404 and 'DeploymentNotFound' in r.text:
                raise SystemExit(
                    f"Embedding deployment '{deployment}' not found at endpoint '{openai_ep}'.\n"
                    "Create an embedding deployment (e.g. model 'text-embedding-3-large' or '-small') in the Azure OpenAI resource, then rerun with --embed-deployment <name>."
                )
            retriable = r.status_code in (429, 408) or r.status_code >= 500
            if not retriable:
                break
            if attempt < max_attempts:
                wait = (base_backoff ** attempt) + random.random()
                print(f"embed: transient {r.status_code} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
        except requests.exceptions.RequestException as e:
            # Network/timeout errors – retry with backoff
            msg = str(e)
            if 'NameResolutionError' in msg or 'getaddrinfo failed' in msg:
                if attempt == 1:
                    print(f"embed: DNS resolution failing for host in endpoint '{openai_ep}'. Check network/VPN/DNS. Will retry.")
            if attempt < max_attempts:
                wait = (base_backoff ** attempt) + random.random()
                print(f"embed: network {type(e).__name__} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            last_body = str(e)
            break
    raise RuntimeError(f"Embedding failed after {max_attempts} attempts: status={last_status} body={last_body}")

def search_docs(search_ep: str, search_key: str, index: str, select: str, top: int, skip: int = 0, orderby: str = None, filter_expr: str = None) -> List[Dict[str,Any]]:
    """Generalized doc page retrieval supporting either skip or key-based filtering.

    If orderby and filter_expr provided, we omit skip (works around 100k skip ceiling).
    """
    url = f"{search_ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    body: Dict[str,Any] = {"search":"*", "top": top, "select": select}
    if orderby:
        body["orderby"] = orderby
    if filter_expr:
        body["filter"] = filter_expr
    else:
        body["skip"] = skip
    max_attempts = 7
    backoff = 1.6
    last_exc = None
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key':search_key,'Content-Type':'application/json'}, json=body, timeout=60)
            if r.status_code == 200:
                return r.json().get('value',[])
            retriable = r.status_code >= 500 or r.status_code == 429 or r.status_code == 408
            if not retriable:
                raise RuntimeError(f"Search page failed {r.status_code}: {r.text[:200]}")
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: ⚠️ search transient {r.status_code} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise RuntimeError(f"Search page failed after retries {r.status_code}: {r.text[:200]}")
        except requests.exceptions.RequestException as e:
            last_exc = e
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: ⚠️ network search exception {type(e).__name__} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise RuntimeError(f"Search page network failure after {max_attempts} attempts: {e}")
    # Should not reach
    if last_exc:
        raise RuntimeError(f"Search failed: {last_exc}")
    return []

def upload_vectors(search_ep: str, search_key: str, index: str, docs: List[Dict[str,Any]]):
    if not docs:
        return
    url = f"{search_ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    max_attempts = 6
    backoff = 1.5
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key':search_key,'Content-Type':'application/json'}, json=payload, timeout=120)
            status = r.status_code
            retriable = status == 429 or status >= 500
            if status < 300:
                return
            # Handle missing field 'has_vector' gracefully (schema not yet updated for this index)
            if status == 400 and 'has_vector' in r.text:
                # Strip has_vector from docs and retry once without counting against attempts aggressively
                removed = False
                for d in payload.get('value', []):
                    if 'has_vector' in d:
                        d.pop('has_vector', None)
                        removed = True
                if removed:
                    print(f"{index}: 'has_vector' field not present in index schema; continuing without it.")
                    # retry immediately (do not increment attempt to preserve retry budget for real transients)
                    continue
            if retriable and attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: ⚠️ vector upload transient {status} attempt {attempt}/{max_attempts}; retrying in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise RuntimeError(f"Vector upload failed {status}: {r.text[:200]}")
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"{index}: ⚠️ network exception {type(e).__name__} attempt {attempt}/{max_attempts}; retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise

def ensure_has_vector_field(search_ep: str, search_key: str, index: str, add_if_missing: bool = True) -> bool:
    """Check if an index has a 'has_vector' boolean field; add it if missing (and permitted).

    Returns True if field will be usable (already existed or successfully added), else False.
    """
    try:
        url = f"{search_ep}/indexes/{index}?api-version={API_VERSION}"
        r = requests.get(url, headers={'api-key': search_key})
        if r.status_code != 200:
            print(f"{index}: unable to fetch index definition ({r.status_code}); skipping has_vector ensure")
            return False
        definition = r.json()
        fields = definition.get('fields', [])
        if any(f.get('name') == 'has_vector' for f in fields):
            return True
        if not add_if_missing:
            return False
        # Append new boolean field (non-searchable, filterable for coverage queries if desired)
        fields.append({
            'name': 'has_vector',
            'type': 'Edm.Boolean',
            'searchable': False,
            'filterable': True,
            'facetable': False,
            'sortable': False,
            'retrievable': True
        })
        # Remove read-only metadata to avoid update conflicts
        for k in list(definition.keys()):
            if k.startswith('@'):
                definition.pop(k, None)
        definition['fields'] = fields
        u = requests.put(url, headers={'api-key': search_key, 'Content-Type': 'application/json', 'If-Match': '*'}, json=definition)
        if u.status_code in (200, 201, 204):
            # Re-fetch (with small polling) to confirm presence – index updates can be eventually consistent
            for attempt in range(6):  # ~ (0 + 0.5 + 1 + 1.5 + 2 + 2.5) = ~7.5s worst case
                vr = requests.get(url, headers={'api-key': search_key})
                if vr.status_code == 200:
                    vdef = vr.json(); vfields = vdef.get('fields', [])
                    if any(f.get('name') == 'has_vector' for f in vfields):
                        if attempt == 0:
                            print(f"{index}: 'has_vector' field present (added).")
                        else:
                            print(f"{index}: 'has_vector' field present after {attempt} poll(s).")
                        return True
                # brief incremental backoff
                time.sleep(0.5 * attempt)
            print(f"{index}: attempted to add 'has_vector' (HTTP {u.status_code}) but field not visible after polling; proceeding without flag support this run.")
            return False
        print(f"{index}: failed to add 'has_vector' field ({u.status_code}) {u.text[:160]}")
        return False
    except requests.exceptions.RequestException as e:
        print(f"{index}: exception ensuring has_vector field: {e}")
        return False

def write_progress(progress_path: str, index: str, processed: int, last_key: str | None = None, scanned: int | None = None):
    """Atomically write/update a simple JSON progress file.

    Extended structure:
      {
        "indexes": {
          index_name: {
            "processed": int,          # number of embedding operations performed (batches aggregated)
            "last_key": str | null,    # last successfully embedded document key (for precise resume)
            "scanned": int | null,     # optional count of docs scanned when fast-forwarding
            "ts": epoch_seconds
          }
        }
      }
    New fields are optional; older progress files without them remain compatible.
    """
    if not progress_path:
        return
    import json, time, os
    data = {"indexes": {}}
    try:
        if os.path.exists(progress_path):
            with open(progress_path, 'r', encoding='utf-8') as f:
                data = json.load(f) or data
    except Exception:
        data = {"indexes": {}}
    entry = {"processed": processed, "ts": int(time.time())}
    if last_key is not None:
        entry["last_key"] = last_key
    if scanned is not None:
        entry["scanned"] = scanned
    data.setdefault("indexes", {})[index] = entry
    tmp_path = progress_path + ".tmp"
    for attempt in range(1, 6):
        try:
            with open(tmp_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, ensure_ascii=False, indent=2)
            try:
                os.replace(tmp_path, progress_path)
            except Exception:
                pass
            break
        except PermissionError:
            if attempt == 5:
                return
            time.sleep(0.15 * attempt)


def process_index(search_ep: str, search_key: str, openai_ep: str, openai_key: str, index: str, batch: int, max_docs: int, force: bool, embed_deployment: str, progress_path: str, start_skip: int = 0, include_has_vector: bool = False, embed_retries: int = 7, skip_failed_batches: bool = False, failed_log: str = "", resume_last_key: str | None = None):
    """Iterate through documents in an index and backfill vector embeddings.

    If the vector field is not marked retrievable (common to reduce payload size), we fall back
    to selecting only the key + text field and will NOT be able to skip already‑embedded docs
    (unless --force is false and we had previously made them retrievable). This is acceptable
    for an initial backfill; duplicates simply overwrite via mergeOrUpload, which is idempotent.
    """
    if index == 'code-chunks':
        key_field = 'chunk_id'; primary_text_field = 'text'; vector_field = 'text_vector'
        fallback_fields = [primary_text_field]
    elif index == 'cobol-symbols':
        # Some symbol docs may have empty or missing qualified_name but do have a 'name'.
        # Use qualified_name first (more specific), then fall back to name to avoid losing coverage.
        key_field = 'item_id'; primary_text_field = 'qualified_name'; vector_field = 'name_vector'
        fallback_fields = ['qualified_name', 'name']
    elif index == 'cobol-paragraphs':
        key_field = 'para_id'; primary_text_field = 'name'; vector_field = 'name_vector'
        fallback_fields = [primary_text_field]
    elif index == 'cobol-calls':
        key_field = 'call_id'; primary_text_field = 'snippet'; vector_field = 'snippet_vector'
        fallback_fields = [primary_text_field]
    elif index == 'cobol-xrefs':
        key_field = 'xref_id'; primary_text_field = 'snippet'; vector_field = 'snippet_vector'
        fallback_fields = [primary_text_field]
    else:
        print(f"Skipping unsupported index {index}")
        return

    processed = 0
    # processed counts embedding operations performed this run.
    # start_skip is an approximate resume location (number of docs previously processed/embedded in prior runs).
    # We'll keep a separate scanned counter to make resume behavior transparent when we switch to key-based paging.
    skip = max(start_skip, 0)
    scanned = 0  # number of documents examined in this run (regardless of embedding)
    resume_target = skip if skip > 0 else None
    page_size = 100
    t0 = time.time()
    vector_retrievable = True
    # If resuming beyond safe threshold, start directly in key-based paging mode to avoid invalid large $skip
    use_key_paging = False
    last_key = None
    SAFE_SKIP_THRESHOLD = 95000  # switch before hitting 100k service limit
    if resume_last_key and not force:
        # Precise resume via stored last_key avoids expensive fast-forward scanning.
        use_key_paging = True
        last_key = resume_last_key
        print(f"{index}: precise resume after last_key={last_key}")
    elif skip >= SAFE_SKIP_THRESHOLD:
        # We cannot issue an initial skip that large (service limit 100k). We'll fetch first page with key-based paging.
        use_key_paging = True
        print(f"{index}: resume skip={skip} >= {SAFE_SKIP_THRESHOLD}; starting with key-based paging.")

    # Heartbeat state
    last_activity_time = time.time()  # time of last embedding upload (or start)
    HEARTBEAT_INTERVAL = 45  # seconds
    pages_since_activity = 0

    while True:
        # Build select list including fallback text fields; drop vector field if previously detected as non‑retrievable
        base_fields = [key_field] + fallback_fields
        if vector_retrievable:
            base_fields.append(vector_field)
        # de-duplicate while preserving order
        seen = {}
        ordered = [seen.setdefault(f, f) for f in base_fields if f not in seen]
        select_fields = ",".join(ordered)
        try:
            if use_key_paging:
                # filter greater than last_key (string compare) – keys must be sortable strings; rely on index recreation with sortable key
                filter_expr = None
                if last_key is not None:
                    # escape single quote in key if any
                    esc = last_key.replace("'","''")
                    filter_expr = f"{key_field} gt '{esc}'"
                docs = search_docs(search_ep, search_key, index, select_fields, page_size, orderby=f"{key_field} asc", filter_expr=filter_expr)
            else:
                docs = search_docs(search_ep, search_key, index, select_fields, page_size, skip=skip)
        except RuntimeError as e:
            msg = str(e)
            if 'not a retrievable field' in msg and vector_retrievable:
                vector_retrievable = False
                print(f"{index}: vector field '{vector_field}' not retrievable; continuing without it (cannot skip existing vectors).")
                # retry immediately without advancing skip
                continue
            raise

        if not docs:
            # Before breaking, emit a final heartbeat if we were silent while paging
            idle_for = time.time() - last_activity_time
            if idle_for >= HEARTBEAT_INTERVAL:
                loc = f"skip={skip}" if not use_key_paging else f"last_key={last_key}"
                print(f"{index}: heartbeat idle={idle_for:.0f}s pages={pages_since_activity} (no more docs) {loc}")
            break
        if use_key_paging:
            last_key = docs[-1][key_field]
        else:
            skip += len(docs)
            if skip >= SAFE_SKIP_THRESHOLD:
                use_key_paging = True
                last_key = docs[-1][key_field]
                print(f"{index}: switching to key-based paging at skip={skip} last_key={last_key}")
        scanned += len(docs)
        if resume_target is not None and use_key_paging:
            # Provide periodic visibility that we are advancing through already-covered territory.
            # We can't rely on 'skip' anymore, so we show cumulative scanned vs resume target until first embedding batch.
            if processed == 0 and scanned % 5000 < page_size:  # roughly every 5k docs
                print(f"{index}: fast-forward scanned~{scanned} toward prior processed={resume_target} (no new embeddings yet)")
        pages_since_activity += 1

        work_batch_texts: List[str] = []
        work_batch_keys: List[str] = []

        for d in docs:
            if max_docs and processed >= max_docs:
                break
            # Choose first non-empty fallback text field
            txt = ''
            for tf in fallback_fields:
                val = d.get(tf)
                if isinstance(val, str) and val.strip():
                    txt = val
                    break
            if not txt:
                continue  # no usable text
            if vector_retrievable:
                has_vec = d.get(vector_field) is not None and isinstance(d.get(vector_field), list) and len(d.get(vector_field)) > 10
            else:
                has_vec = False  # cannot know; assume missing so we (re)write
            if has_vec and not force:
                continue
            work_batch_texts.append(txt[:7000])  # rough safety trim
            work_batch_keys.append(d[key_field])
            processed += 1
            if len(work_batch_texts) == batch:
                try:
                    vecs = embed_texts(openai_ep, openai_key, work_batch_texts, embed_deployment, embed_retries)
                except Exception as e:
                    if skip_failed_batches:
                        if failed_log:
                            try:
                                with open(failed_log, 'a', encoding='utf-8') as flog:
                                    flog.write(json.dumps({
                                        'index': index,
                                        'keys': work_batch_keys,
                                        'error': str(e),
                                        'ts': int(time.time())
                                    }) + '\n')
                            except Exception:
                                pass
                        print(f"{index}: ⚠️ embedding batch failed ({len(work_batch_keys)} docs) skipped: {e}")
                        work_batch_texts.clear(); work_batch_keys.clear()
                        continue
                    raise
                if include_has_vector:
                    out_docs = [{"@search.action": "mergeOrUpload", key_field: k, vector_field: v, "has_vector": True} for k, v in zip(work_batch_keys, vecs)]
                else:
                    out_docs = [{"@search.action": "mergeOrUpload", key_field: k, vector_field: v} for k, v in zip(work_batch_keys, vecs)]
                upload_vectors(search_ep, search_key, index, out_docs)
                rate = processed / (time.time() - t0)
                loc = f"skip={skip}" if not use_key_paging else f"last_key={last_key}"
                last_batch_key = work_batch_keys[-1] if work_batch_keys else None
                print(f"{index}: processed={processed} {loc} scanned={scanned} rate={rate:,.1f} docs/s")
                write_progress(progress_path, index, processed, last_key=last_batch_key or last_key, scanned=scanned)
                last_activity_time = time.time(); pages_since_activity = 0
                work_batch_texts.clear(); work_batch_keys.clear()

        if work_batch_texts:
            try:
                vecs = embed_texts(openai_ep, openai_key, work_batch_texts, embed_deployment, embed_retries)
            except Exception as e:
                if skip_failed_batches:
                    if failed_log:
                        try:
                            with open(failed_log, 'a', encoding='utf-8') as flog:
                                flog.write(json.dumps({
                                    'index': index,
                                    'keys': work_batch_keys,
                                    'error': str(e),
                                    'ts': int(time.time())
                                }) + '\n')
                        except Exception:
                            pass
                    print(f"{index}: ⚠️ embedding batch failed ({len(work_batch_keys)} docs) skipped: {e}")
                    # do not upload, simply drop this partial batch
                    work_batch_texts.clear(); work_batch_keys.clear()
                    continue
                raise
            if include_has_vector:
                out_docs = [{"@search.action": "mergeOrUpload", key_field: k, vector_field: v, "has_vector": True} for k, v in zip(work_batch_keys, vecs)]
            else:
                out_docs = [{"@search.action": "mergeOrUpload", key_field: k, vector_field: v} for k, v in zip(work_batch_keys, vecs)]
            upload_vectors(search_ep, search_key, index, out_docs)
            rate = processed / (time.time() - t0)
            loc = f"skip={skip}" if not use_key_paging else f"last_key={last_key}"
            last_batch_key = work_batch_keys[-1] if work_batch_keys else None
            print(f"{index}: processed={processed} {loc} scanned={scanned} rate={rate:,.1f} docs/s")
            write_progress(progress_path, index, processed, last_key=last_batch_key or last_key, scanned=scanned)
            last_activity_time = time.time(); pages_since_activity = 0

        if max_docs and processed >= max_docs:
            break

        # Heartbeat if we've had no embedding uploads for the interval
        idle_for = time.time() - last_activity_time
        if idle_for >= HEARTBEAT_INTERVAL:
            loc = f"skip={skip}" if not use_key_paging else f"last_key={last_key}"
            print(f"{index}: heartbeat idle={idle_for:.0f}s pages={pages_since_activity} {loc} processed={processed} scanned={scanned}")
            # Reset timer so we don't spam every loop; next heartbeat after another interval
            last_activity_time = time.time(); pages_since_activity = 0

    print(f"Done {index}: total vectors upserted {processed} scanned={scanned} elapsed={time.time()-t0:.1f}s")
    # Final progress write (last_key may have advanced inside loop already)
    write_progress(progress_path, index, processed, last_key=last_key, scanned=scanned)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--indexes', default='code-chunks,cobol-symbols')
    ap.add_argument('--batch', type=int, default=64)
    ap.add_argument('--max', type=int, help='Limit number of documents (testing)')
    ap.add_argument('--force', action='store_true', help='Recreate vectors even if already present')
    ap.add_argument('--embed-deployment', help='Azure OpenAI embedding deployment name (env AZURE_OPENAI_EMBED_DEPLOYMENT also accepted).')
    ap.add_argument('--progress-file', help='Optional path to write periodic JSON progress.')
    ap.add_argument('--resume-from-progress', action='store_true', help='If set, attempt to resume each index using processed count in progress file as initial skip.')
    ap.add_argument('--auto-add-has-vector', action='store_true', help='Attempt to add has_vector field to each index if missing (recommended).')
    ap.add_argument('--embed-retries', type=int, default=7, help='Max attempts for each embedding batch (increase for flaky network).')
    ap.add_argument('--skip-failed-batches', action='store_true', help='On persistent embedding failure for a batch, skip it and continue instead of aborting.')
    ap.add_argument('--failed-log', help='Optional path to append JSON lines for failed batches when skipping.')
    args = ap.parse_args()
    search_ep, search_key, openai_ep, openai_key = load_settings()
    embed_dep = args.embed_deployment or DEFAULT_EMBED_DEPLOYMENT
    if not embed_dep:
        raise SystemExit("No embedding deployment specified. Use --embed-deployment or set AZURE_OPENAI_EMBED_DEPLOYMENT.")
    print(f"Using embedding deployment: {embed_dep}")
    resume_map = {}
    resume_key_map = {}
    if args.resume_from_progress and args.progress_file and os.path.exists(args.progress_file):
        try:
            with open(args.progress_file,'r',encoding='utf-8') as f:
                pj = json.load(f)
            for k,v in (pj.get('indexes') or {}).items():
                if isinstance(v, dict) and 'processed' in v:
                    resume_map[k] = int(v.get('processed') or 0)
                    lk = v.get('last_key')
                    if isinstance(lk, str) and lk:
                        resume_key_map[k] = lk
            if resume_map:
                print(f"Resume map loaded: { {k: resume_map[k] for k in resume_map} }")
        except Exception as e:
            print(f"⚠️ Failed to load resume progress: {e}")
    # Pre-flight ensure has_vector for each target index (optional)
    has_vector_support: Dict[str, bool] = {}
    for ix in [x.strip() for x in args.indexes.split(',') if x.strip()]:
        has_vec = False
        if args.auto_add_has_vector:
            has_vec = ensure_has_vector_field(search_ep, search_key, ix, add_if_missing=True)
        else:
            # Just detect without adding
            has_vec = ensure_has_vector_field(search_ep, search_key, ix, add_if_missing=False)
        has_vector_support[ix] = has_vec
    for ix in [x.strip() for x in args.indexes.split(',') if x.strip()]:
        # In force mode we intentionally ignore any stored progress so every document is reprocessed
        if args.force:
            stored_skip = resume_map.get(ix, 0)
            if stored_skip:
                print(f"{ix}: ignoring resume skip={stored_skip} due to --force (reprocessing from beginning)")
            start_skip = 0
        else:
            start_skip = resume_map.get(ix, 0)
            if start_skip:
                print(f"{ix}: starting with skip={start_skip} from progress file (approximate)")
        process_index(
            search_ep, search_key, openai_ep, openai_key,
            ix, args.batch, args.max or 0, args.force, embed_dep,
            args.progress_file or "", start_skip=start_skip,
            include_has_vector=has_vector_support.get(ix, False),
            embed_retries=args.embed_retries,
            skip_failed_batches=args.skip_failed_batches,
            failed_log=args.failed_log or "",
            resume_last_key=resume_key_map.get(ix)
        )

if __name__ == '__main__':
    main()
