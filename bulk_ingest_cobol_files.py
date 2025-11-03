"""Bulk ingest all COBOL source files into the new-cobol-files index.

Features:
 - Scans one or more source roots for .cbl / .cob files
 - Extracts minimal metadata (programId from PROGRAM-ID, basic flags)
 - Generates embeddings (Azure OpenAI if configured; else deterministic fallback) sized to 3072 dims
 - Batches documents (default 32) using mergeOrUpload
 - Resumable: maintains a checkpoint file of ingested ids; can skip existing unless --force provided
 - Basic stats & rate limiting with simple exponential backoff
 - Validates vector dimension length before upload

Usage examples:
  python bulk_ingest_cobol_files.py --roots src legacy_src --batch-size 32
  python bulk_ingest_cobol_files.py --roots C:\Code\cobol --max-files 500 --dry-run

Environment:
  Requires AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (autoload via env_autoload + local.settings.json)
  Optional embedding: AZURE_OPENAI_* variables.

"""
from __future__ import annotations
import os, re, json, time, argparse, hashlib, struct, sys, gzip, io
from pathlib import Path
from typing import List, Dict, Iterable
import requests
import env_autoload

INDEX_NAME = 'new-cobol-files'
API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
VECTOR_DIMS = 3072
CHECKPOINT_FILE = '.bulk_ingest_cobol_checkpoint.json'
SUPPORTED_EXT = {'.cbl', '.cob', '.cobol', '.cpy', '.copy'}  # include copybooks

PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\.?\s+([A-Z0-9_-]+)', re.IGNORECASE)
COPY_RE = re.compile(r'COPY\s+([A-Z0-9_-]+)', re.IGNORECASE)
EXEC_SQL_RE = re.compile(r'EXEC\s+SQL', re.IGNORECASE)
CICS_RE = re.compile(r'EXEC\s+CICS', re.IGNORECASE)
SCREEN_RE = re.compile(r'SCREEN\s+SECTION', re.IGNORECASE)
PARA_RE = re.compile(r'^(\s{0,7}[A-Z0-9_-]+)\.', re.MULTILINE)
DEFINE_RE = re.compile(r'(?:01|77|78)\s+([A-Z0-9_-]+)')
REF_RE = re.compile(r'\b([A-Z][A-Z0-9_-]{2,})\b')

# ---------- Embeddings ----------

def have_azure_openai() -> bool:
    return all(os.getenv(k) for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'])

def embed_text_azure_openai(texts: List[str]) -> List[List[float]]:
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    key = os.getenv('AZURE_OPENAI_KEY')
    dep = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    url = f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json={'input': texts}, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding batch failed {r.status_code}: {r.text[:200]}")
    data = r.json()
    out: List[List[float]] = [d['embedding'] for d in data['data']]
    for vec in out:
        if len(vec) != VECTOR_DIMS:
            raise ValueError(f"Embedding dimension mismatch expected {VECTOR_DIMS} got {len(vec)}")
    return out

def embed_text_fallback(text: str) -> List[float]:
    seed = hashlib.sha256(text.encode('utf-8')).digest()
    floats: List[float] = []
    while len(floats) < VECTOR_DIMS:
        for i in range(0, len(seed), 4):
            if len(floats) >= VECTOR_DIMS:
                break
            chunk = seed[i:i+4]
            if len(chunk) < 4:
                chunk = chunk.ljust(4,b'\0')
            val = struct.unpack('!I', chunk)[0]
            floats.append(((val % 1000) / 1000.0) - 0.5)
        seed = hashlib.sha256(seed).digest()
    return floats

def batch_embeddings(texts: List[str]) -> List[List[float]]:
    if have_azure_openai():
        try:
            return embed_text_azure_openai(texts)
        except Exception as e:
            print(f"WARNING embedding provider failed, fallback per-item: {e}")
    return [embed_text_fallback(t) for t in texts]

# ---------- File Discovery ----------

def discover_files(roots: List[str]) -> List[Path]:
    files: List[Path] = []
    for root in roots:
        p = Path(root)
        if not p.exists():
            continue
        for f in p.rglob('*'):
            if f.is_file() and f.suffix.lower() in SUPPORTED_EXT:
                files.append(f)
    return files

# ---------- Parsing & Doc Build ----------

def extract_program_id(text: str, fallback: str) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return fallback.upper()

def summarize_content(program: str, text: str) -> str:
    # naive summary: first 3 non-comment lines truncated
    lines = [ln.strip() for ln in text.splitlines() if ln.strip() and not ln.strip().startswith('*')][:5]
    joined = ' '.join(lines)[:800]
    return f"Program {program} summary: {joined}"

def stable_id(program: str, path: Path) -> str:
    # Stable short hash of relative path to avoid collisions for programs with multiple source fragments / copybooks
    rel = path.as_posix()
    h = hashlib.sha1(rel.encode('utf-8')).hexdigest()[:10]
    return f"{program}-{h}"

def build_document(path: Path, max_content_bytes: int = 0) -> Dict:
    # Read raw bytes then decode with replacement to avoid code page translation issues downstream.
    try:
        data = path.read_bytes()
        raw = data.decode('utf-8', errors='replace')
    except Exception:
        # Fallback to latin-1 with replacement if any unexpected decoding issue occurs
        try:
            raw = data.decode('latin-1', errors='replace')
        except Exception:
            raw = ''
    original_len = len(raw)
    if max_content_bytes and original_len > max_content_bytes:
        raw = raw[:max_content_bytes]
    # Short excerpt used for semantic config (avoid huge semantic payload). ~12k chars cap.
    content_short = raw[:12000]
    # NOTE: Previously we generated 'contentChunks' (overlapping segments) and stored them in the
    # document. The index schema no longer contains a 'contentChunks' field (to reduce payload
    # size and avoid large-term issues), so we omit it here. If chunking is reintroduced later,
    # update the index schema first and then re-add generation.
    program_guess = path.stem.upper()
    program = extract_program_id(raw, program_guess)
    paragraphs = sorted({m.group(1).strip().split()[0] for m in PARA_RE.finditer(raw) if len(m.group(1).strip())>0})[:200]
    defines = sorted({m.group(1).upper() for m in DEFINE_RE.finditer(raw)})[:400]
    # crude references: uppercase tokens that appear & not in defines
    refs = []
    for tok in set(REF_RE.findall(raw)):
        if tok not in defines and len(tok) > 3:
            refs.append(tok)
    refs = refs[:400]
    copybooks = sorted({m.group(1).upper() for m in COPY_RE.finditer(raw)})[:100]
    has_sql = bool(EXEC_SQL_RE.search(raw))
    has_cics = bool(CICS_RE.search(raw))
    has_screens = bool(SCREEN_RE.search(raw))
    summary = summarize_content(program, raw)
    vector = embed_text_fallback(summary)  # placeholder; replaced in batch pipeline unless disabled
    ext = path.suffix.lower()
    kind = 'copybook' if ext in {'.cpy', '.copy'} else 'program'
    doc = {
        '@search.action': 'mergeOrUpload',
        'id': stable_id(program, path),
        'programId': program,
        'kind': kind,
        'path': str(path.as_posix()),
        'language': 'COBOL',
        'sloc': raw.count('\n') + 1,
        'hasSQL': has_sql,
        'hasCICS': has_cics,
        'hasScreens': has_screens,
        'paragraphs': paragraphs,
        'definesNames': defines,
        'referencesNames': refs,
        'copybooksUsed': copybooks,
        'summary': summary,
        'content': raw,  # full content retained
        'contentShort': content_short,
        'contentVector': vector,   # temporary; overwritten later if batch embedding
        'commit': 'bulk-ingest',
        'updatedAt': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    }
    return doc

# ---------- Checkpoint ----------

def load_checkpoint() -> Dict[str, float]:
    p = Path(CHECKPOINT_FILE)
    if p.exists():
        try:
            return json.loads(p.read_text())
        except Exception:
            return {}
    return {}

def save_checkpoint(done: Dict[str, float]):
    Path(CHECKPOINT_FILE).write_text(json.dumps(done, indent=2))

# ---------- Upload ----------

def upload_batch(docs: List[Dict], timeout: int = 120, use_gzip: bool = False):
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not endpoint or not key:
        raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')

    # Whitelist of allowed fields based on current new-cobol-files schema to prevent
    # accidental inclusion of deprecated/unknown properties (e.g., prior 'contentChunks').
    ALLOWED_FIELDS = {
        '@search.action','id','programId','kind','path','language','sloc','hasSQL','hasCICS','hasScreens',
        'paragraphs','definesNames','referencesNames','copybooksUsed','summary','content','contentShort',
        'contentVector','commit','updatedAt'
    }
    cleaned_docs = []
    for d in docs:
        if 'contentChunks' in d:
            # Hard prune any lingering key (should not exist after build change)
            d.pop('contentChunks', None)
        cleaned_docs.append({k: v for k, v in d.items() if k in ALLOWED_FIELDS})
    docs = cleaned_docs
    url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload = {'value': docs}
    # Prepare body (optionally gzip)
    body_bytes = json.dumps(payload).encode('utf-8')
    headers = {'api-key': key, 'Content-Type':'application/json'}
    if use_gzip:
        # NOTE: Azure Cognitive Search indexing endpoint currently rejects gzip-encoded
        # request bodies with a Unicode translation error (service attempts to treat
        # compressed binary as plain JSON). Until official support is confirmed, we
        # silently disable gzip here and emit a one-time warning to avoid 400 errors
        # like: "Unable to translate bytes [8B] at index 1 from specified code page to Unicode.".
        # If future service versions add support, this guard can be removed.
        if not getattr(upload_batch, '_warned_gzip', False):
            print("WARNING: gzip compression for indexing requests is disabled (service does not accept gzip bodies). Proceeding uncompressed.")
            upload_batch._warned_gzip = True  # type: ignore[attr-defined]
        # (Body remains uncompressed.)
    backoff = 2.0
    for attempt in range(5):
        r = requests.post(url, headers=headers, data=body_bytes, timeout=timeout)
        if r.status_code < 300:
            data = r.json()
            failed = [res for res in data.get('value', []) if not res.get('status')]
            if failed:
                print(f"WARNING: {len(failed)} docs reported failure statuses")
                # write a failure log for inspection
                fails = []
                for fdoc in failed:
                    fails.append({
                        'key': fdoc.get('key'),
                        'errorMessage': fdoc.get('errorMessage')
                    })
                with open('bulk_ingest_failures.log', 'a', encoding='utf-8') as fh:
                    for line in fails:
                        fh.write(json.dumps(line)+'\n')
            return data
        print(f"Upload attempt {attempt+1} failed {r.status_code}: {r.text[:180]}")
        time.sleep(backoff)
        backoff *= 1.8
    raise RuntimeError('Batch upload failed after retries')

def upload_with_split(docs: List[Dict], max_payload_bytes: int, timeout: int = 120, use_gzip: bool = False):
    """Upload docs splitting recursively if JSON payload exceeds max_payload_bytes.

    This guards against extremely large full-content batches causing connection stalls
    or server-side limits. max_payload_bytes=0 disables splitting.
    """
    if not max_payload_bytes or max_payload_bytes <= 0:
        return upload_batch(docs, timeout=timeout, use_gzip=use_gzip)
    # Estimate size
    data = json.dumps({'value': docs}).encode('utf-8')
    if len(data) <= max_payload_bytes:
        return upload_batch(docs, timeout=timeout, use_gzip=use_gzip)
    if len(docs) == 1:
        # Single doc still too large; last resort: truncate contentShort/content to soft cap
        d = docs[0]
        content = d.get('content','')
        if len(content) > 64000:
            d['content'] = content[:64000]
        cs = d.get('contentShort','')
        if len(cs) > 12000:
            d['contentShort'] = cs[:12000]
        return upload_batch(docs, timeout=timeout, use_gzip=use_gzip)
    mid = len(docs)//2
    upload_with_split(docs[:mid], max_payload_bytes, timeout=timeout, use_gzip=use_gzip)
    upload_with_split(docs[mid:], max_payload_bytes, timeout=timeout, use_gzip=use_gzip)


def adaptive_truncate_and_retry(failed_docs: List[Dict], soft_cap: int) -> List[Dict]:
    """Truncate oversized 'content' fields for docs that previously failed with large-term errors.

    We only apply this if the error log indicates the 32766-byte term limit issue. soft_cap is a
    character count (approx bytes for ASCII COBOL) to which we reduce the content while preserving
    contentShort (already limited) and summary. Returns the list of modified docs.
    """
    adjusted = []
    for d in failed_docs:
        content = d.get('content','')
        if len(content) > soft_cap:
            d['content'] = content[:soft_cap]
        # ensure contentShort still within bounds
        cs = d.get('contentShort','')
        if len(cs) > 12000:
            d['contentShort'] = cs[:12000]
        adjusted.append(d)
    return adjusted

# ---------- Main Pipeline ----------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--roots', nargs='+', required=True, help='Root directories to scan for COBOL files')
    ap.add_argument('--batch-size', type=int, default=32)
    ap.add_argument('--max-files', type=int, default=0, help='Optional limit for debugging')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--force', action='store_true', help='Ignore checkpoint and re-upload all')
    ap.add_argument('--no-embed-batch', action='store_true', help='Disable batch embedding replacement (leave fallback vectors)')
    ap.add_argument('--discover-only', action='store_true', help='Only list counts by extension and exit (diagnostics)')
    ap.add_argument('--max-content-bytes', type=int, default=0, help='Optional cap; 0 means no truncation (store entire file).')
    ap.add_argument('--only-file-list', type=str, default='', help='Optional path to a file containing explicit file paths (one per line) to ingest instead of scanning roots.')
    ap.add_argument('--adaptive-retry-log', type=str, default='bulk_ingest_failures.log', help='Failure log to parse for adaptive retries of large-term errors.')
    ap.add_argument('--adaptive-soft-cap', type=int, default=16000, help='Soft cap characters for truncation when large-term errors encountered.')
    ap.add_argument('--adaptive-retry', action='store_true', help='After initial pass, attempt adaptive truncation retries for docs that failed with large-term error.')
    ap.add_argument('--max-payload-bytes', type=int, default=2_000_000, help='Approximate max JSON payload size before splitting batch (0 disables).')
    ap.add_argument('--single-doc-mode', action='store_true', help='Upload each document in its own request (no pre-batching).')
    ap.add_argument('--progress-every', type=int, default=200, help='Progress reporting interval for single-doc-mode.')
    ap.add_argument('--stream', action='store_true', help='Stream build+upload batches instead of building full list first.')
    ap.add_argument('--gzip', action='store_true', help='Enable gzip compression for indexing requests.')
    ap.add_argument('--request-timeout', type=int, default=180, help='Timeout (seconds) for batch indexing requests.')
    ap.add_argument('--single-doc-timeout', type=int, default=45, help='Per-document request timeout (seconds) in single-doc-mode.')
    ap.add_argument('--shrink-on-timeout', action='store_true', help='If a single doc upload times out, progressively shrink its content and retry.')
    ap.add_argument('--shrink-ratio', type=float, default=0.8, help='Fraction of previous content length to retain on each shrink attempt.')
    ap.add_argument('--max-shrinks', type=int, default=5, help='Maximum number of shrink attempts per document when timeouts occur.')
    args = ap.parse_args()

    env_autoload.ensure(['AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY'])

    if args.only_file_list:
        list_path = Path(args.only_file_list)
        if not list_path.exists():
            raise SystemExit(f"--only-file-list file not found: {list_path}")
        listed = [Path(l.strip()) for l in list_path.read_text(encoding='utf-8').splitlines() if l.strip()]
        # Keep only those that actually exist & match supported ext
        all_files = [p for p in listed if p.exists() and p.is_file() and p.suffix.lower() in SUPPORTED_EXT]
        print(f"Loaded {len(all_files)} paths from {args.only_file_list}")
    else:
        all_files = discover_files(args.roots)
    if args.max_files:
        all_files = all_files[:args.max_files]
    print(f"Discovered {len(all_files)} candidate COBOL files")

    if args.discover_only:
        from collections import Counter
        counter = Counter([f.suffix.lower() for f in all_files])
        print("Extension distribution:")
        for ext, cnt in counter.most_common():
            print(f"  {ext or '(none)'}: {cnt}")
        sample_paths = list(str(p) for p in all_files[:20])
        print("Sample paths:")
        for sp in sample_paths:
            print(f"  {sp}")
        print("(Use --roots pointing at the actual COBOL source root directories if count is lower than expected.)")
        return

    checkpoint = {} if args.force else load_checkpoint()
    done_ids = set(checkpoint.keys())

    if args.single_doc_mode:
        from requests import Session
        from requests.exceptions import ReadTimeout, ConnectionError as ReqConnectionError
        endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or '').rstrip('/')
        key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        if not endpoint or not key:
            raise SystemExit('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY')
        url = f"{endpoint}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
        session = Session()
        total_candidates = len(all_files)
        uploaded = 0

        ALLOWED_FIELDS = {
            '@search.action','id','programId','kind','path','language','sloc','hasSQL','hasCICS','hasScreens',
            'paragraphs','definesNames','referencesNames','copybooksUsed','summary','content','contentShort',
            'contentVector','commit','updatedAt'
        }

        def single_upload(doc: Dict):
            clean = {k: v for k, v in doc.items() if k in ALLOWED_FIELDS}
            payload = {'value': [clean]}
            data = json.dumps(payload).encode('utf-8')
            r = session.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, data=data, timeout=args.single_doc_timeout)
            if r.status_code >= 300:
                raise RuntimeError(f"HTTP {r.status_code}: {r.text[:160]}")
            jr = r.json()
            val = jr.get('value', [])
            if val and not val[0].get('status', False):
                em = val[0].get('errorMessage')
                raise RuntimeError(f"Doc status failure: {em}")

        for idx, path in enumerate(all_files, 1):
            doc = build_document(path, args.max_content_bytes)
            if doc['id'] in done_ids and not args.force:
                continue
            attempt = 0
            shrinks = 0
            while attempt < 5:
                attempt += 1
                try:
                    single_upload(doc)
                    checkpoint[doc['id']] = time.time()
                    uploaded += 1
                    break
                except (ReadTimeout, ReqConnectionError) as e:
                    print(f"TIMEOUT/CONN attempt {attempt} doc {doc['id']} size={len(doc.get('content',''))}: {e}")
                    if args.shrink_on_timeout and shrinks < args.max_shrinks and len(doc.get('content','')) > 5000:
                        old_len = len(doc['content'])
                        new_len = max(1000, int(old_len * args.shrink_ratio))
                        doc['content'] = doc['content'][:new_len]
                        shrinks += 1
                        print(f"  Shrink #{shrinks}: {old_len} -> {new_len} chars; retrying immediately")
                        continue
                    wait = min(30, 2 ** attempt)
                    time.sleep(wait)
                except Exception as e:
                    print(f"ERROR attempt {attempt} doc {doc['id']}: {e}")
                    wait = min(30, 2 ** attempt)
                    time.sleep(wait)
            else:
                with open('bulk_ingest_failures.log','a',encoding='utf-8') as fh:
                    fh.write(json.dumps({'key': doc['id'], 'errorMessage': f'upload failed after {attempt} attempts'})+'\n')
            if uploaded and uploaded % 50 == 0:
                save_checkpoint(checkpoint)
            if uploaded and uploaded % args.progress_every == 0:
                pct = (idx / total_candidates) * 100 if total_candidates else 0
                print(f"Progress single-doc: uploaded {uploaded}; scanned {idx}/{total_candidates} ({pct:.1f}%)")
        save_checkpoint(checkpoint)
        print(f"DONE single-doc-mode. Uploaded {uploaded} new documents (skipped {len(done_ids)} pre-existing).")
        return

    # Streaming mode: build and upload batches on the fly
    if args.stream:
        batch: List[Dict] = []
        uploaded = 0
        start = time.time()
        for idx, path in enumerate(all_files, 1):
            doc = build_document(path, args.max_content_bytes)
            if doc['id'] in done_ids and not args.force:
                continue
            batch.append(doc)
            if len(batch) >= args.batch_size:
                upload_with_split(batch, args.max_payload_bytes, timeout=args.request_timeout, use_gzip=args.gzip)
                for d in batch:
                    checkpoint[d['id']] = time.time()
                save_checkpoint(checkpoint)
                uploaded += len(batch)
                elapsed = time.time() - start
                rate = uploaded/elapsed if elapsed>0 else 0
                # Per-batch progress (less noisy than per-doc; still gives visibility)
                print(f"Stream progress: {uploaded} docs uploaded so far ({rate:.2f}/sec)")
                batch = []
        if batch:
            upload_with_split(batch, args.max_payload_bytes, timeout=args.request_timeout, use_gzip=args.gzip)
            for d in batch:
                checkpoint[d['id']] = time.time()
            save_checkpoint(checkpoint)
            uploaded += len(batch)
        total_elapsed = time.time() - start
        print(f"DONE stream mode. Uploaded {uploaded} docs in {total_elapsed:.1f}s ({uploaded/total_elapsed if total_elapsed>0 else 0:.2f} docs/sec)")
        return

    # Legacy full-build path (non-stream)
    docs: List[Dict] = []
    new_count = 0
    for idx, path in enumerate(all_files, 1):
        doc = build_document(path, args.max_content_bytes)
        if doc['id'] in done_ids and not args.force:
            continue
        docs.append(doc)
        new_count += 1
        if idx % 500 == 0:
            print(f"Prepared {idx} files so far... current added {new_count}")
    print(f"Prepared {new_count} new documents (skipped {len(done_ids)} from checkpoint)")

    if args.dry_run:
        print("Dry run complete: no uploads performed.")
        return

    # Batch embedding replacement
    if not args.no_embed_batch and have_azure_openai():
        print("Generating embeddings in batches...")
        total = len(docs)
        last_logged = time.time()
        for i in range(0, len(docs), args.batch_size):
            batch = docs[i:i+args.batch_size]
            # Use summary + truncated semantic excerpt ensuring stable dimension context
            texts = [d['summary'] + '\n' + d.get('contentShort','')[:4000] for d in batch]
            vectors = batch_embeddings(texts)
            for d, vec in zip(batch, vectors):
                d['contentVector'] = vec
            if (i // args.batch_size) % 50 == 0 or time.time() - last_logged > 30:
                done = min(i + args.batch_size, total)
                pct = (done / total) * 100 if total else 0
                print(f"  Embedding progress: {done}/{total} ({pct:.1f}%)")
                last_logged = time.time()
        print("Embeddings generated.")
    else:
        if not have_azure_openai():
            print("Azure OpenAI not configured: using deterministic fallback vectors already assigned.")
        else:
            print("Embedding batch disabled via --no-embed-batch; fallback vectors retained.")

    # Sanity check vector dims
    bad = [d['id'] for d in docs if len(d['contentVector']) != VECTOR_DIMS]
    if bad:
        raise SystemExit(f"Vector dimension mismatch on {len(bad)} docs; aborting")

    start = time.time()
    uploaded = 0
    for i in range(0, len(docs), args.batch_size):
        batch = docs[i:i+args.batch_size]
        upload_with_split(batch, args.max_payload_bytes, timeout=args.request_timeout, use_gzip=args.gzip)
        for d in batch:
            checkpoint[d['id']] = time.time()
        save_checkpoint(checkpoint)
        uploaded += len(batch)
        if (i//args.batch_size) % 10 == 0:
            elapsed = time.time() - start
            rate = uploaded / elapsed if elapsed>0 else 0
            print(f"Progress: {uploaded}/{len(docs)} ({rate:.2f} docs/sec)")
    total_elapsed = time.time() - start
    print(f"DONE. Uploaded {uploaded} docs in {total_elapsed:.1f}s ({uploaded/total_elapsed if total_elapsed>0 else 0:.2f} docs/sec)")

    # Adaptive retry phase (optional)
    if args.adaptive_retry and os.path.exists(args.adaptive_retry_log):
        print("Adaptive retry enabled: scanning failure log for large-term errors ...")
        large_term_keys = []
        with open(args.adaptive_retry_log, 'r', encoding='utf-8') as fh:
            for line in fh:
                try:
                    rec = json.loads(line)
                except Exception:
                    continue
                msg = (rec.get('errorMessage') or '').lower()
                if 'contains a term that is too large' in msg and rec.get('key'):
                    large_term_keys.append(rec['key'])
        large_term_keys = sorted(set(large_term_keys))
        if not large_term_keys:
            print("No large-term failures detected for adaptive retry.")
            return
        print(f"Found {len(large_term_keys)} large-term failure docs. Rebuilding and truncating content <= {args.adaptive_soft_cap} chars ...")
        # Rebuild docs for those keys by re-reading source paths (need mapping id->file path). For this we re-scan files.
        # Build a map from stable id to path again.
        id_to_path = {}
        # Reconstruct file list similarly to earlier (respect roots / only-file-list flags).
        if args.only_file_list:
            list_path = Path(args.only_file_list)
            listed = [Path(l.strip()) for l in list_path.read_text(encoding='utf-8').splitlines() if l.strip()]
            all_files_retry = [p for p in listed if p.exists() and p.is_file() and p.suffix.lower() in SUPPORTED_EXT]
        else:
            all_files_retry = discover_files(args.roots)
        for p in all_files_retry:
            raw = p.read_text(errors='ignore')
            program_guess = p.stem.upper()
            program = extract_program_id(raw, program_guess)
            sid = stable_id(program, p)
            id_to_path[sid] = p
        retry_docs = []
        for key_id in large_term_keys:
            p = id_to_path.get(key_id)
            if not p:
                continue
            d = build_document(p, max_content_bytes=0)
            retry_docs.append(d)
        # Apply adaptive truncation
        retry_docs = adaptive_truncate_and_retry(retry_docs, args.adaptive_soft_cap)
        print(f"Retrying {len(retry_docs)} truncated docs in batches of {args.batch_size} ...")
        for i in range(0, len(retry_docs), args.batch_size):
            batch = retry_docs[i:i+args.batch_size]
            upload_batch(batch)
        print("Adaptive retry complete.")

if __name__ == '__main__':
    main()
