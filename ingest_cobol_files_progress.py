#!/usr/bin/env python3
"""ingest_cobol_files_progress.py

Python-only variant of file-level COBOL ingestion with inline progress display.
Reimplements the core logic of ingest_cobol_files.py while:
  * Auto-loading credentials (Azure Search + Azure OpenAI) from environment or local.settings.json
  * Showing step-by-step progress (file scan, filtering, embedding batches, upload batches)
  * Avoiding PowerShell-specific environment gymnastics

Features:
  - Skips unchanged files by content hash (SHA1) unless --force
  - Optional embeddings (Azure OpenAI) with dimension compression (3072->1536) and retry/backoff
  - Batching for both embeddings & uploads
  - Dry-run / sampling modes
  - Simple ETA estimation

Environment (fallback order: env vars, then local.settings.json Values):
  SEARCH_ENDPOINT / AZURE_SEARCH_ENDPOINT
  SEARCH_KEY / AZURE_SEARCH_KEY
  AZURE_OPENAI_ENDPOINT
  AZURE_OPENAI_KEY
  AZURE_OPENAI_EMBED_MODEL or AZURE_OPENAI_EMBED_DEPLOYMENT (default text-embedding-3-large)
  FACT_FILE_EMBED_DIM (default 1536)

Usage examples:
  python ingest_cobol_files_progress.py --root ./src --embed
  python ingest_cobol_files_progress.py --root ./src --embed --sample 50 --embed-batch-size 8
  python ingest_cobol_files_progress.py --root ./src --dry-run --embed
"""
from __future__ import annotations
import os, sys, json, argparse, hashlib, time, math, random, re
from pathlib import Path
from typing import List, Dict, Any, Optional
import requests
try:
    import tiktoken  # optional for accurate token counting
    _HAS_TIKTOKEN = True
except Exception:
    _HAS_TIKTOKEN = False

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
INDEX_NAME = os.getenv('COBOL_FILE_INDEX','cobol-files-v1')
VECTOR_FIELD = 'full_text_vector'
COBOL_EXTS = {'.cbl','.cob','.cobol','.cpy','.cb2'}
PROGRAM_ID_RE = re.compile(r'PROGRAM-ID\s+([A-Z0-9_-]+)\.', re.IGNORECASE)
EMBED_MODEL = (os.getenv('AZURE_OPENAI_EMBED_MODEL') or os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT') or 'text-embedding-3-large')
EXPECTED_DIM = int(os.getenv('FACT_FILE_EMBED_DIM','1536'))

DEFAULT_EMBED_BATCH = int(os.getenv('EMBED_BATCH_SIZE','8'))
DEFAULT_EMBED_DELAY = float(os.getenv('EMBED_DELAY_SECONDS','0.25'))
DEFAULT_EMBED_MAX_RETRIES = int(os.getenv('EMBED_MAX_RETRIES','5'))
BACKOFF_BASE = float(os.getenv('EMBED_BACKOFF_BASE','1.5'))
BACKOFF_JITTER = float(os.getenv('EMBED_BACKOFF_JITTER','0.1'))

# Adaptive throttling defaults
ADAPTIVE_DEFAULT_INITIAL_DELAY = float(os.getenv('ADAPTIVE_INITIAL_DELAY','0.40'))
ADAPTIVE_DEFAULT_MAX_DELAY = float(os.getenv('ADAPTIVE_MAX_DELAY','6.0'))
ADAPTIVE_THROTTLE_MULT = float(os.getenv('ADAPTIVE_THROTTLE_MULT','1.35'))
ADAPTIVE_RECOVERY_DIV = float(os.getenv('ADAPTIVE_RECOVERY_DIV','1.12'))  # gradual step down
ADAPTIVE_COOLDOWN_BATCHES = int(os.getenv('ADAPTIVE_COOLDOWN_BATCHES','10'))
ADAPTIVE_MIN_BATCH = int(os.getenv('ADAPTIVE_MIN_BATCH','2'))
ADAPTIVE_BATCH_SHRINK_ON_429 = int(os.getenv('ADAPTIVE_BATCH_SHRINK_ON_429','2'))  # maximum shrink steps early

# ----------------- Helpers -----------------

def load_local_settings():
    """Load local.settings.json Values and opportunistically populate env vars.

    We intentionally only set environment variables that are relevant to search
    and Azure OpenAI so that embedding works even if the caller did not export
    AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY. This mirrors what other helper
    scripts do (quick_vector_coverage, etc.) but adds OpenAI credentials.
    """
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
            if isinstance(vals, dict):
                for k, v in vals.items():
                    if k in (
                        'SEARCH_ENDPOINT','SEARCH_KEY',
                        'AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY',
                        'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY',
                        'AZURE_OPENAI_EMBED_MODEL','AZURE_OPENAI_EMBED_DEPLOYMENT',
                        'AZURE_OPENAI_DEPLOYMENT','OPENAI_API_KEY'
                    ) and k not in os.environ and v:
                        # Do not print secrets; just quietly set.
                        os.environ[k] = v
            return vals
        except Exception:
            return {}
    return {}

def get_env_or_local(keys: List[str], local: Dict[str,str]) -> Optional[str]:
    for k in keys:
        v = os.getenv(k) or local.get(k)
        if v:
            return v
    return None

def load_search_credentials():
    local_vals = load_local_settings()
    endpoint = get_env_or_local(['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT'], local_vals)
    key = get_env_or_local(['SEARCH_KEY','AZURE_SEARCH_KEY'], local_vals)
    if not (endpoint and key):
        print('ERROR: Missing search endpoint/key (SEARCH_ENDPOINT/SEARCH_KEY).', file=sys.stderr)
        sys.exit(2)
    return endpoint.rstrip('/'), key

def sha1_bytes(data: bytes) -> str:
    return hashlib.sha1(data).hexdigest()

def find_program_name(text: str, path: Path) -> str:
    m = PROGRAM_ID_RE.search(text)
    if m:
        return m.group(1).upper()
    return path.stem.upper()

def gather_files(roots: List[Path]) -> List[Path]:
    files = []
    for r in roots:
        if not r.exists():
            continue
        for p in r.rglob('*'):
            if p.is_file() and p.suffix.lower() in COBOL_EXTS:
                files.append(p)
    return files

def chunk(seq, n):
    for i in range(0,len(seq),n):
        yield seq[i:i+n]

def get_existing_docs(ep: str, key: str, ids: List[str]) -> Dict[str, Dict[str,Any]]:
    out = {}
    headers={'api-key': key,'Content-Type':'application/json'}
    base=f"{ep}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    for sub in chunk(ids, 12):
        filt = ' or '.join([f"file_id eq '{fid}'" for fid in sub])
        # include has_vectors so we can decide whether to embed unchanged docs lacking vectors
        body={"search":"*","filter": filt, "top": len(sub), "select":"file_id,file_hash,has_vectors"}
        r=requests.post(base, headers=headers, json=body, timeout=30)
        if r.status_code>=300:
            raise SystemExit(f"Lookup failed {r.status_code}: {r.text[:300]}")
        for v in r.json().get('value',[]):
            out[v['file_id']] = v
    return out

def compress_embedding(vec: List[float]) -> List[float]:
    if len(vec) == EXPECTED_DIM:
        return vec
    if len(vec) == EXPECTED_DIM * 2:  # simple average pairwise compression
        return [(vec[i]+vec[i+1])/2.0 for i in range(0,len(vec),2)]
    raise RuntimeError(f"Unexpected embedding dimension {len(vec)} (expected {EXPECTED_DIM} or {EXPECTED_DIM*2})")

def embed_batch(texts: List[str], model: str, delay: float):
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key = os.getenv('AZURE_OPENAI_KEY')
    if not (azure_ep and azure_key):
        raise RuntimeError('Missing Azure OpenAI embedding credentials (AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY).')
    url = f"{azure_ep.rstrip('/')}/openai/deployments/{model}/embeddings?api-version=2024-02-15-preview"
    payload={'input': texts}
    for attempt in range(DEFAULT_EMBED_MAX_RETRIES):
        r=requests.post(url, headers={'api-key': azure_key,'Content-Type':'application/json'}, json=payload, timeout=120)
        if r.status_code < 300:
            data = r.json().get('data',[])
            if len(data) != len(texts):
                raise RuntimeError(f"Embedding batch mismatch returned={len(data)} expected={len(texts)}")
            out=[]
            for obj in data:
                emb = compress_embedding(obj['embedding'])
                out.append(emb)
            return out
        if r.status_code in (429,500,502,503,504):
            sleep_time = delay * (BACKOFF_BASE ** attempt) + random.uniform(0,BACKOFF_JITTER)
            print(f"[embed] retry {attempt+1}/{DEFAULT_EMBED_MAX_RETRIES} status={r.status_code} sleep={sleep_time:.2f}s")
            time.sleep(sleep_time)
            continue
        raise RuntimeError(f"Embedding error {r.status_code}: {r.text[:300]}")
    raise RuntimeError(f"Embedding failed after {DEFAULT_EMBED_MAX_RETRIES} retries (last status {r.status_code})")

def batch_upload(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs: return
    url=f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=payload, timeout=120)
    if r.status_code>=300:
        raise SystemExit(f"Upload failed {r.status_code}: {r.text[:400]}")
    js=r.json()
    failed=[v for v in js.get('value',[]) if not v.get('status')]
    if failed:
        raise SystemExit(f"Some documents failed: {failed[:3]}")

# ----------------- Progress Utilities -----------------

def fmt_eta(start_ts: float, done: int, total: int) -> str:
    if done == 0: return 'ETA --:--'
    rate = done / (time.time() - start_ts)
    remaining = (total - done) / rate if rate>0 else 0
    m, s = divmod(int(remaining), 60)
    return f"ETA {m:02d}:{s:02d} rate {rate:.2f}/s"

def progress(prefix: str, done: int, total: int, extra: str = ''):
    pct = (done/total*100) if total else 0
    bar_len = 24
    filled = int(bar_len * pct/100)
    bar = '#' * filled + '-'*(bar_len-filled)
    print(f"{prefix} [{bar}] {done}/{total} {pct:5.1f}% {extra}")

# ----------------- Main -----------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', action='append', help='Root folder with COBOL sources (repeatable)')
    ap.add_argument('--batch', type=int, default=32)
    ap.add_argument('--embed', action='store_true')
    ap.add_argument('--embed-batch-size', type=int, default=DEFAULT_EMBED_BATCH)
    ap.add_argument('--embed-delay-seconds', type=float, default=DEFAULT_EMBED_DELAY)
    ap.add_argument('--max-embed-chars', type=int, default=24000)
    ap.add_argument('--max-embed-tokens', type=int, default=8000, help='Hard cap on tokens per document embedding (approx if tiktoken missing).')
    ap.add_argument('--no-tokenizer', action='store_true', help='Disable tiktoken even if installed.')
    ap.add_argument('--adaptive', action='store_true', help='Enable adaptive throttling + batch size adjustment + resume state file.')
    ap.add_argument('--initial-embed-delay', type=float, default=ADAPTIVE_DEFAULT_INITIAL_DELAY)
    ap.add_argument('--max-embed-delay', type=float, default=ADAPTIVE_DEFAULT_MAX_DELAY)
    ap.add_argument('--throttle-multiplier', type=float, default=ADAPTIVE_THROTTLE_MULT)
    ap.add_argument('--recovery-divisor', type=float, default=ADAPTIVE_RECOVERY_DIV)
    ap.add_argument('--cooldown-batches', type=int, default=ADAPTIVE_COOLDOWN_BATCHES, help='Clean batches without 429 before attempting delay reduction')
    ap.add_argument('--resume-state-file', default='.file_ingest_state.json')
    ap.add_argument('--min-embed-batch-size', type=int, default=ADAPTIVE_MIN_BATCH)
    ap.add_argument('--max-batches', type=int, help='Stop after this many embedding batches (for phased runs)')
    ap.add_argument('--start-after', help='Skip documents whose file_id is <= this hash/id (for manual resume)')
    ap.add_argument('--ignore-resume', action='store_true', help='Ignore any existing resume state file even if present.')
    ap.add_argument('--force', action='store_true')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--sample', type=int, help='Only process first N files after sorting')
    args = ap.parse_args()

    start = time.time()
    endpoint, key = load_search_credentials()

    roots: List[Path] = []
    if args.root: roots = [Path(r) for r in args.root]
    else:
        env_roots = os.getenv('COBOL_SRC_ROOTS','')
        for part in re.split(r'[;,]', env_roots):
            part = part.strip()
            if part: roots.append(Path(part))
    if not roots:
        print('ERROR: No roots specified (--root or COBOL_SRC_ROOTS).', file=sys.stderr)
        sys.exit(2)

    all_files = gather_files(roots)
    all_files.sort()
    if args.sample:
        all_files = all_files[:args.sample]
    if not all_files:
        print('No COBOL files found.')
        return
    print(f"Discovered {len(all_files)} COBOL source files.")

    # Build metadata
    meta = []
    for p in all_files:
        data = p.read_bytes()
        h = sha1_bytes(data)
        try:
            text = data.decode('utf-8','replace')
        except Exception:
            text = data.decode('latin-1','replace')
        prog = find_program_name(text, p)
        lines = text.count('\n')+1
        fid = h  # hash-based id
        meta.append({'path': str(p), 'file_id': fid, 'hash': h, 'program': prog, 'text': text, 'lines': lines})
    print(f"Prepared metadata for {len(meta)} files.")

    existing = get_existing_docs(endpoint, key, [m['file_id'] for m in meta])
    to_upload = []
    embed_targets = []  # (index in to_upload, text)

    skip_mode = False
    if args.start_after:
        # We'll skip until we pass this file_id
        skip_mode = True
    # Prepare tokenizer (optional)
    enc = None
    if args.embed and _HAS_TIKTOKEN and (not args.no_tokenizer):
        try:
            # Use cl100k_base for text-embedding-3-large compatibility
            enc = tiktoken.get_encoding('cl100k_base')
        except Exception:
            enc = None

    for m in meta:
        if skip_mode:
            if m['file_id'] == args.start_after:
                skip_mode = False
            else:
                continue
        ex = existing.get(m['file_id'])
        if (not args.force) and ex and ex.get('file_hash') == m['hash']:
            # Skip only if we are NOT embedding OR we are embedding and vectors already present
            if (not args.embed) or (args.embed and str(ex.get('has_vectors')).lower() in ('true','1','yes')):
                continue
        doc = {
            'file_id': m['file_id'],
            'program_name': m['program'],
            'path': m['path'],
            'length_lines': m['lines'],
            'file_hash': m['hash'],
            'ingest_timestamp': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
            'has_vectors': False,
            'full_text': m['text']
        }
        if args.embed:
            trunc = m['text'][:args.max_embed_chars]
            # Token-based safety truncation
            if enc is not None:
                toks = enc.encode(trunc)
                if len(toks) > args.max_embed_tokens:
                    toks = toks[:args.max_embed_tokens]
                    trunc = enc.decode(toks)
                    # Removed embed_truncated flag (field not in index schema)
            else:
                # Fallback approximate tokens: rough 4 chars per token heuristic
                approx_tokens = len(trunc) / 4.0
                if approx_tokens > args.max_embed_tokens:
                    keep_chars = int(args.max_embed_tokens * 4)
                    trunc = trunc[:keep_chars]
                    # Removed embed_truncated flag (field not in index schema)
            embed_targets.append( (len(to_upload), trunc) )
        to_upload.append(doc)

    print(f"Changed/new files: {len(to_upload)} (of {len(meta)}) | Embedding: {'yes' if args.embed else 'no'}")
    if args.dry_run:
        for d in to_upload[:5]:
            print('  sample:', d['path'], 'lines=', d['length_lines'])
        return

    # Embedding phase
    if args.embed and embed_targets:
        # Resume state load
        state = {}
        if args.adaptive and (not args.ignore_resume) and os.path.exists(args.resume_state_file):
            try:
                state = json.load(open(args.resume_state_file,'r',encoding='utf-8'))
            except Exception:
                state = {}
        processed_docs = state.get('processed_docs',0) if state else 0
        embed_targets = embed_targets[processed_docs:] if processed_docs else embed_targets
        if processed_docs:
            print(f"Resuming embedding after {processed_docs} docs (state file {args.resume_state_file})")

        current_delay = args.initial_embed_delay if args.adaptive else args.embed_delay_seconds
        clean_streak = 0
        shrink_steps = 0
        batch_size = args.embed_batch_size
        total_batches = math.ceil(len(embed_targets)/batch_size) if batch_size else 0
        print(f"Embedding {len(embed_targets)} docs in ~{total_batches} batches (initial size {batch_size})")
        batch_idx = 0
        e_start = time.time()
        try:
            while embed_targets:
                if args.max_batches is not None and batch_idx >= args.max_batches:
                    print(f"Reached --max-batches {args.max_batches}, stopping early.")
                    break
                batch_idx += 1
                current = embed_targets[:batch_size]
                embed_targets = embed_targets[batch_size:]
                texts = [t for _,t in current]
                attempt_vectors=None
                try:
                    attempt_vectors = embed_batch(texts, EMBED_MODEL, current_delay)
                    clean_streak += 1
                except RuntimeError as ex:
                    msg=str(ex)
                    if '429' in msg and args.adaptive:
                        clean_streak = 0
                        current_delay = min(current_delay * args.throttle_multiplier, args.max_embed_delay)
                        # Early phase: shrink batch size a few times if still large
                        if shrink_steps < ADAPTIVE_BATCH_SHRINK_ON_429 and batch_size > args.min_embed_batch_size*2:
                            batch_size = max(args.min_embed_batch_size, batch_size//2)
                            shrink_steps += 1
                            print(f"[adaptive] 429: shrinking batch size -> {batch_size}")
                        print(f"[adaptive] 429: increasing delay -> {current_delay:.2f}s (batch {batch_idx})")
                        # Requeue current docs by restoring them at front
                        embed_targets = current + embed_targets
                        continue
                    else:
                        print(f"[embed][batch {batch_idx}] FATAL: {ex}")
                        raise
                if attempt_vectors is None:
                    continue
                for (doc_index,_), vec in zip(current, attempt_vectors):
                    to_upload[doc_index][VECTOR_FIELD] = vec
                    to_upload[doc_index]['has_vectors'] = True
                processed_docs += len(current)
                # Adaptive recovery: after cooldown batches without 429 reduce delay slightly
                if args.adaptive and clean_streak and (clean_streak % args.cooldown_batches == 0) and current_delay > args.initial_embed_delay:
                    new_delay = max(args.initial_embed_delay, current_delay/args.recovery_divisor)
                    if new_delay != current_delay:
                        print(f"[adaptive] recovery: reducing delay {current_delay:.2f}s -> {new_delay:.2f}s after {clean_streak} clean batches")
                        current_delay = new_delay
                if args.adaptive:
                    # Persist state
                    try:
                        json.dump({'processed_docs': processed_docs, 'batch_size': batch_size, 'delay': current_delay, 'last_batch': batch_idx}, open(args.resume_state_file,'w',encoding='utf-8'))
                    except Exception:
                        pass
                progress('Embed', batch_idx, total_batches or 1, f"delay={current_delay:.2f}s bs={batch_size} {fmt_eta(e_start, batch_idx, total_batches or 1)}")
        except KeyboardInterrupt:
            print('\nInterrupted by user, state saved. You can resume with --adaptive using the same parameters.')
            return

    # Upload phase
    total_docs = len(to_upload)
    if total_docs == 0:
        print('Nothing to upload (all files unchanged).')
        return
    print(f"Uploading {total_docs} documents in batches of {args.batch} ...")
    u_start = time.time()
    uploaded = 0
    batch_num = 0
    for i in range(0, total_docs, args.batch):
        batch_num += 1
        batch_docs = to_upload[i:i+args.batch]
        batch_upload(endpoint, key, batch_docs)
        uploaded += len(batch_docs)
        progress('Upload', uploaded, total_docs, fmt_eta(u_start, uploaded, total_docs))
    elapsed = time.time() - start
    print(f"Completed. Uploaded {uploaded} docs. Embeddings={'yes' if args.embed else 'no'}. Elapsed {elapsed:.1f}s")
    if args.embed and args.adaptive:
        print("Resume state file:", args.resume_state_file)

if __name__ == '__main__':
    main()
