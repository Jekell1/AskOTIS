"""Ingest COPY statement usage occurrences into new_cobol_copybook_usage.

One document per COPY statement with contextual lines and optional vector embedding.

Features:
  * Streaming + buffer flush with resume state
  * Paragraph + section detection (heuristic)
  * Embedding of contextualized COPY occurrence
  * Deterministic usage_id
  * Dry-run preview

Example:
  python ingest_copybook_usage.py --roots . --limit 50 --dry-run
  python ingest_copybook_usage.py --roots cobol_src --buffer-size 400 --batch-size 200
"""
from __future__ import annotations
import os, re, json, sys, argparse, hashlib, time, random, requests
from typing import List, Dict, Any, Tuple
from embedding_utils import batch_embed

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_copybook_usage'
VECTOR_DIM = 3072
DEFAULT_BUFFER = 300
STATE_VERSION = 1

# Expanded extension list to include uppercase variants.
COBOL_EXTS = {'.cbl', '.cob', '.cobol', '.CBL', '.COB'}

# COPY detection improvements:
#  1. Accept quoted names (single/double quotes) and paths with slashes.
#  2. Accept unquoted token (letters/digits/_- and path separators) ending before period/comma/space.
#  3. Capture optional REPLACING clause (we'll store raw line for now, but flag presence).
#  4. Avoid false positives such as matching 'SECTION' after COPY noise; we'll post-filter.
# We allow up to 12 leading columns for sequence numbers or indicator area.
RE_COPY = re.compile(
    r'''(?ix)               # ignore case, verbose
        ^.{0,12}?           # sequence / indicator columns
        \bCOPY\s+          # COPY keyword
        (?:
           ["']([^"']+)["']   # 1: quoted copybook/path
           |                    # or
           ([A-Z0-9_/-]+)       # 2: unquoted token (may include / for library path)
        )
    ''')
RE_COPY_REPLACING = re.compile(r'\bCOPY\b.*\bREPLACING\b', re.IGNORECASE)
RE_PARAGRAPH = re.compile(r'^[0-9 ]{0,6}([A-Z0-9-]+)\.$')
RE_WS = re.compile(r'^\s*WORKING-STORAGE\s+SECTION', re.IGNORECASE)
RE_LINK = re.compile(r'^\s*LINKAGE\s+SECTION', re.IGNORECASE)
RE_LOCAL = re.compile(r'^\s*LOCAL-STORAGE\s+SECTION', re.IGNORECASE)

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def walk_sources(roots: List[str]) -> List[str]:
    paths: List[str] = []
    for root in roots:
        for dp,_,files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1]
                if ext in COBOL_EXTS or ext.lower() in {e.lower() for e in COBOL_EXTS}:
                    paths.append(os.path.join(dp,f))
    return sorted(paths)

def read_lines(path: str) -> List[str]:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            return f.read().splitlines()
    except Exception:
        return []

def program_id_from_path(path: str) -> str:
    base = os.path.basename(path)
    name,_ = os.path.splitext(base)
    return name.upper()

def usage_id(program_id: str, copybook: str, line_start: int, line_end: int) -> str:
    raw = f"{program_id}|{copybook}|{line_start}|{line_end}".encode('utf-8')
    return hashlib.sha1(raw).hexdigest()[:40]

def normalize_copybook(raw: str) -> str:
    # Strip directory prefixes like LIBGB/, LIBLP/, etc., and extension .CPY/.cpy/.CBL if present
    name = raw.strip()
    # Remove surrounding quotes if any linger
    if (name.startswith('"') and name.endswith('"')) or (name.startswith("'") and name.endswith("'")):
        name = name[1:-1]
    # Split path components, take last
    if '/' in name:
        name = name.split('/')[-1]
    # Remove extension
    for ext in ('.CPY', '.cpy', '.CBL', '.cbl'):
        if name.endswith(ext):
            name = name[:-len(ext)]
    return name.upper()

def extract_usages(path: str, lines: List[str], debug_noisy: bool=False) -> List[Dict[str,Any]]:
    program = program_id_from_path(path)
    usages: List[Dict[str,Any]] = []
    current_paragraph = None
    section = None
    inclusion_order = 0
    for idx, line in enumerate(lines):
        if debug_noisy and 'COPY' in line.upper():
            print(f"[NOISY] {path}:{idx+1}: {line.rstrip()}")
        mpara = RE_PARAGRAPH.match(line)
        if mpara:
            current_paragraph = mpara.group(1).upper()
        if RE_WS.search(line):
            section = 'WORKING-STORAGE'
        elif RE_LINK.search(line):
            section = 'LINKAGE'
        elif RE_LOCAL.search(line):
            section = 'LOCAL-STORAGE'
        stripped = line.lstrip()
        if stripped.startswith('*') or stripped.startswith('*>'):
            continue
        m = RE_COPY.search(line)
        if m:
            raw_copy = (m.group(1) or m.group(2) or '').strip()
            copybook = normalize_copybook(raw_copy)
            # Build canonical copybook_name for filtering: ensure .CPY extension, strip any path
            canonical = raw_copy.upper()
            plain = canonical.split('/')[-1]
            if '.' not in plain:
                plain = plain + '.CPY'
            if '.' not in canonical:
                canonical = canonical + '.CPY'
            # Normalize common alternate extensions to .CPY for consistency (e.g., .CBL include usage but treat as .CPY?)
            # We'll keep actual if already .CPY / .CBL to avoid losing information.
            # Filter obvious false positives
            if copybook in {'SECTION', 'PROGRAM-ID', 'AUTHOR', 'ENVIRONMENT', 'DATA', 'PROCEDURE'}:
                continue
            replacing = bool(RE_COPY_REPLACING.search(line))
            line_start = idx + 1
            line_end = line_start
            inclusion_order += 1
            lo = max(0, idx-8)
            hi = min(len(lines), idx+9)
            ctx_lines = lines[lo:hi]
            snippet = '\n'.join(ctx_lines)
            if len(snippet) > 2000:
                snippet = snippet[:2000] + '...'
            doc = {
                'usage_id': usage_id(program, copybook, line_start, line_end),
                'program_id': program,
                'program_name': program,
                # Preserve original raw copy token (including extension if present) for filtering (e.g., SCREEN.CPY)
                'copybook_name': canonical,  # may include path
                'copybook_name_plain': plain,  # no path
                'normalized_copybook_name': copybook,
                'section': section or 'UNKNOWN',
                'paragraph_name': current_paragraph,
                'line_start': line_start,
                'line_end': line_end,
                'inclusion_order': inclusion_order,
                'file_path': path,
                'line_number': line_start,
                'raw_copy_line': line.strip(),
                'context_snippet': snippet,
                'expansion_present': False,
                'has_replacing_clause': replacing,
                'program_classification': None,
                'ingested_at': None,
                'has_vector': False
            }
            usages.append(doc)
    return usages

def embed_and_upload(ep: str, key: str, docs: List[Dict[str,Any]], batch_size: int, embed_batch_size: int):
    if not docs:
        return 0
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    texts = [f"COPY {d['copybook_name']} in {d['program_id']} context: {d['context_snippet'][:800]}" for d in docs]
    attempt = 0
    while True:
        try:
            # Force target_dim to VECTOR_DIM so vectors align with index schema (3072)
            vecs = batch_embed(texts, target_dim=VECTOR_DIM, batch_size=embed_batch_size)
            break
        except Exception as e:
            attempt += 1
            if attempt > 5:
                print(f"[FATAL] Embedding retries exceeded: {e}")
                raise
            back = min(60, 2**attempt + random.uniform(0,1))
            print(f"[WARN] Embed error {e} retry in {back:.1f}s")
            time.sleep(back)
    for d, v in zip(docs, vecs):
        if len(v) != VECTOR_DIM:
            if len(v) > VECTOR_DIM:
                v = v[:VECTOR_DIM]
            else:
                v = v + [0.0]*(VECTOR_DIM-len(v))
        d['context_vector'] = v
        d['has_vector'] = True
        d['ingested_at'] = ts
    uploaded = 0
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type':'application/json'}
    for i in range(0, len(docs), batch_size):
        chunk = docs[i:i+batch_size]
        payload = {'value':[{'@search.action':'mergeOrUpload', **c} for c in chunk]}
        r = requests.post(url, headers=headers, json=payload, timeout=60)
        if r.status_code not in (200,201):
            print(f"[ERROR] Upload failure {r.status_code}: {r.text[:300]}")
        else:
            uploaded += len(chunk)
    return uploaded

def load_state(path: str):
    if not os.path.exists(path):
        return {}
    try:
        data = json.load(open(path,'r',encoding='utf-8'))
        if data.get('version') != STATE_VERSION:
            return {}
        return data
    except Exception:
        return {}

def save_state(path: str, state: Dict[str,Any]):
    tmp = path + '.tmp'
    state['version'] = STATE_VERSION
    with open(tmp,'w',encoding='utf-8') as f:
        json.dump(state,f)
    os.replace(tmp,path)

def main():
    ap = argparse.ArgumentParser(description='Ingest COPY usage occurrences')
    ap.add_argument('--roots', nargs='+', default=['.'])
    ap.add_argument('--limit', type=int)
    ap.add_argument('--buffer-size', type=int, default=DEFAULT_BUFFER)
    ap.add_argument('--batch-size', type=int, default=200)
    ap.add_argument('--embed-batch-size', type=int, default=32)
    ap.add_argument('--resume', action='store_true')
    ap.add_argument('--state-file', default='copybook_usage_state.json')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--no-embed', action='store_true', help='Skip embedding for speed testing')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--debug-noisy', action='store_true', help='Print every line containing COPY for diagnostics')
    args = ap.parse_args()

    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    files = walk_sources(args.roots)
    if args.limit:
        files = files[:args.limit]

    state = load_state(args.state_file) if args.resume else {}
    start_file_index = state.get('file_index', 0)
    total_uploaded = state.get('uploaded', 0)
    total_extracted = state.get('extracted', 0)
    buffer: List[Dict[str,Any]] = []
    t0 = time.time()
    last_flush = t0

    def flush(reason: str):
        nonlocal buffer, total_uploaded, last_flush
        if not buffer:
            return
        docs = buffer
        buffer = []
        if args.dry_run:
            # In dry-run we don't embed or upload; just report.
            print(f"[FLUSH] (dry-run) reason={reason} pending_docs={len(docs)} total_extracted={total_extracted}")
            return
        uploaded = 0
        if args.no_embed:
            # Skip embedding call; attach zero vector and upload directly.
            ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
            for d in docs:
                d['context_vector'] = [0.0]*VECTOR_DIM
                d['has_vector'] = False
                d['ingested_at'] = ts
            # Manual upload without embedding pass
            url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
            headers = {'api-key': key, 'Content-Type':'application/json'}
            for i in range(0, len(docs), args.batch_size):
                chunk = docs[i:i+args.batch_size]
                payload = {'value':[{'@search.action':'mergeOrUpload', **c} for c in chunk]}
                r = requests.post(url, headers=headers, json=payload, timeout=60)
                if r.status_code not in (200,201):
                    print(f"[ERROR] Upload failure {r.status_code}: {r.text[:300]}")
                else:
                    uploaded += len(chunk)
        else:
            uploaded = embed_and_upload(ep, key, docs, args.batch_size, args.embed_batch_size)
        total_uploaded += uploaded
        save_state(args.state_file, {
            'file_index': current_file_idx,
            'uploaded': total_uploaded,
            'extracted': total_extracted,
            'version': STATE_VERSION
        })
        now = time.time()
        rate = total_uploaded / max(1, now - t0)
        print(f"[FLUSH] reason={reason} +{uploaded} total_uploaded={total_uploaded} extracted={total_extracted} rate={rate:.1f}/s elapsed={now-t0:.1f}s")
        last_flush = now

    try:
        for current_file_idx, path in enumerate(files):
            if current_file_idx < start_file_index:
                continue
            lines = read_lines(path)
            if not lines:
                continue
            usages = extract_usages(path, lines, debug_noisy=args.debug_noisy)
            if usages:
                buffer.extend(usages)
                total_extracted += len(usages)
            if (current_file_idx+1) % 50 == 0:
                print(f"Processed {current_file_idx+1} files extracted={total_extracted} uploaded={total_uploaded} buffer={len(buffer)}")
            if len(buffer) >= args.buffer_size:
                flush('buffer-full')
            if time.time() - last_flush > 120:
                flush('time-interval')
    except KeyboardInterrupt:
        print('\n[INTERRUPT] Flushing before exit...')
        flush('interrupt')
        print(f"[INTERRUPT] State saved uploaded={total_uploaded} extracted={total_extracted}")
        return

    if args.dry_run:
        # Final preview of first N docs (buffer already contains unflushed docs only)
        preview = buffer[:min(20, len(buffer))]
        print(f"[DRY-RUN] Files scanned={len(files)} extracted={total_extracted} (not uploaded) preview_count={len(preview)}")
        for d in preview:
            print(json.dumps(d, indent=2)[:800])
        return

    flush('final')
    print(f"Ingestion complete uploaded={total_uploaded} extracted={total_extracted}")

if __name__ == '__main__':
    main()
