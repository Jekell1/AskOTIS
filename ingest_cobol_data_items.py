"""Extract and ingest COBOL data division items into new_cobol_data_items.

Features:
    * Walk COBOL source roots
    * Parse DATA DIVISION level items (01-49 default)
    * Build hierarchical path and parent_item
    * Derive length_bytes from PIC (best-effort)
    * Embed semantic description text into 3072-dim vector
    * Streaming buffered upload with retry & exponential backoff
    * Resume support via state file (--resume / --state-file)
    * Max-items safety cap for partial test runs
    * Dry-run mode to inspect sample

Usage:
    python ingest_cobol_data_items.py --roots ./cobol_src --dry-run --limit 40
    python ingest_cobol_data_items.py --roots ./cobol_src --buffer-size 2000 --batch-size 500 --embed-batch-size 64
    python ingest_cobol_data_items.py --roots ./cobol_src --resume --state-file data_items_state.json

Env / local.settings.json values:
    AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
    AZURE_SEARCH_KEY / SEARCH_KEY
    (embedding provider keys as used by embedding_utils)
"""
from __future__ import annotations
import os, re, sys, json, argparse, time, hashlib, requests, math, random
from typing import List, Dict, Any, Optional
from embedding_utils import batch_embed, provider_info

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_data_items'
VECTOR_DIM = 3072

LEVEL_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+REDEFINES\s+([A-Z0-9-]+))?', re.IGNORECASE)
PIC_RE = re.compile(r'PIC\s+([^\s.]+)', re.IGNORECASE)
# VALUE_RE: Match VALUE "text" or VALUE 'text' or VALUE SPACES/ZEROS (unquoted literals)
# For quoted strings, capture everything between quotes (including spaces)
# For unquoted, capture single word like SPACES, ZEROS, etc.
VALUE_RE = re.compile(r"VALUE\s+(?:IS\s+)?(?:(['\"])([^'\"]*)\1|([A-Z0-9-]+))", re.IGNORECASE)
USAGE_RE = re.compile(r'USAGE\s+IS\s+([A-Z0-9-]+)', re.IGNORECASE)
USAGE_SHORT_RE = re.compile(r'USAGE\s+([A-Z0-9-]+)', re.IGNORECASE)
OCCURS_RE = re.compile(r'OCCURS\s+([0-9]+)\s+TIMES', re.IGNORECASE)
COMP3_RE = re.compile(r'COMP-?3', re.IGNORECASE)
DATA_DIV_RE = re.compile(r'^\s*DATA\s+DIVISION', re.IGNORECASE)
PROC_DIV_RE = re.compile(r'^\s*PROCEDURE\s+DIVISION', re.IGNORECASE)

COBOL_EXTS = {'.cbl','.cob','.cpy'}

PAGE = 500
DEFAULT_BUFFER_SIZE = 2000
STATE_VERSION = 1


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def normalize_program_id(path: str) -> str:
    base = os.path.basename(path)
    name, _ = os.path.splitext(base)
    return name.upper()


def file_id_from_path(path: str) -> str:
    h = hashlib.sha1(path.encode('utf-8')).hexdigest()[:16]
    return h


def read_file(path: str) -> str:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            return f.read()
    except Exception as e:
        return ''


def estimate_length_bytes(pic: Optional[str]) -> Optional[int]:
    if not pic:
        return None
    p = pic.upper()
    # Basic patterns: X(n), 9(n), 9V9(n), S9(n) COMP-3
    # Extract number after ( ... ) if exists
    m = re.search(r'X\((\d+)\)', p)
    if m:
        return int(m.group(1))
    m = re.search(r'9\((\d+)\)', p)
    if m:
        n = int(m.group(1))
        if COMP3_RE.search(p):
            return (n + 2) // 2  # packed decimal approx
        return n
    # Simple single char forms X or 9 repeated implicitly -> treat as 1 byte
    if p.startswith('X') or p.startswith('9'):
        return 1
    return None


def parse_items(source_path: str, content: str, levels: List[int]) -> List[Dict[str, Any]]:
    """Parse DATA DIVISION level items from COBOL source.

    We treat every matched level line whose level number is in the supplied
    levels list as an indexed item (group or elementary). Non-indexed levels
    are still tracked for hierarchy building so paths/parent_item are correct.
    """
    lines = content.splitlines()
    in_data = False
    items: List[Dict[str, Any]] = []
    stack: List[Dict[str, Any]] = []  # hierarchy stack

    for idx, raw in enumerate(lines, start=1):
        if DATA_DIV_RE.search(raw):
            in_data = True
        if PROC_DIV_RE.search(raw):
            break  # stop once PROCEDURE DIVISION reached
        if not in_data:
            continue

        m = LEVEL_RE.match(raw.expandtabs())
        if not m:
            continue

        level = int(m.group(1))
        name = m.group(2).upper()
        redef = m.group(3).upper() if m.group(3) else None

        # prune stack for current level (applies to all items)
        while stack and stack[-1]['level'] >= level:
            stack.pop()

        if level not in levels:
            # Track hierarchy but don't emit searchable document
            stack.append({'level': level, 'name': name, 'path': name if not stack else f"{stack[-1]['path']}.{name}"})
            continue

        parent_path = stack[-1]['path'] if stack else None
        hierarchical_path = f"{parent_path}.{name}" if parent_path else name

        # Attribute extraction
        pic_m = PIC_RE.search(raw)
        pic = pic_m.group(1).rstrip('.') if pic_m else None
        
        # VALUE clause extraction: check current line first, then next line for multi-line patterns
        # New VALUE_RE has 3 groups: (1)quote char, (2)quoted text, (3)unquoted literal
        value_m = VALUE_RE.search(raw)
        if value_m:
            value_clause = value_m.group(2) if value_m.group(2) is not None else value_m.group(3)
        else:
            value_clause = None
        
        # If no VALUE on current line and there's a next line, check it for VALUE clause
        if not value_clause and idx < len(lines):
            next_line = lines[idx].strip()  # idx is 1-based, lines is 0-based, so lines[idx] is next line
            if next_line and not LEVEL_RE.match(next_line.expandtabs()):  # not a new level definition
                value_m_next = VALUE_RE.search(next_line)
                if value_m_next:
                    value_clause = value_m_next.group(2) if value_m_next.group(2) is not None else value_m_next.group(3)
        usage_m = USAGE_RE.search(raw) or USAGE_SHORT_RE.search(raw)
        usage = usage_m.group(1).upper() if usage_m else None
        occurs_m = OCCURS_RE.search(raw)
        occurs = f"OCCURS {occurs_m.group(1)} TIMES" if occurs_m else None
        length_bytes = estimate_length_bytes(pic)
        is_group = pic is None

        raw_id_basis = f"{source_path}:{idx}:{hierarchical_path}"
        item_id = hashlib.sha1(raw_id_basis.encode('utf-8')).hexdigest()[:32]

        items.append({
            'item_id': item_id,
            'program_id': normalize_program_id(path_from_source(source_path)),  # fixed in second pass
            'file_id': '',  # filled later
            'file_path': path_from_source(source_path),
            'level': level,
            'item_name': name,
            'pic': pic,
            'occurs': occurs,
            'redefines': redef,
            'usage': usage,
            'full_clause': raw.strip(),
            'value_clause': value_clause,
            'parent_item': stack[-1]['name'] if stack else None,
            'path': hierarchical_path,
            'line_start': idx,
            'line_end': idx,
            'length_bytes': length_bytes if length_bytes is not None else -1,
            'is_group': is_group,
            'has_vector': False,
            'ingested_at': None
        })

        # push current item onto stack
        stack.append({'level': level, 'name': name, 'path': hierarchical_path})

    # Second pass: normalize program/file identifiers
    for it in items:
        it['file_path'] = source_path
        it['program_id'] = normalize_program_id(source_path)
        it['file_id'] = file_id_from_path(source_path)
    return items


def path_from_source(p: str) -> str:
    return p.replace('\\','/')


def embed_and_upload(ep: str, key: str, docs: List[Dict[str,Any]], batch_size: int, embed_batch_size: int, max_retries: int = 5):
    if not docs:
        return 0
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    for d in docs:
        d['ingested_at'] = ts

    # Embedding with simple retry around batch_embed (provider may already handle some retries)
    texts = [f"{d['item_name']} level {d['level']} {'group' if d['is_group'] else 'element'} {d.get('pic','') or ''} {d.get('occurs','') or ''} {d.get('usage','') or ''} in program {d['program_id']} path {d['path']}" for d in docs]
    attempt = 0
    while True:
        try:
            vecs = batch_embed(texts, target_dim=None, batch_size=embed_batch_size)
            break
        except Exception as e:
            attempt += 1
            if attempt > max_retries:
                print(f"[FATAL] Embedding failed after {max_retries} retries: {e}")
                raise
            back = min(60, 2 ** attempt + random.uniform(0, 1))
            print(f"[WARN] Embedding error: {e} retrying in {back:.1f}s (attempt {attempt}/{max_retries})")
            time.sleep(back)
    for d, v in zip(docs, vecs):
        # Normalize vector length (pad/trim) to avoid 400 errors if provider returns mismatch
        if len(v) != VECTOR_DIM:
            if len(v) > VECTOR_DIM:
                v = v[:VECTOR_DIM]
            else:
                v = v + [0.0] * (VECTOR_DIM - len(v))
        d['vector'] = v
        d['has_vector'] = True

    uploaded = 0
    first_error_logged = False
    for i in range(0, len(docs), batch_size):
        chunk = docs[i:i+batch_size]
        payload = {'value': [{"@search.action": "mergeOrUpload", **c} for c in chunk]}
        url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
        attempt_u = 0
        while True:
            try:
                r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload, timeout=60)
            except Exception as e:
                r = None
                print(f"[WARN] Upload request exception: {e}")
            if r and r.status_code in (200, 201):
                uploaded += len(chunk)
                break
            attempt_u += 1
            # Log detail on first 400
            if r and r.status_code == 400 and not first_error_logged:
                first_error_logged = True
                snippet = r.text[:800] if r.text else 'NO_BODY'
                sample_doc = {k:type(chunk[0].get(k)).__name__ for k in chunk[0].keys() if k != 'vector'}
                print(f"[DIAG] 400 body: {snippet}")
                print(f"[DIAG] Sample doc keys/types (excluding vector): {json.dumps(sample_doc, indent=2)}")
                print(f"[DIAG] Vector length: {len(chunk[0].get('vector', []))} expected {VECTOR_DIM}")
            if attempt_u > max_retries:
                print(f"[ERROR] Upload chunk failed after {max_retries} retries status={getattr(r,'status_code',None)} text={(r.text[:400] if r else 'NO_RESPONSE')} — continuing.")
                break
            back = min(90, 2 ** attempt_u + random.uniform(0, 1))
            print(f"[WARN] Upload failure status={getattr(r,'status_code',None)} retry in {back:.1f}s (attempt {attempt_u}/{max_retries})")
            time.sleep(back)
    return uploaded


def walk_files(roots: List[str]) -> List[str]:
    out: List[str] = []
    for root in roots:
        for dirpath, _, files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_EXTS:
                    out.append(os.path.join(dirpath, f))
    return sorted(out)


def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def load_state(path: str) -> Dict[str, Any]:
    if not os.path.exists(path):
        return {}
    try:
        with open(path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        if data.get('version') != STATE_VERSION:
            return {}
        return data
    except Exception:
        return {}


def save_state(path: str, state: Dict[str, Any]):
    tmp = path + '.tmp'
    state['version'] = STATE_VERSION
    with open(tmp, 'w', encoding='utf-8') as f:
        json.dump(state, f)
    os.replace(tmp, path)


def main():
    ap = argparse.ArgumentParser(description='Ingest COBOL data division items into search index (streaming).')
    ap.add_argument('--roots', nargs='+', default=['./cobol_src'])
    ap.add_argument('--levels', default='01-49', help='Level range like 01-49 or comma list e.g. 01,05,10')
    ap.add_argument('--limit', type=int, help='Limit number of source files scanned.')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--batch-size', type=int, default=500, help='Upload batch size.')
    ap.add_argument('--embed-batch-size', type=int, default=64, help='Embedding provider batch size.')
    ap.add_argument('--buffer-size', type=int, default=DEFAULT_BUFFER_SIZE, help='Number of parsed items to accumulate before embedding/upload.')
    ap.add_argument('--max-items', type=int, help='Stop after indexing this many items (for partial test).')
    ap.add_argument('--resume', action='store_true', help='Resume from state file if present.')
    ap.add_argument('--state-file', default='data_items_state.json')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    args = ap.parse_args()

    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    # parse levels
    level_list: List[int] = []
    if '-' in args.levels:
        a, b = args.levels.split('-', 1)
        try:
            start = int(a); end = int(b)
            level_list = list(range(start, end + 1))
        except ValueError:
            print('[FATAL] Invalid level range', file=sys.stderr); sys.exit(1)
    else:
        for tok in args.levels.split(','):
            tok = tok.strip()
            if tok:
                level_list.append(int(tok))

    files = walk_files(args.roots)
    if args.limit:
        files = files[:args.limit]

    state = load_state(args.state_file) if args.resume else {}
    start_file_index = state.get('file_index', 0)
    total_uploaded = state.get('uploaded', 0)
    total_parsed = state.get('parsed', 0)

    print(f"Scanning {len(files)} files for data items (levels {args.levels}) Provider={provider_info()} resume={bool(state)}")
    buffer: List[Dict[str, Any]] = []
    t0 = time.time()
    last_flush_time = t0
    def flush(reason: str):
        nonlocal buffer, total_uploaded, last_flush_time
        if not buffer:
            return
        uploaded = embed_and_upload(ep, key, buffer, args.batch_size, args.embed_batch_size)
        total_uploaded += uploaded
        buffer_count = len(buffer)
        buffer = []
        save_state(args.state_file, {'file_index': current_file_index, 'uploaded': total_uploaded, 'parsed': total_parsed})
        now = time.time()
        rate = total_uploaded / max(1, (now - t0))
        print(f"[FLUSH] reason={reason} uploaded+={uploaded} total_uploaded={total_uploaded} parsed={total_parsed} rate={rate:.1f} items/s elapsed={now-t0:.1f}s")
        last_flush_time = now

    current_file_index = 0
    try:
        for fi, path in enumerate(files):
            current_file_index = fi
            if fi < start_file_index:
                continue
            content = read_file(path)
            if not content:
                continue
            parsed = parse_items(path, content, level_list)
            if parsed:
                buffer.extend(parsed)
                total_parsed += len(parsed)
            if (fi + 1) % 200 == 0:
                print(f"Processed {fi+1} files... parsed={total_parsed} buffered={len(buffer)} uploaded={total_uploaded}")
            # flush if buffer large
            if len(buffer) >= args.buffer_size:
                flush('buffer-full')
            # flush periodically every ~120s even if buffer not full
            if time.time() - last_flush_time > 120:
                flush('time-interval')
            # stop if max-items reached (counting parsed not yet uploaded + uploaded)
            if args.max_items and (total_uploaded + len(buffer)) >= args.max_items:
                # trim buffer to exactly reach max_items
                overflow = (total_uploaded + len(buffer)) - args.max_items
                if overflow > 0:
                    buffer = buffer[:-overflow]
                flush('max-items')
                print('[INFO] Reached max-items limit; stopping early.')
                break
    except KeyboardInterrupt:
        # Ensure any buffered docs are flushed & state saved
        print('\n[INTERRUPT] Caught KeyboardInterrupt; flushing remaining buffer and saving state...')
        flush('interrupt')
        print(f"[INTERRUPT] Partial ingestion state saved: uploaded={total_uploaded} parsed={total_parsed} file_index={current_file_index}")
        return

    if args.dry_run:
        print(f"[DRY-RUN] Parsed {total_parsed} items. Showing samples:")
        for sample in buffer[:min(20, len(buffer))]:
            print(json.dumps(sample, indent=2)[:500])
        print('Dry-run complete.')
        return

    # final flush
    flush('final')
    print(f"Ingestion complete: uploaded={total_uploaded} parsed={total_parsed}")


if __name__ == '__main__':
    main()
