"""Ingest copybook metadata into new_cobol_copybook_meta index.

One document per .cpy file capturing structural statistics and a semantic summary.

Features:
  * Walk roots for .cpy files
  * Derive copybook_name, counts of significant constructs
  * Lightweight data item scan (level 01) to approximate structure richness
  * Produce natural language summary -> embed (3072-dim) -> store as summary_vector
  * Resume support via state file
  * Dry-run mode to inspect sample documents

Usage examples:
  python ingest_copybook_meta.py --roots ./cobol_src --dry-run --limit 5
  python ingest_copybook_meta.py --roots . --buffer-size 200 --batch-size 200
  python ingest_copybook_meta.py --roots . --resume --state-file copybook_meta_state.json
"""
from __future__ import annotations
import os, sys, re, json, argparse, time, hashlib, random, requests
from typing import List, Dict, Any
from embedding_utils import batch_embed, provider_info

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_copybook_meta'
VECTOR_DIM = 3072
COBOL_COPY_EXTS = {'.cpy'}
DEFAULT_BUFFER = 200
STATE_VERSION = 1

RE_COMMENT = re.compile(r'^(\s{0,7}\*|\*>).*')
RE_PROC_DIV = re.compile(r'^\s*PROCEDURE\s+DIVISION', re.IGNORECASE)
RE_DATA_DIV = re.compile(r'^\s*DATA\s+DIVISION', re.IGNORECASE)
RE_LEVEL01 = re.compile(r'^\s*01\s+[A-Z0-9-]+', re.IGNORECASE)
RE_REDEFINES = re.compile(r'REDEFINES', re.IGNORECASE)
RE_OCCURS = re.compile(r'OCCURS\s+\d+\s+TIMES', re.IGNORECASE)
RE_WORKING_STORAGE = re.compile(r'^\s*WORKING-STORAGE\s+SECTION', re.IGNORECASE)
RE_LINKAGE = re.compile(r'^\s*LINKAGE\s+SECTION', re.IGNORECASE)

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def read_file(path: str) -> str:
    try:
        with open(path,'r',encoding='utf-8',errors='replace') as f:
            return f.read()
    except Exception:
        return ''

def normalize_name(path: str) -> str:
    base = os.path.basename(path)
    name,_ = os.path.splitext(base)
    return name.upper()

def copybook_id(path: str) -> str:
    return hashlib.sha1(path.encode('utf-8')).hexdigest()[:32]

def scan_copybook(path: str, content: str) -> Dict[str, Any]:
    lines = content.splitlines()
    total = len(lines)
    non_comment = 0
    program_like = False
    data_div = False
    working_storage_items = 0
    linkage_items = 0
    redefines_count = 0
    occurs_count = 0
    level01_count = 0
    approximate_item_count = 0
    in_working = False
    in_linkage = False
    for ln in lines:
        if not RE_COMMENT.match(ln) and ln.strip():
            non_comment += 1
        if RE_PROC_DIV.search(ln):
            program_like = True
        if RE_DATA_DIV.search(ln):
            data_div = True
        if RE_WORKING_STORAGE.search(ln):
            in_working = True; in_linkage = False
        if RE_LINKAGE.search(ln):
            in_linkage = True; in_working = False
        if RE_LEVEL01.search(ln):
            level01_count += 1
            approximate_item_count += 1
            if in_working:
                working_storage_items += 1
            if in_linkage:
                linkage_items += 1
        if RE_REDEFINES.search(ln):
            redefines_count += 1
        if RE_OCCURS.search(ln):
            occurs_count += 1
    summary_parts = []
    summary_parts.append(f"Copybook {normalize_name(path)}: {level01_count} top-level items")
    if working_storage_items:
        summary_parts.append(f"{working_storage_items} WS items")
    if linkage_items:
        summary_parts.append(f"{linkage_items} linkage items")
    if redefines_count:
        summary_parts.append(f"{redefines_count} REDEFINES")
    if occurs_count:
        summary_parts.append(f"{occurs_count} OCCURS clauses")
    if program_like:
        summary_parts.append("contains PROCEDURE logic")
    if not summary_parts:
        summary_parts.append("structural copybook")
    summary = "; ".join(summary_parts)
    return {
        'copybook_id': copybook_id(path),
        'copybook_name': normalize_name(path),
        'file_path': path.replace('\\','/'),
        'program_like': program_like,
        'lines_total': total,
        'lines_non_comment': non_comment,
        'data_division_present': data_div,
        'working_storage_items': working_storage_items,
        'linkage_items': linkage_items,
        'redefines_count': redefines_count,
        'occurs_count': occurs_count,
        'level01_count': level01_count,
        'approximate_item_count': approximate_item_count,
        'summary': summary,
        'has_vector': False,
        'ingested_at': None
    }

def walk_copybooks(roots: List[str]) -> List[str]:
    out: List[str] = []
    for root in roots:
        for dirpath,_,files in os.walk(root):
            for f in files:
                ext = os.path.splitext(f)[1].lower()
                if ext in COBOL_COPY_EXTS:
                    out.append(os.path.join(dirpath,f))
    return sorted(out)

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

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

def embed_and_upload(ep: str, key: str, docs: List[Dict[str,Any]], batch_size: int, embed_batch_size: int):
    if not docs:
        return 0
    ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    texts = [d['summary'] for d in docs]
    attempt = 0
    while True:
        try:
            vecs = batch_embed(texts, target_dim=None, batch_size=embed_batch_size)
            break
        except Exception as e:
            attempt += 1
            if attempt > 5:
                print(f"[FATAL] Embedding failed after retries: {e}")
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
        d['summary_vector'] = v
        d['has_vector'] = True
        d['ingested_at'] = ts
    uploaded = 0
    for i in range(0, len(docs), batch_size):
        chunk = docs[i:i+batch_size]
        payload = {'value': [{"@search.action":"mergeOrUpload", **c} for c in chunk]}
        url = f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
        r = requests.post(url, headers={'api-key': key, 'Content-Type':'application/json'}, json=payload, timeout=60)
        if r.status_code not in (200,201):
            print(f"[ERROR] Upload batch failed {r.status_code}: {r.text[:400]}")
        else:
            uploaded += len(chunk)
    return uploaded

def main():
    ap = argparse.ArgumentParser(description='Ingest copybook metadata into index.')
    ap.add_argument('--roots', nargs='+', default=['.'])
    ap.add_argument('--limit', type=int)
    ap.add_argument('--buffer-size', type=int, default=DEFAULT_BUFFER)
    ap.add_argument('--batch-size', type=int, default=200)
    ap.add_argument('--embed-batch-size', type=int, default=32)
    ap.add_argument('--resume', action='store_true')
    ap.add_argument('--state-file', default='copybook_meta_state.json')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    args = ap.parse_args()

    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    files = walk_copybooks(args.roots)
    if args.limit:
        files = files[:args.limit]

    state = load_state(args.state_file) if args.resume else {}
    start_index = state.get('file_index', 0)
    total_uploaded = state.get('uploaded', 0)
    total_parsed = state.get('parsed', 0)
    buffer: List[Dict[str,Any]] = []
    t0 = time.time()
    last_flush = t0
    current_file_index = 0

    def flush(reason: str):
        nonlocal buffer, total_uploaded, last_flush
        if not buffer:
            return
        uploaded = embed_and_upload(ep, key, buffer, args.batch_size, args.embed_batch_size)
        total_uploaded += uploaded
        buffer = []
        save_state(args.state_file, {'file_index': current_file_index, 'uploaded': total_uploaded, 'parsed': total_parsed, 'version': STATE_VERSION})
        now = time.time()
        rate = total_uploaded / max(1, now - t0)
        print(f"[FLUSH] reason={reason} uploaded+={uploaded} total_uploaded={total_uploaded} parsed={total_parsed} rate={rate:.1f}/s elapsed={now-t0:.1f}s")
        last_flush = now

    try:
        for i, path in enumerate(files):
            current_file_index = i
            if i < start_index:
                continue
            content = read_file(path)
            if not content:
                continue
            meta = scan_copybook(path, content)
            buffer.append(meta)
            total_parsed += 1
            if (i+1) % 100 == 0:
                print(f"Processed {i+1} copybooks parsed={total_parsed} uploaded={total_uploaded} buffer={len(buffer)}")
            if len(buffer) >= args.buffer_size:
                flush('buffer-full')
            if time.time() - last_flush > 120:
                flush('time-interval')
    except KeyboardInterrupt:
        print('\n[INTERRUPT] Flushing before exit...')
        flush('interrupt')
        print(f"[INTERRUPT] Partial state saved uploaded={total_uploaded} parsed={total_parsed}")
        return

    if args.dry_run:
        print(f"[DRY-RUN] Parsed {total_parsed} copybooks; sample:")
        for d in buffer[:min(10, len(buffer))]:
            print(json.dumps(d, indent=2)[:500])
        return

    flush('final')
    print(f"Ingestion complete uploaded={total_uploaded} parsed={total_parsed}")

if __name__ == '__main__':
    main()
