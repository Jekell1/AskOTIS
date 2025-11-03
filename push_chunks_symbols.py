#!/usr/bin/env python3
"""Push-ingest large JSONL datasets (chunks, symbols) directly into Azure AI Search, bypassing blob indexers.

Usage examples:
  python push_chunks_symbols.py --container aisearch --prefix S35-Source/ --which chunks --max 2000
  python push_chunks_symbols.py --container aisearch --prefix S35-Source/ --which both

Features:
  * Streams blob JSONL (no full file download) using azure-storage-blob
  * Batches uploads (default 500 docs) via Search REST API upload action
  * Automatic resume support via --resume-file (stores last processed line number)
  * Fallback logic: tries nested path (chunks/chunks.jsonl) then flat (chunks.jsonl)
  * Progress output with docs/sec estimate

Notes:
  * Ensure local.settings.json contains endpoint/key (AZURE_SEARCH_ENDPOINT/AZURE_SEARCH_KEY or SEARCH_ENDPOINT/SEARCH_KEY)
  * Large files (hundreds of MB) will take several minutes; script prints periodic flush stats.
"""
import argparse, json, os, time, requests, math, random, bisect
from typing import Optional, Dict, Any, List
from azure.storage.blob import BlobServiceClient

API_VERSION = "2024-07-01"
BATCH_DEFAULT = 500
DATASETS = {
    "chunks": {
        "index": "code-chunks",
        "nested": "chunks/chunks.jsonl",
        "flat": "chunks.jsonl",
        "key_field": "chunk_id"
    },
    "symbols": {
        "index": "cobol-symbols",
        "nested": "symbols/data_items.jsonl",
        "flat": "data_items.jsonl",
        "key_field": "item_id"
    }
}

class ResumeState:
    def __init__(self, path: Optional[str]):
        self.path = path
        self.line_done = 0
        if path and os.path.exists(path):
            try:
                self.line_done = int(open(path,'r',encoding='utf-8').read().strip() or '0')
            except Exception:
                self.line_done = 0
    def update(self, line_no: int):
        if not self.path: return
        # write infrequently (every 100 lines) handled by caller
        with open(self.path,'w',encoding='utf-8') as f:
            f.write(str(line_no))

def load_settings() -> Dict[str,str]:
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    return vals

def resolve_blob_path(prefix_base: str, ds_cfg: Dict[str,str], container_client) -> str:
    nested = f"{prefix_base}/{ds_cfg['nested']}"
    flat = f"{prefix_base}/{ds_cfg['flat']}"
    # prefer nested if exists
    for candidate in (nested, flat):
        try:
            bc = container_client.get_blob_client(candidate)
            if bc.exists():
                return candidate
        except Exception:
            pass
    raise FileNotFoundError(f"Neither nested nor flat blob found for dataset (tried {nested}, {flat})")

def get_index_count(ep: str, key: str, index: str) -> Optional[int]:
    try:
        u = f"{ep}/indexes/{index}/docs?api-version={API_VERSION}&$count=true&$top=0"
        r = requests.get(u, headers={'api-key':key}, timeout=60)
        return r.json().get('@odata.count')
    except Exception:
        return None

def upload_batch(ep: str, key: str, index: str, docs: List[Dict[str,Any]]):
    """Upload a batch with simple exponential backoff retries for transient errors.

    Retries cover connection timeouts / resets and HTTP 5xx / 429 conditions.
    """
    if not docs:
        return
    u = f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    max_attempts = 6
    backoff = 1.5
    attempt = 0
    while True:
        attempt += 1
        try:
            r = requests.post(u, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=300)
            status = r.status_code
            retriable = status >= 500 or status == 429
            if status < 300:
                jr = r.json()
                failures = []
                for v in jr.get('value', []):
                    ok = v.get('succeeded')
                    if ok is None:
                        ok = v.get('status')
                    if not ok:
                        failures.append(v)
                if failures:
                    # treat as retriable once in case of partial transient failure
                    if attempt < max_attempts:
                        wait = backoff ** attempt + random.random()
                        print(f"  ⚠️ partial batch failure ({len(failures)}) attempt {attempt}/{max_attempts}; retrying in {wait:.1f}s")
                        time.sleep(wait)
                        continue
                    raise RuntimeError(f"{len(failures)} doc failures; first: {failures[0]}")
                return
            else:
                if retriable and attempt < max_attempts:
                    wait = backoff ** attempt + random.random()
                    print(f"  ⚠️ transient upload error {status} attempt {attempt}/{max_attempts}; retrying in {wait:.1f}s: {r.text[:160]}")
                    time.sleep(wait)
                    continue
                raise RuntimeError(f"Upload failed {status}: {r.text[:400]}")
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"  ⚠️ network exception attempt {attempt}/{max_attempts} {type(e).__name__}: {e}; retrying in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise

# Replace original stream_jsonl with offset-capable version

def stream_jsonl(blob_client, start_skip: int = 0, max_lines: Optional[int] = None, start_offset: Optional[int] = None):
    downloader = blob_client.download_blob(offset=start_offset) if start_offset else blob_client.download_blob()
    buf = b''
    emitted = 0
    skipped = 0
    for chunk in downloader.chunks():
        buf += chunk
        while True:
            nl = buf.find(b'\n')
            if nl == -1:
                break
            line = buf[:nl]
            buf = buf[nl+1:]
            if not line.strip():
                continue
            skipped += 1
            if skipped <= start_skip:
                continue
            emitted += 1
            yield emitted + start_skip, line.decode('utf-8', errors='ignore')
            if max_lines and emitted >= max_lines:
                return
    if buf.strip():
        skipped += 1
        if skipped > start_skip:
            if not max_lines or (skipped - start_skip) <= max_lines:
                yield skipped, buf.decode('utf-8', errors='ignore')


def load_line_index(path: str):
    lines = []
    offsets = []
    with open(path,'r',encoding='utf-8') as f:
        for row in f:
            row = row.strip()
            if not row:
                continue
            try:
                ln, off = row.split(',',1)
                lines.append(int(ln))
                offsets.append(int(off))
            except ValueError:
                continue
    return lines, offsets


def ingest_dataset(ep: str, key: str, bsc: BlobServiceClient, container: str, prefix: str, name: str, cfg: Dict[str,str], batch_size: int, resume: ResumeState, limit: Optional[int], dry: bool, dedupe: bool, line_index_path: Optional[str]):
    cc = bsc.get_container_client(container)
    base = f"{prefix.rstrip('/')}/JSONL".rstrip('/')
    blob_path = resolve_blob_path(base, cfg, cc)
    bc = cc.get_blob_client(blob_path)
    print(f"→ {name}: blob={blob_path}")
    start_line = resume.line_done
    start_offset = None
    if start_line:
        print(f"  Resuming after line {start_line}")
        if line_index_path and os.path.exists(line_index_path):
            try:
                lns, offs = load_line_index(line_index_path)
                pos = bisect.bisect_right(lns, start_line) - 1
                if pos >= 0:
                    ck_line = lns[pos]
                    ck_off = offs[pos]
                    adj_skip = start_line - ck_line
                    if ck_off >= 0 and ck_line >= 0 and adj_skip >= 0:
                        print(f"  Using line-index checkpoint line={ck_line} offset={ck_off} adjusted_skip={adj_skip}")
                        start_offset = ck_off
                        start_line = adj_skip
            except Exception as e:
                print(f"  ⚠️ line-index load failed: {e}")
    before = get_index_count(ep,key,cfg['index'])
    print(f"  Current index count: {before}")
    t0 = time.time()
    batch: List[Dict[str,Any]] = []
    total_sent = 0
    last_flush_t = t0
    seen_keys = set() if dedupe else None
    key_field = cfg['key_field']
    for line_no, raw in stream_jsonl(bc, start_skip=start_line, max_lines=limit, start_offset=start_offset):
        try:
            doc = json.loads(raw)
        except Exception as e:
            print(f"  ⚠️ JSON parse error line {line_no}: {e}")
            continue
        if seen_keys is not None:
            kid = doc.get(key_field)
            if kid is None:
                # allow through, but warn once per 5k anomalies
                if (total_sent + len(batch)) % 5000 == 0:
                    print(f"  ⚠️ Missing key field '{key_field}' at line {line_no}")
            else:
                if kid in seen_keys:
                    # skip duplicate upload
                    continue
                seen_keys.add(kid)
        # Attach action
        doc['@search.action'] = 'upload'
        batch.append(doc)
        if len(batch) >= batch_size:
            if not dry:
                upload_batch(ep,key,cfg['index'],batch)
            total_sent += len(batch)
            batch.clear()
            now = time.time()
            dt = now - last_flush_t
            rate = (total_sent)/(now - t0) if (now - t0) else 0
            print(f"  flushed {total_sent} (line {line_no}) rate={rate:,.1f} docs/s batch_dt={dt:.2f}s")
            last_flush_t = now
            if resume.path and (total_sent % (batch_size*2) == 0):
                resume.update(line_no)
    if batch:
        if not dry:
            upload_batch(ep,key,cfg['index'],batch)
        total_sent += len(batch)
    if resume.path:
        resume.update(start_line + total_sent)
    after = get_index_count(ep,key,cfg['index'])
    print(f"  Done {name}: uploaded={total_sent} before={before} after={after} elapsed={time.time()-t0:.1f}s")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--container', required=True)
    ap.add_argument('--prefix', required=True)
    ap.add_argument('--which', choices=['chunks','symbols','both'], default='both')
    ap.add_argument('--batch-size', type=int, default=BATCH_DEFAULT)
    ap.add_argument('--max', type=int, help='Limit number of docs (for testing)')
    ap.add_argument('--resume-file', help='Store last processed line (enables resume)')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--dedupe', action='store_true', help='Skip uploading duplicate key values within a single run (local set)')
    ap.add_argument('--max-text-bytes', type=int, default=30000, help='For chunks dataset: truncate text field beyond this many bytes to avoid term-too-large errors')
    ap.add_argument('--line-index', help='Optional line index CSV (line,offset) for fast resume')
    args = ap.parse_args()

    vals = load_settings()
    ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
    key = (vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')).strip()
    bsc = BlobServiceClient.from_connection_string(vals['AzureWebJobsStorage'])

    target = ['chunks','symbols'] if args.which == 'both' else [args.which]
    resume = ResumeState(args.resume_file) if args.resume_file else ResumeState(None)

    for ds in target:
        if ds == 'chunks' and args.max_text_bytes:
            original_upload_batch = upload_batch
            def truncating_upload(ep_, key_, index_, docs_):
                for d in docs_:
                    txt = d.get('text')
                    if not txt:
                        continue
                    b = txt.encode('utf-8')
                    if len(b) > args.max_text_bytes:
                        d['text'] = b[:args.max_text_bytes].decode('utf-8','ignore')
                return original_upload_batch(ep_, key_, index_, docs_)
            globals()['upload_batch'] = truncating_upload
            try:
                ingest_dataset(ep,key,bsc,args.container,args.prefix,ds,DATASETS[ds],args.batch_size,resume,args.max,args.dry_run,args.dedupe, args.line_index)
            finally:
                globals()['upload_batch'] = original_upload_batch
        else:
            ingest_dataset(ep,key,bsc,args.container,args.prefix,ds,DATASETS[ds],args.batch_size,resume,args.max,args.dry_run,args.dedupe, args.line_index)

if __name__ == '__main__':
    main()
