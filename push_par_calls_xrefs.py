#!/usr/bin/env python3
"""Direct push ingestion for additional datasets: paragraphs, calls, xrefs.

Rationale:
 Indexer-based ingestion for these datasets showed 0 docs after recent index recreation.
 To ensure deterministic, resumable ingestion (matching the proven chunks/symbols flow),
 we push JSONL lines directly to their target indexes using the Search REST API.

Datasets supported (folder -> blob -> index -> key field):
  paragraphs  paragraphs.jsonl   cobol-paragraphs  para_id
  calls       calls.jsonl        cobol-calls       call_id
  xrefs       xrefs.jsonl        cobol-xrefs       xref_id

Features:
  * Streams blob (no full local download)
  * Batch uploads with retry/backoff
  * Optional resume by line number (--resume-file)
  * Optional doc limit for testing
  * Dedupe guard (skip duplicate keys within a run) --helps if source contains repeats

Usage examples:
  python push_par_calls_xrefs.py --container aisearch --prefix S35-Source/ --which paragraphs
  python push_par_calls_xrefs.py --container aisearch --prefix S35-Source/ --which all --batch-size 500

Environment / settings:
  Requires local.settings.json with Values: AZURE_SEARCH_ENDPOINT / KEY and AzureWebJobsStorage
"""
import argparse, json, os, time, random, bisect
from typing import Dict, Any, List, Optional
import requests
from azure.storage.blob import BlobServiceClient

API_VERSION = "2024-07-01"

DATASETS: Dict[str, Dict[str,str]] = {
    "paragraphs": {
        "index": "cobol-paragraphs",
        "nested": "paragraphs/paragraphs.jsonl",
        "flat": "paragraphs.jsonl",
        "key_field": "para_id"
    },
    "calls": {
        "index": "cobol-calls",
        "nested": "calls/calls.jsonl",
        "flat": "calls.jsonl",
        "key_field": "call_id"
    },
    "xrefs": {
        "index": "cobol-xrefs",
        "nested": "xrefs/xrefs.jsonl",
        "flat": "xrefs.jsonl",
        "key_field": "xref_id"
    }
}

def load_settings() -> Dict[str,str]:
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    need = ["AzureWebJobsStorage"]
    for n in need:
        if not vals.get(n):
            raise SystemExit(f"Missing '{n}' in local.settings.json Values")
    if not (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')):
        raise SystemExit("Missing AZURE_SEARCH_ENDPOINT/SEARCH_ENDPOINT in settings")
    if not (vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')):
        raise SystemExit("Missing AZURE_SEARCH_KEY/SEARCH_KEY in settings")
    return vals

def resolve_blob_path(prefix_base: str, cfg: Dict[str,str], container_client) -> str:
    nested = f"{prefix_base}/{cfg['nested']}"
    flat = f"{prefix_base}/{cfg['flat']}"
    for candidate in (nested, flat):
        try:
            bc = container_client.get_blob_client(candidate)
            if bc.exists():
                return candidate
        except Exception:
            pass
    raise FileNotFoundError(f"Dataset blob not found (tried {nested} & {flat})")

def upload_batch(ep: str, key: str, index: str, docs: List[Dict[str,Any]]):
    if not docs: return
    url = f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    max_attempts = 6
    backoff = 1.6
    for attempt in range(1, max_attempts+1):
        try:
            r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=300)
            status = r.status_code
            retriable = status == 429 or status >= 500
            if status < 300:
                jr = r.json()
                fails = [v for v in jr.get('value',[]) if not v.get('succeeded', v.get('status', True))]
                if fails and attempt < max_attempts:
                    wait = backoff ** attempt + random.random()
                    print(f"  ⚠️ partial failure {len(fails)} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                    time.sleep(wait)
                    continue
                if fails:
                    raise RuntimeError(f"Unrecoverable partial batch failure count={len(fails)} first={fails[0]}")
                return
            if retriable and attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"  ⚠️ upload transient {status} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise RuntimeError(f"Upload failed {status}: {r.text[:400]}")
        except requests.exceptions.RequestException as e:
            if attempt < max_attempts:
                wait = backoff ** attempt + random.random()
                print(f"  ⚠️ network exception {type(e).__name__} attempt {attempt}/{max_attempts} retry in {wait:.1f}s")
                time.sleep(wait)
                continue
            raise

def get_index_count(ep: str, key: str, index: str) -> Optional[int]:
    try:
        u = f"{ep}/indexes/{index}/docs?api-version={API_VERSION}&$top=0&$count=true"
        r = requests.get(u, headers={'api-key':key}, timeout=60)
        return r.json().get('@odata.count')
    except Exception:
        return None

def stream_jsonl(blob_client, start_skip: int = 0, max_lines: Optional[int] = None):
    downloader = blob_client.download_blob()
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
        with open(self.path,'w',encoding='utf-8') as f:
            f.write(str(line_no))

def ingest_dataset(ep: str, key: str, bsc: BlobServiceClient, container: str, prefix: str, name: str, cfg: Dict[str,str], batch_size: int, resume: ResumeState, limit: Optional[int], dedupe: bool):
    cc = bsc.get_container_client(container)
    base = f"{prefix.rstrip('/')}/JSONL".rstrip('/')
    blob_path = resolve_blob_path(base, cfg, cc)
    bc = cc.get_blob_client(blob_path)
    print(f"→ {name}: blob={blob_path}")
    start_line = resume.line_done
    if start_line:
        print(f"  Resuming after line {start_line}")
    before = get_index_count(ep,key,cfg['index'])
    print(f"  Current index count: {before}")
    seen = set() if dedupe else None
    key_field = cfg['key_field']
    batch: List[Dict[str,Any]] = []
    total_sent = 0
    t0 = time.time()
    last_flush_t = t0
    for line_no, raw in stream_jsonl(bc, start_skip=start_line, max_lines=limit):
        try:
            doc = json.loads(raw)
        except Exception as e:
            if (total_sent % 5000) == 0:
                print(f"  ⚠️ JSON parse error line {line_no}: {e}")
            continue
        if seen is not None:
            kid = doc.get(key_field)
            if kid in seen:
                continue
            if kid is not None:
                seen.add(kid)
        doc['@search.action'] = 'upload'
        batch.append(doc)
        if len(batch) >= batch_size:
            upload_batch(ep,key,cfg['index'],batch)
            total_sent += len(batch)
            batch.clear()
            now = time.time()
            rate = total_sent / (now - t0)
            print(f"  flushed {total_sent} (line {line_no}) rate={rate:,.1f} docs/s batch_dt={now-last_flush_t:.2f}s")
            last_flush_t = now
            if resume.path and total_sent % (batch_size*2) == 0:
                resume.update(line_no)
    if batch:
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
    ap.add_argument('--which', choices=['paragraphs','calls','xrefs','all'], default='all')
    ap.add_argument('--batch-size', type=int, default=500)
    ap.add_argument('--max', type=int, help='Limit docs (testing)')
    ap.add_argument('--resume-file')
    ap.add_argument('--dedupe', action='store_true')
    args = ap.parse_args()

    vals = load_settings()
    ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
    key = (vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')).strip()
    bsc = BlobServiceClient.from_connection_string(vals['AzureWebJobsStorage'])
    targets = list(DATASETS.keys()) if args.which == 'all' else [args.which]
    resume = ResumeState(args.resume_file) if args.resume_file else ResumeState(None)
    for ds in targets:
        ingest_dataset(ep,key,bsc,args.container,args.prefix,ds,DATASETS[ds],args.batch_size,resume,args.max,args.dedupe)

if __name__ == '__main__':
    main()
