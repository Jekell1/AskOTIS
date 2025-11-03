#!/usr/bin/env python3
"""Upload enriched flow edge documents into cobol-flow-edges-v2.

Assumes you already ran resolve_flow_edges.py and produced flow_edges_enriched.jsonl.

Features:
  - Batch upload with configurable size
  - Exponential backoff on 429 / 503 / transient network errors
  - Validation queries after upload (count, sample resolved, unresolved)
  - Dry-run & max-docs support for pilot testing

Usage:
  python upload_enriched_flow_edges.py \
      --jsonl JSONL/flow_edges_enriched.jsonl --batch 800 --max-docs 0

Environment / settings required:
  SEARCH_ENDPOINT or AZURE_SEARCH_ENDPOINT
  SEARCH_KEY or AZURE_SEARCH_KEY

Exit codes:
  0 success (even if some docs failed but below threshold)
  2 fatal configuration / HTTP error
"""
import os, sys, json, time, argparse, math, random, re, hashlib
from typing import List, Dict, Iterable
import requests

API_VERSION = "2024-07-01"
TARGET_INDEX = "cobol-flow-edges-v2"
TRANSIENT_STATUS = {429, 503, 500}

REQUIRED_FIELDS = {"edge_id","file_id","caller_para","raw_target","resolved","kind"}

class UploadError(Exception):
    pass

def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    cfg = {
        'endpoint': (first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT') or '').rstrip('/'),
        'key': first('AZURE_SEARCH_KEY','SEARCH_KEY')
    }
    missing = [k for k,v in cfg.items() if not v]
    if missing:
        raise SystemExit(f"Missing config values: {missing}")
    return cfg

def read_jsonl(path: str, required: set, max_docs: int = 0) -> Iterable[Dict]:
    with open(path,'r',encoding='utf-8') as f:
        for i,line in enumerate(f,1):
            if max_docs and i>max_docs:
                break
            line=line.strip()
            if not line:
                continue
            try:
                obj=json.loads(line)
            except Exception:
                continue
            if not required.issubset(obj):
                continue
            obj['@search.action'] = 'mergeOrUpload'
            yield obj

def batch(iterable, size: int):
    buf=[]
    for item in iterable:
        buf.append(item)
        if len(buf)>=size:
            yield buf
            buf=[]
    if buf:
        yield buf

def post_with_retry(url: str, key: str, docs: List[Dict], max_retries=5):
    attempt=0
    while True:
        attempt+=1
        try:
            r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'value':docs}, timeout=90)
        except requests.RequestException as e:
            if attempt>max_retries:
                raise UploadError(f"Network error after {attempt} attempts: {e}")
            sleep = min(8.0, 0.5 * (2 ** (attempt-1)))
            time.sleep(sleep)
            continue
        if r.status_code==200:
            data=r.json()
            vals = data.get('value',[])
            failed=[]
            for d in vals:
                # Azure Search returns 'status': true/false (older form) OR 'succeeded'
                status = d.get('status')
                if status is None:
                    status = d.get('succeeded')
                if status is False:
                    failed.append(d)
            return failed
        if r.status_code in TRANSIENT_STATUS and attempt<=max_retries:
            retry_after = float(r.headers.get('Retry-After','0') or 0)
            sleep = retry_after if retry_after>0 else min(8.0, 0.5 * (2 ** (attempt-1)))
            time.sleep(sleep)
            continue
        raise UploadError(f"HTTP {r.status_code}: {r.text[:400]}")

def validate_counts(cfg):
    url = f"{cfg['endpoint']}/indexes/{TARGET_INDEX}/docs?api-version={API_VERSION}&$top=0&$count=true"
    r=requests.get(url, headers={'api-key':cfg['key']}, timeout=30)
    if r.status_code!=200:
        print(f"⚠️ Count query failed: {r.status_code} {r.text[:160]}")
        return None
    return r.json().get('@odata.count')

def search_sample(cfg, search_text: str, filter_expr: str = None, top=5):
    url = f"{cfg['endpoint']}/indexes/{TARGET_INDEX}/docs/search?api-version={API_VERSION}"
    body = {"search": search_text, "top": top}
    if filter_expr:
        body['filter'] = filter_expr
    r=requests.post(url, headers={'api-key':cfg['key'],'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code!=200:
        return []
    return [x.get('edge_id') for x in r.json().get('value',[])]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--jsonl', default='JSONL/flow_edges_enriched.jsonl')
    ap.add_argument('--batch', type=int, default=800)
    ap.add_argument('--max-docs', type=int, default=0)
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--stop-on-fail', action='store_true', help='Abort on first batch with failures')
    ap.add_argument('--progress-every', type=int, default=10)
    args = ap.parse_args()

    cfg = load_settings()
    index_url = f"{cfg['endpoint']}/indexes/{TARGET_INDEX}/docs/index?api-version={API_VERSION}"

    total=0
    total_failed=0
    start=time.time()
    def sanitize_key(k: str) -> str:
        if not k:
            return k
        # Replace invalid chars with '-'
        sk = re.sub(r'[^A-Za-z0-9_\-=]', '-', k)
        # Collapse multiple dashes
        sk = re.sub(r'-{2,}', '-', sk)
        # Trim leading/trailing dashes
        sk = sk.strip('-') or 'edge'
        # Azure Search key max length safeguard (1024) -> cut at 256 for safety
        if len(sk) > 250:
            h = hashlib.sha1(k.encode()).hexdigest()[:8]
            sk = sk[:240] + '-' + h
        return sk

    for bi,b in enumerate(batch(read_jsonl(args.jsonl, REQUIRED_FIELDS, args.max_docs), args.batch),1):
        # Sanitize keys and ensure uniqueness inside batch
        seen_keys = {}
        for doc in b:
            orig = doc.get('edge_id')
            sk = sanitize_key(orig)
            if sk in seen_keys and seen_keys[sk] != orig:
                # Collision -> append short hash of original
                suffix = hashlib.sha1(orig.encode()).hexdigest()[:6]
                sk2 = f"{sk}-{suffix}"
                sk = sanitize_key(sk2)  # re-sanitize in case
            seen_keys[sk] = orig
            doc['edge_id'] = sk
        if args.dry_run:
            total += len(b)
            if bi % args.progress_every == 0:
                print(f"[dry-run] batches={bi} total_docs={total}")
            continue
        failed = post_with_retry(index_url, cfg['key'], b)
        total += len(b)
        total_failed += len(failed)
        if bi % args.progress_every == 0 or failed:
            print(f"Uploaded batch={bi} docs={len(b)} total={total} failed_batch={len(failed)} total_failed={total_failed}")
        if failed:
            for fdoc in failed[:5]:
                print(f"  ⚠️ failed edge key={fdoc.get('key')} error={fdoc.get('errorMessage')}")
            if args.stop_on_fail:
                break
    dur=time.time()-start
    print(f"Done. total_docs={total} total_failed={total_failed} elapsed={dur:.1f}s")

    if not args.dry_run:
        cnt = validate_counts(cfg)
        if cnt is not None:
            print(f"Index count now: {cnt}")
        # Validation samples
        examples = [
            ("TIM360", None),
            ("PERFORM", "resolved eq true"),
            ("TIM360", "resolved eq true")
        ]
        for s,f in examples:
            ids = search_sample(cfg, s, f)
            print(f"Sample search '{s}' filter={f}: {ids[:5]}")

    if total_failed>0:
        print("⚠️ Some documents failed to index.", file=sys.stderr)
        if args.stop_on_fail:
            sys.exit(2)

if __name__=='__main__':
    main()
