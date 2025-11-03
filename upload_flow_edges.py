#!/usr/bin/env python3
"""Upload flow edge documents from a JSONL file into the 'cobol-flow-edges' Azure AI Search index.

Reads configuration from local.settings.json / environment:
  SEARCH_ENDPOINT or AZURE_SEARCH_ENDPOINT
  SEARCH_KEY or AZURE_SEARCH_KEY
  (optional) BATCH_SIZE (default 1000)

Usage:
  python upload_flow_edges.py --jsonl JSONL/flow_edges.jsonl --batch 800

The script performs basic validation:
  - Ensures required fields edge_id, caller_para, target_para, kind, line, file_id
  - Deduplicates by edge_id inside the load
  - Reports ingestion errors (status != 201/200) at doc level if any

Prereqs: pip install azure-search-documents
"""
import os, json, argparse, sys, time
from typing import List, Dict

REQUIRED_FIELDS = {"edge_id","caller_para","target_para","kind","line","file_id"}

# We use REST API directly for minimal dependency if azure-search-documents not installed
import requests
API_VERSION = "2024-07-01"

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
        'search_endpoint': first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT'),
        'search_key': first('AZURE_SEARCH_KEY','SEARCH_KEY'),
    }
    missing = [k for k,v in cfg.items() if not v]
    if missing:
        print(f"Missing config values: {missing}", file=sys.stderr)
        sys.exit(2)
    cfg['search_endpoint'] = cfg['search_endpoint'].rstrip('/')
    return cfg


def read_jsonl(path: str):
    seen = set()
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except Exception:
                print(f"WARN: skip malformed JSON line: {line[:120]}")
                continue
            if not REQUIRED_FIELDS.issubset(obj.keys()):
                print(f"WARN: missing required fields in edge_id={obj.get('edge_id')} -> skipping")
                continue
            eid = obj['edge_id']
            if eid in seen:
                continue
            seen.add(eid)
            obj['@search.action'] = 'mergeOrUpload'
            yield obj


def batch(iterable, size):
    buf=[]
    for item in iterable:
        buf.append(item)
        if len(buf)>=size:
            yield buf
            buf=[]
    if buf:
        yield buf


def upload(cfg, docs: List[Dict]):
    url = f"{cfg['search_endpoint']}/indexes/cobol-flow-edges/docs/index?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json={'value': docs}, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Batch failed {r.status_code}: {r.text[:300]}")
    data = r.json()
    errors = [item for item in data.get('value',[]) if not item.get('succeeded')]
    return len(docs), len(errors), errors


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--jsonl', default='JSONL/flow_edges.jsonl', help='Path to JSONL with edge docs')
    ap.add_argument('--batch', type=int, default=1000, help='Batch size')
    ap.add_argument('--max-docs', type=int, default=0, help='Limit total docs uploaded (0=all)')
    ap.add_argument('--dry-run', action='store_true', help='Parse & validate only; no upload')
    args = ap.parse_args()

    cfg = load_settings()

    total=0
    total_errors=0
    start = time.time()
    for b in batch(read_jsonl(args.jsonl), args.batch):
        if args.max_docs and total>=args.max_docs:
            break
        if args.dry_run:
            total += len(b)
            continue
        uploaded, errs, err_detail = upload(cfg, b)
        total += uploaded
        total_errors += errs
        print(f"Uploaded batch: docs={uploaded} errors={errs} total={total}")
        if errs:
            for e in err_detail[:5]:
                print(f"  Error doc: key={e.get('key')} error={e.get('errorMessage')}")
    dur = time.time()-start
    print(f"Done. total_docs={total} total_errors={total_errors} elapsed={dur:.1f}s")
    if total_errors:
        print("Some documents failed to ingest.", file=sys.stderr)

if __name__=='__main__':
    main()
