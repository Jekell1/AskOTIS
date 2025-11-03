#!/usr/bin/env python3
"""Incremental update script for vectorized indexes.

Detects changed blobs (by last modified) in JSONL dataset folders and re-uploads + re-embeds
only affected documents.

Strategy (lightweight, no external state DB):
 1. Maintain a local state file (incremental_state.json) recording per-blob last_modified + ETag.
 2. For each dataset (chunks, symbols, paragraphs, calls, xrefs) discover blob path (nested preferred).
 3. If blob changed (new ETag or timestamp), stream it and push docs with @search.action=mergeOrUpload.
 4. After ingestion for changed datasets, run backfill_vectors.py limited to those indexes with --force optional.

Notes:
  - Assumes same JSONL blob naming conventions used elsewhere.
  - You can restrict datasets via --datasets argument (comma list).
  - Embedding backfill is batched; we invoke the existing script via subprocess for reuse.
"""
import argparse, json, os, subprocess, sys, time
from typing import Dict, Any, List, Optional
from azure.storage.blob import BlobServiceClient
import requests, random

API_VERSION = "2024-07-01"
DATASET_CONFIG = {
    'chunks':       {'index':'code-chunks',      'nested':'chunks/chunks.jsonl',          'flat':'chunks.jsonl',          'key':'chunk_id'},
    'symbols':      {'index':'cobol-symbols',    'nested':'symbols/data_items.jsonl',      'flat':'data_items.jsonl',      'key':'item_id'},
    'paragraphs':   {'index':'cobol-paragraphs', 'nested':'paragraphs/paragraphs.jsonl',   'flat':'paragraphs.jsonl',      'key':'para_id'},
    'calls':        {'index':'cobol-calls',      'nested':'calls/calls.jsonl',             'flat':'calls.jsonl',          'key':'call_id'},
    'xrefs':        {'index':'cobol-xrefs',      'nested':'xrefs/xrefs.jsonl',             'flat':'xrefs.jsonl',          'key':'xref_id'},
}
STATE_FILE = 'incremental_state.json'


def load_settings():
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
    key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    blob_conn = vals['AzureWebJobsStorage']
    return ep, key, blob_conn


def load_state()->Dict[str,Any]:
    if os.path.exists(STATE_FILE):
        try:
            return json.load(open(STATE_FILE,'r',encoding='utf-8'))
        except Exception:
            return {}
    return {}

def save_state(st:Dict[str,Any]):
    tmp = STATE_FILE + '.tmp'
    with open(tmp,'w',encoding='utf-8') as f:
        json.dump(st,f,indent=2)
    os.replace(tmp,STATE_FILE)


def resolve_blob(base:str, cfg:Dict[str,str], cc)->Optional[str]:
    for path in (f"{base}/{cfg['nested']}", f"{base}/{cfg['flat']}"):
        bc = cc.get_blob_client(path)
        if bc.exists():
            return path
    return None


def stream_jsonl(bc):
    dl = bc.download_blob()
    buf = b''
    for chunk in dl.chunks():
        buf += chunk
        while True:
            nl = buf.find(b'\n')
            if nl == -1:
                break
            line = buf[:nl]; buf = buf[nl+1:]
            if not line.strip():
                continue
            yield line.decode('utf-8','ignore')
    if buf.strip():
        yield buf.decode('utf-8','ignore')


def upload_batch(ep:str, key:str, index:str, docs:List[Dict[str,Any]]):
    if not docs: return
    url = f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload = {"value": docs}
    backoff = 1.5
    for attempt in range(1,7):
        try:
            r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=payload, timeout=120)
            if r.status_code < 300:
                return
            if (r.status_code >=500 or r.status_code==429) and attempt<6:
                wait = backoff**attempt + random.random()
                time.sleep(wait)
                continue
            raise RuntimeError(f"Upload failed {r.status_code}: {r.text[:200]}")
        except Exception as e:
            if attempt<6:
                wait = backoff**attempt + random.random()
                time.sleep(wait)
                continue
            raise


def ingest_blob(ep:str, key:str, index:str, bc, key_field:str):
    batch:List[Dict[str,Any]] = []
    total = 0
    for raw in stream_jsonl(bc):
        try:
            doc = json.loads(raw)
        except Exception:
            continue
        doc['@search.action'] = 'mergeOrUpload'
        if key_field not in doc:
            continue
        batch.append(doc)
        if len(batch) >= 500:
            upload_batch(ep,key,index,batch); total += len(batch); batch.clear()
    if batch:
        upload_batch(ep,key,index,batch); total += len(batch)
    return total


def run_backfill(indexes:List[str], embed_dep:Optional[str]):
    cmd = [sys.executable, 'backfill_vectors.py', '--indexes', ','.join(indexes)]
    if embed_dep:
        cmd += ['--embed-deployment', embed_dep]
    subprocess.check_call(cmd)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--container', required=True)
    ap.add_argument('--prefix', required=True)
    ap.add_argument('--datasets', help='Comma list (default all)')
    ap.add_argument('--embed-deployment', help='Embedding deployment override')
    ap.add_argument('--no-embed', action='store_true', help='Skip embedding backfill step')
    args = ap.parse_args()

    ep, search_key, blob_conn = load_settings()
    bsc = BlobServiceClient.from_connection_string(blob_conn)
    cc = bsc.get_container_client(args.container)
    base = f"{args.prefix.rstrip('/')}/JSONL"
    want = [d.strip() for d in (args.datasets.split(',') if args.datasets else DATASET_CONFIG.keys()) if d.strip()]
    state = load_state()
    changed_indexes:List[str] = []

    for ds in want:
        cfg = DATASET_CONFIG.get(ds)
        if not cfg:
            print(f"Skip unknown dataset {ds}")
            continue
        blob_path = resolve_blob(base, cfg, cc)
        if not blob_path:
            print(f"No blob for {ds}")
            continue
        bc = cc.get_blob_client(blob_path)
        props = bc.get_blob_properties()
        lm = props['last_modified'].timestamp()
        etag = props['etag']
        rec = state.get(blob_path)
        if rec and rec.get('etag') == etag:
            continue  # unchanged
        print(f"∆ {ds}: ingesting changed blob {blob_path}")
        count = ingest_blob(ep, search_key, cfg['index'], bc, cfg['key'])
        print(f"  uploaded {count} docs -> {cfg['index']}")
        state[blob_path] = {'etag': etag, 'ts': lm}
        changed_indexes.append(cfg['index'])

    save_state(state)

    if changed_indexes and not args.no_embed:
        run_backfill(changed_indexes, args.embed_deployment)
    elif changed_indexes:
        print("Skipped embedding backfill (--no-embed)")
    else:
        print("No dataset changes detected.")

if __name__ == '__main__':
    main()
