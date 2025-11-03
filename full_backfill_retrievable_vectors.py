#!/usr/bin/env python3
"""Full backfill of retrievable vector field (contentVector_r) with continuous progress output.

Features:
  - Streams through ALL documents (uses $skip paging) until exhaustion.
  - Skips docs already having non-empty contentVector_r unless --overwrite.
  - Resume support via --start-skip (raw skip offset) AND optional checkpoint file storing last processed skip.
  - Periodic progress line every batch: processed, embedded, rate (embeds/sec), ETA.
  - Graceful interrupt: Ctrl+C writes checkpoint then exits.
  - Batch embedding with Azure OpenAI (same env vars as sampler). Fallback auto-load from local.settings.json.

Env required:
  SEARCH_ENDPOINT/SEARCH_KEY (or AZURE_* variants)
  AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_KEY, AZURE_OPENAI_EMBED_DEPLOYMENT

Usage:
  python full_backfill_retrievable_vectors.py --index new-cobol-files
  python full_backfill_retrievable_vectors.py --index new-cobol-files --batch-size 32 --page-size 120
  python full_backfill_retrievable_vectors.py --index new-cobol-files --overwrite --checkpoint backfill.ckpt

Exit codes:
  0 success / complete
  130 interrupted (checkpoint written)
"""
from __future__ import annotations
import os, json, time, argparse, math, signal, sys, random
from typing import List, Dict
import requests

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2024-07-01')
VECTOR_DIMS = 3072
FIELD = 'contentVector_r'
SUMMARY_FIELD = 'summary'
CONTENT_FIELD = 'contentShort'

_stop = False

def _sigint(signum, frame):
    global _stop
    _stop = True
    print("\nInterrupt received; finishing current batch then exiting...", file=sys.stderr)

signal.signal(signal.SIGINT, _sigint)

# ---------------- Credential helpers ----------------

def load_local_settings_values():
    path='local.settings.json'
    if not os.path.exists(path): return {}
    try:
        with open(path,'r',encoding='utf-8') as f:
            return json.load(f).get('Values',{}) or {}
    except Exception:
        return {}

def resolve_search_creds():
    vals=load_local_settings_values()
    ep=(os.getenv('SEARCH_ENDPOINT') or os.getenv('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT'))
    key=(os.getenv('SEARCH_KEY') or os.getenv('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY'))
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY credentials.')
    return ep.rstrip('/'), key

def ensure_openai():
    vals=load_local_settings_values()
    for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT']:
        if not os.getenv(k) and vals.get(k):
            os.environ[k]=vals[k]
    missing=[k for k in ['AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBED_DEPLOYMENT'] if not os.getenv(k)]
    if missing:
        raise SystemExit(f'Missing Azure OpenAI vars: {", ".join(missing)}')

# ---------------- API helpers ----------------

def fetch_page(ep: str, key: str, index: str, skip: int, top: int, select: str, retries: int = 4):
    params={'api-version':API_VERSION,'$skip':str(skip),'$top':str(top),'$select':select}
    url=f"{ep}/indexes/{index}/docs"
    delay=1.5
    for attempt in range(retries+1):
        try:
            r=requests.get(url, headers={'api-key':key}, params=params, timeout=60)
            if r.status_code==200:
                return r.json().get('value',[])
            if r.status_code in (429,500,502,503,504) and attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Fetch failed {r.status_code}: {r.text[:180]}')
        except requests.RequestException as ex:
            if attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Fetch exception after retries: {ex}')
    return []

def upload_vectors(ep: str, key: str, index: str, docs: List[Dict], retries: int = 4):
    if not docs: return
    url=f"{ep}/indexes/{index}/docs/index?api-version={API_VERSION}"
    payload={'value':docs}
    body=json.dumps(payload).encode('utf-8')
    delay=1.5
    for attempt in range(retries+1):
        try:
            r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, data=body, timeout=120)
            if r.status_code<300:
                failures=[x for x in r.json().get('value',[]) if not x.get('status')]
                if failures:
                    raise RuntimeError(f"{len(failures)} merge failures; sample {failures[0]}")
                return
            if r.status_code in (429,500,502,503,504) and attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Upload failed {r.status_code}: {r.text[:180]}')
        except requests.RequestException as ex:
            if attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Upload exception after retries: {ex}')

def embed_batch(texts: List[str], retries: int = 3) -> List[List[float]]:
    ensure_openai()
    endpoint=os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
    dep=os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT')
    key=os.getenv('AZURE_OPENAI_KEY')
    url=f"{endpoint}/openai/deployments/{dep}/embeddings?api-version=2024-08-01-preview"
    delay=1.5
    for attempt in range(retries+1):
        try:
            r=requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json={'input':texts}, timeout=120)
            if r.status_code==200:
                data=r.json().get('data',[])
                out=[d['embedding'] for d in data]
                for v in out:
                    if len(v)!=VECTOR_DIMS:
                        raise RuntimeError(f'Dimension mismatch expected {VECTOR_DIMS} got {len(v)}')
                return out
            if r.status_code in (429,500,502,503,504) and attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Embed request failed {r.status_code}: {r.text[:180]}')
        except requests.RequestException as ex:
            if attempt<retries:
                time.sleep(delay); delay*=2; continue
            raise RuntimeError(f'Embed exception after retries: {ex}')
    return []

# ---------------- Checkpoint ----------------

def load_checkpoint(path: str) -> int:
    if not path or not os.path.exists(path): return 0
    try:
        return int(open(path,'r').read().strip() or '0')
    except Exception:
        return 0

def save_checkpoint(path: str, skip: int):
    if not path: return
    with open(path,'w',encoding='utf-8') as f:
        f.write(str(skip))

# ---------------- Main ----------------

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--index', required=True)
    ap.add_argument('--batch-size', type=int, default=32)
    ap.add_argument('--page-size', type=int, default=120)
    ap.add_argument('--overwrite', action='store_true')
    ap.add_argument('--start-skip', type=int, default=0)
    ap.add_argument('--checkpoint', default='', help='Optional checkpoint file to persist skip offset.')
    ap.add_argument('--progress-every', type=int, default=1, help='Emit progress line every N batches.')
    ap.add_argument('--diag', action='store_true', help='Verbose per-page diagnostics.')
    ap.add_argument('--verify-schema', action='store_true', help='Verify retrievability of the target field before processing.')
    ap.add_argument('--max-pages', type=int, default=0, help='Optional page limit (debug).')
    args=ap.parse_args()

    ep, key = resolve_search_creds()
    skip = args.start_skip or load_checkpoint(args.checkpoint)

    if args.verify_schema:
        meta=requests.get(f"{ep}/indexes/{args.index}?api-version={API_VERSION}", headers={'api-key':key})
        if meta.status_code==200:
            try:
                j=meta.json(); fld=next((f for f in j.get('fields',[]) if f.get('name')==FIELD), None)
                if not fld:
                    print(f"SCHEMA WARNING: Field {FIELD} not found in index definition.")
                else:
                    print(f"SCHEMA Field {FIELD} retrievable={fld.get('retrievable')} type={fld.get('type')} dims={fld.get('vectorSearchDimensions') or fld.get('dimensions')} profile={fld.get('vectorSearchProfile')}")
            except Exception:
                print('SCHEMA parse failed')
        else:
            print(f'SCHEMA fetch failed {meta.status_code}: {meta.text[:120]}')

    select=f"id,{SUMMARY_FIELD},{CONTENT_FIELD},{FIELD}"

    processed=0
    embedded=0
    start=time.time()
    batch_counter=0

    page_counter=0
    print(f"START index={args.index} batch_size={args.batch_size} page_size={args.page_size} overwrite={args.overwrite} start_skip={skip}")
    while not _stop:
        page=fetch_page(ep,key,args.index,skip,args.page_size,select)
        if not page:
            break
        skip += len(page)
        processed += len(page)
        page_counter += 1
        existing_in_page = sum(1 for d in page if isinstance(d.get(FIELD), list) and d.get(FIELD))
        # Prepare candidates
        candidates=[]
        for d in page:
            vec=d.get(FIELD)
            if vec and not args.overwrite:
                continue
            text=((d.get(SUMMARY_FIELD,'') or '') + '\n\n' + (d.get(CONTENT_FIELD,'') or '')[:4000]).strip()
            if not text:
                continue
            candidates.append((d['id'], text))
        if args.diag:
            print(f"PAGE {page_counter} skip_start={skip-len(page)} docs={len(page)} existing_vecs={existing_in_page} new_candidates={len(candidates)}")
        if args.max_pages and page_counter>=args.max_pages:
            print('Reached max-pages limit (debug).')
            break
        # Embed in sub-batches
        for i in range(0,len(candidates), args.batch_size):
            sub=candidates[i:i+args.batch_size]
            if not sub: continue
            texts=[t for _,t in sub]
            vectors=embed_batch(texts)
            merge=[{'@search.action':'merge','id':doc_id, FIELD: vec} for (doc_id,_), vec in zip(sub,vectors)]
            upload_vectors(ep,key,args.index,merge)
            embedded += len(sub)
            batch_counter += 1
            if batch_counter % args.progress_every == 0:
                elapsed=time.time()-start
                rate=embedded/elapsed if elapsed>0 else 0.0
                print(f"PROGRESS page={page_counter} processed={processed} embedded={embedded} existing_in_page={existing_in_page} skip={skip} rate={rate:.2f}/s elapsed={elapsed/60:.1f}m")
        if args.checkpoint:
            save_checkpoint(args.checkpoint, skip)
    # Final checkpoint
    if args.checkpoint:
        save_checkpoint(args.checkpoint, skip)
    elapsed=time.time()-start
    print(f"DONE processed={processed} embedded={embedded} elapsed={elapsed/60:.2f}m")
    if _stop:
        sys.exit(130)

if __name__=='__main__':
    main()
