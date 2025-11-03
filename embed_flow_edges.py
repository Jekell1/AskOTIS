#!/usr/bin/env python3
"""Embed flow edge documents into edge_vector with edge_text content.

Logic mirrors embed_cobol_copybooks.py / ingest_* scripts:
  - Page through index using order-by on key (edge_id) else fallback to skip.
  - Build edge_text from caller_para, raw_target, resolved_target_para, edge_subkind, resolution_strategy.
  - Skip docs already having has_vector unless --force.
  - Batch embed using embedding_utils.batch_embed
  - Upload via mergeOrUpload with has_vector flag.

Usage:
  python embed_flow_edges.py [--force] [--limit N] [--batch 64]
"""
import os, json, sys, requests, argparse, time, random
from typing import List, Optional, Tuple, Callable

API='2024-07-01'
INDEX='cobol-flow-edges-v2'
ID_FIELD='edge_id'
VECTOR_FIELD='edge_vector'
TEXT_FIELD='edge_text'
FLAG_FIELD='has_vector'
DEFAULT_BATCH=64
PAGE=1000
PROGRESS_FILE='flow_edges_embed_progress.json'
PROGRESS_SAVE_INTERVAL=30  # seconds

def load_env():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            os.environ.setdefault(k,v)
    except Exception:
        pass
    ep=os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key=os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    return ep.rstrip('/'), key

def load_progress(resume: bool) -> Tuple[Optional[str], int, int]:
    """Return (last_id, embedded_count, scanned_total)."""
    if not resume or not os.path.exists(PROGRESS_FILE):
        return None, 0, 0
    try:
        data=json.load(open(PROGRESS_FILE,'r',encoding='utf-8'))
        return data.get('last_id'), data.get('embedded',0), data.get('scanned',0)
    except Exception:
        return None, 0, 0

def save_progress(last_id: Optional[str], embedded: int, scanned: int):
    tmp=PROGRESS_FILE+'.tmp'
    with open(tmp,'w',encoding='utf-8') as f:
        json.dump({'last_id':last_id,'embedded':embedded,'scanned':scanned,'ts':time.time()}, f, indent=2)
    os.replace(tmp, PROGRESS_FILE)

def post_with_retry(session: requests.Session, url: str, headers: dict, body: dict, *,
                    max_retries: int = 5, base_delay: float = 1.0,
                    connect_timeout: float = 8.0, read_timeout: float = 45.0,
                    verify_ssl: bool = True):
    for attempt in range(max_retries):
        try:
            r=session.post(url, headers=headers, json=body,
                           timeout=(connect_timeout, read_timeout), verify=verify_ssl)
            return r
        except KeyboardInterrupt:
            raise
        except Exception as e:
            wait = base_delay * (2 ** attempt) + random.random()
            print(f"Request error {e.__class__.__name__}: {e} (attempt {attempt+1}/{max_retries}) -> retry in {wait:.1f}s", flush=True)
            time.sleep(wait)
    raise RuntimeError('Exceeded max retries for POST request to Azure Search')

def stream_docs(ep: str, key: str, select_fields: str, start_after: Optional[str], scanned_so_far: int,
                session: requests.Session, connect_timeout: float, read_timeout: float, verify_ssl: bool):
    headers={'api-key':key,'Content-Type':'application/json'}
    last_id=start_after
    use_order=True
    while True:
        filter_expr=None
        if last_id:
            safe=last_id.replace("'","''")
            filter_expr=f"{ID_FIELD} gt '{safe}'"
        body={'search':'*','select':select_fields,'top':PAGE}
        if use_order:
            body['orderby']=f'{ID_FIELD} asc'
        if filter_expr:
            body['filter']=filter_expr
        r=post_with_retry(session, f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers, body,
                          connect_timeout=connect_timeout, read_timeout=read_timeout, verify_ssl=verify_ssl)
        if r.status_code==400 and use_order and 'orderby' in r.text.lower():
            # fallback to skip mode
            use_order=False
            yield from legacy_skip(ep, key, select_fields, scanned_so_far, session, connect_timeout, read_timeout, verify_ssl)
            return
        if r.status_code!=200:
            raise RuntimeError(f"Fetch error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            scanned_so_far += 1
            yield d, scanned_so_far
        last_id=batch[-1].get(ID_FIELD)
        if len(batch)<PAGE:
            break

def legacy_skip(ep: str, key: str, select_fields: str, scanned_so_far: int,
                session: requests.Session, connect_timeout: float, read_timeout: float, verify_ssl: bool):
    headers={'api-key':key,'Content-Type':'application/json'}
    skip=0
    while True:
        body={'search':'*','select':select_fields,'top':PAGE,'skip':skip}
        r=post_with_retry(session, f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers, body,
                          connect_timeout=connect_timeout, read_timeout=read_timeout, verify_ssl=verify_ssl)
        if r.status_code!=200:
            raise RuntimeError(f"Legacy skip fetch error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            scanned_so_far += 1
            yield d, scanned_so_far
        skip+=len(batch)
        if skip>=100000:
            print('Reached 100k skip cap; stopping early.')
            break
        if len(batch)<PAGE:
            break

def build_text(d: dict) -> str:
    parts=[
        d.get('caller_para',''),
        d.get('raw_target',''),
        d.get('resolved_target_para',''),
        d.get('edge_subkind',''),
        d.get('resolution_strategy','')
    ]
    return ' | '.join(p for p in parts if p)

def upload(ep: str, key: str, docs: List[dict]):
    if not docs: return
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload, timeout=60)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--force', action='store_true', help='Re-embed even if has_vector already true')
    ap.add_argument('--limit', type=int, default=0, help='Max docs to embed (0 = all)')
    ap.add_argument('--batch', type=int, default=DEFAULT_BATCH, help='Embedding batch size')
    ap.add_argument('--resume', action='store_true', help='Resume from last progress file position')
    ap.add_argument('--reset-progress', action='store_true', help='Ignore and delete existing progress file before run')
    ap.add_argument('--disable-hf', action='store_true', help='Disable HuggingFace fallback provider (Azure only)')
    ap.add_argument('--stream', action='store_true', help='Embed on-the-fly while scanning (lower memory, continuous progress)')
    ap.add_argument('--flush-size', type=int, default=400, help='Docs accumulated before forcing an embedding flush in --stream mode (default 400)')
    ap.add_argument('--flush-interval', type=int, default=90, help='Max seconds between automatic flushes in --stream mode (default 90)')
    ap.add_argument('--connect-timeout', type=float, default=8.0, help='Connect timeout seconds for Azure Search requests')
    ap.add_argument('--read-timeout', type=float, default=45.0, help='Read timeout seconds for Azure Search requests')
    ap.add_argument('--insecure', action='store_true', help='Disable SSL verification (NOT recommended, last resort)')
    args=ap.parse_args()

    ep,key=load_env()
    # Set env var BEFORE importing embedding_utils so provider init respects flag
    if args.disable_hf:
        os.environ['DISABLE_HF_FALLBACK']='1'

    from embedding_utils import batch_embed, provider_info  # deferred import

    print('Starting embed_flow_edges main()', flush=True)
    print('Embedding provider:', provider_info(), flush=True)
    select=f"{ID_FIELD},caller_para,raw_target,resolved_target_para,edge_subkind,resolution_strategy,{FLAG_FIELD},{TEXT_FIELD}" if FLAG_FIELD else f"{ID_FIELD},caller_para,raw_target,resolved_target_para,edge_subkind,resolution_strategy,{TEXT_FIELD}"
    if args.reset_progress and os.path.exists(PROGRESS_FILE):
        os.remove(PROGRESS_FILE)
        print('Deleted existing progress file.')

    session=requests.Session()
    verify_ssl=not args.insecure
    if not verify_ssl:
        print('WARNING: SSL verification disabled (--insecure). This should only be used temporarily.', flush=True)
    start_after, previously_embedded, previously_scanned = load_progress(args.resume)
    if start_after:
        print(f"Resuming after id={start_after} (embedded so far recorded: {previously_embedded}, scanned: {previously_scanned})")
    else:
        print('Starting fresh scan.')

    candidates=[]
    scanned=previously_scanned
    last_progress_save=time.time()
    batch_docs=[]; batch_texts=[]; done=0; total_to_embed=0
    streaming_mode = args.stream

    last_flush_time=time.time()

    def flush(force: bool=False):
        nonlocal batch_docs, batch_texts, done, previously_embedded
        nonlocal last_flush_time
        if not batch_docs:
            return
        # Only flush if force or threshold conditions reached
        if streaming_mode and not force:
            size_trigger = len(batch_docs) >= max(1, args.flush_size)
            interval_trigger = (time.time() - last_flush_time) >= args.flush_interval
            if not (size_trigger or interval_trigger):
                return
        try:
            vecs=batch_embed(batch_texts, batch_size=args.batch)
        except KeyboardInterrupt:
            print('KeyboardInterrupt during embedding batch; partial batch discarded (no upload).', flush=True)
            raise
        for doc,vec in zip(batch_docs,vecs):
            if not doc.get(TEXT_FIELD):
                doc[TEXT_FIELD]=build_text(doc)
            doc[VECTOR_FIELD]=vec
            doc[FLAG_FIELD]=True
        # Upload in 500-chunks
        for i in range(0,len(batch_docs),500):
            upload(ep,key,batch_docs[i:i+500])
        done_batch=len(batch_docs)
        done+=done_batch
        previously_embedded += done_batch
        last_id=batch_docs[-1].get(ID_FIELD)
        save_progress(last_id, previously_embedded, scanned)
        print(f"Embedded {done}/{total_to_embed if total_to_embed else done} total-in-session; cumulative recorded: {previously_embedded}; last_id={last_id}", flush=True)
        batch_docs.clear(); batch_texts.clear()
        last_flush_time=time.time()

    if streaming_mode:
        print('Streaming embedding mode enabled.', flush=True)
        try:
            for d, scanned in stream_docs(ep,key,select,start_after,scanned, session, args.connect_timeout, args.read_timeout, verify_ssl):
                if not args.force and d.get(FLAG_FIELD):
                    continue
                if not d.get(TEXT_FIELD):
                    d[TEXT_FIELD]=build_text(d)
                batch_docs.append(d)
                batch_texts.append(d[TEXT_FIELD])
                total_to_embed += 1
                flush()  # conditional flush (size/interval governed inside)
                if args.limit and total_to_embed>=args.limit:
                    break
                if time.time()-last_progress_save > PROGRESS_SAVE_INTERVAL:
                    save_progress(d.get(ID_FIELD), previously_embedded, scanned)
                    last_progress_save=time.time()
                    print(f"Progress checkpoint saved at id {d.get(ID_FIELD)} scanned={scanned}", flush=True)
        except KeyboardInterrupt:
            print('\nInterrupted by user; performing final flush...', flush=True)
            flush(force=True)
            print('Graceful interrupt complete.', flush=True)
            return
        flush(force=True)
        print('Completed streaming embedding session.', flush=True)
    else:
        for d, scanned in stream_docs(ep,key,select,start_after,scanned, session, args.connect_timeout, args.read_timeout, verify_ssl):
            if not args.force and d.get(FLAG_FIELD):
                continue
            candidates.append(d)
            if args.limit and len(candidates)>=args.limit:
                break
            if time.time()-last_progress_save > PROGRESS_SAVE_INTERVAL:
                save_progress(d.get(ID_FIELD), previously_embedded, scanned)
                last_progress_save=time.time()
                print(f"Progress checkpoint saved at id {d.get(ID_FIELD)} scanned={scanned}", flush=True)
        print(f"Docs scanned (this session total): {scanned}", flush=True)
        if not args.force:
            print(f"Docs to embed now: {len(candidates)}")
        else:
            print(f"Force mode: embedding {len(candidates)} docs")
        if not candidates:
            print('Nothing to do from current position.', flush=True)
            return
        total_to_embed=len(candidates)
        for d in candidates:
            if not d.get(TEXT_FIELD):
                d[TEXT_FIELD]=build_text(d)
            batch_docs.append(d)
            batch_texts.append(d[TEXT_FIELD])
            if len(batch_docs)>=PAGE:
                flush()
        flush()
        print('Completed embedding batch for this session.', flush=True)
    # If limit was set we may resume later; progress file already updated.

if __name__=='__main__':
    main()
