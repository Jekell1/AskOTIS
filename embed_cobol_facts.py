#!/usr/bin/env python3
"""Embed cobol-facts documents into fact_vector using fact_text content.

Behavior mirrors embed_flow_edges.py:
  - Optional streaming mode (--stream) for constant progress
  - Resume via progress file (facts_embed_progress.json)
  - Skip already embedded unless --force
  - Builds fact_text dynamically if absent (snippet + other relevant fields)

Usage:
  python embed_cobol_facts.py --stream --batch 64
"""
import os, json, time, argparse, random, requests
from typing import Optional, List, Iterable, Tuple

API = '2024-07-01'
INDEX = 'cobol-facts'
ID_FIELD = 'fact_id'
TEXT_FIELD = 'fact_text'
VECTOR_FIELD = 'fact_vector'
FLAG_FIELD = 'has_vector'
DEFAULT_BATCH = 64
PAGE = 1000
PROGRESS_FILE = 'facts_embed_progress.json'
PROGRESS_SAVE_INTERVAL = 30

FACT_TEXT_PARTS = [
    'snippet','callee','callee_data_name','using_raw','target','source_raw','expr_raw','para'
]

# ------------------ config helpers ------------------

def load_env():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            os.environ.setdefault(k,v)
    except Exception:
        pass
    ep = os.environ.get('SEARCH_ENDPOINT') or os.environ.get('AZURE_SEARCH_ENDPOINT')
    key = os.environ.get('SEARCH_KEY') or os.environ.get('AZURE_SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing SEARCH_ENDPOINT/SEARCH_KEY')
    return ep.rstrip('/'), key

def load_progress(resume: bool):
    if not resume or not os.path.exists(PROGRESS_FILE):
        return None, 0, 0
    try:
        data = json.load(open(PROGRESS_FILE,'r',encoding='utf-8'))
        return data.get('last_id'), data.get('embedded',0), data.get('scanned',0)
    except Exception:
        return None, 0, 0

def save_progress(last_id: Optional[str], embedded: int, scanned: int):
    tmp = PROGRESS_FILE + '.tmp'
    with open(tmp,'w',encoding='utf-8') as f:
        json.dump({'last_id':last_id,'embedded':embedded,'scanned':scanned,'ts':time.time()}, f, indent=2)
    os.replace(tmp, PROGRESS_FILE)

# ------------------ search paging ------------------

def post_with_retry(session: requests.Session, url: str, headers: dict, body: dict, *,
                    max_retries: int = 5, base_delay: float = 1.0,
                    connect_timeout: float = 8.0, read_timeout: float = 45.0,
                    verify_ssl: bool = True):
    for attempt in range(max_retries):
        try:
            r = session.post(url, headers=headers, json=body,
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
        filt=None
        if last_id:
            safe=last_id.replace("'", "''")
            filt=f"{ID_FIELD} gt '{safe}'"
        body={'search':'*','select':select_fields,'top':PAGE}
        if use_order:
            body['orderby']=f'{ID_FIELD} asc'
        if filt:
            body['filter']=filt
        r=post_with_retry(session, f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers, body,
                          connect_timeout=connect_timeout, read_timeout=read_timeout, verify_ssl=verify_ssl)
        if r.status_code==400 and use_order and 'orderby' in r.text.lower():
            use_order=False
            yield from legacy_skip(ep,key,select_fields, scanned_so_far, session, connect_timeout, read_timeout, verify_ssl)
            return
        if r.status_code!=200:
            raise RuntimeError(f"Fetch error {r.status_code}: {r.text[:200]}")
        batch=r.json().get('value',[])
        if not batch: break
        for d in batch:
            scanned_so_far += 1
            yield d, scanned_so_far
        last_id=batch[-1].get(ID_FIELD)
        if len(batch)<PAGE: break

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
        if skip>=100000 or len(batch)<PAGE: break

# ------------------ text building ------------------

def build_text(d: dict) -> str:
    parts=[]
    for p in FACT_TEXT_PARTS:
        v=d.get(p)
        if v:
            parts.append(str(v))
    return ' | '.join(parts)

# ------------------ upload ------------------

def upload(ep: str, key: str, docs: List[dict]):
    if not docs: return
    headers={'api-key':key,'Content-Type':'application/json'}
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API}"
    payload={'value':[{'@search.action':'mergeOrUpload', **d} for d in docs]}
    r=requests.post(url, headers=headers, json=payload, timeout=60)
    if r.status_code not in (200,201):
        raise RuntimeError(f"Upload error {r.status_code}: {r.text[:300]}")

# ------------------ main ------------------

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--force', action='store_true')
    ap.add_argument('--limit', type=int, default=0)
    ap.add_argument('--batch', type=int, default=DEFAULT_BATCH)
    ap.add_argument('--resume', action='store_true')
    ap.add_argument('--reset-progress', action='store_true')
    ap.add_argument('--disable-hf', action='store_true')
    ap.add_argument('--stream', action='store_true')
    ap.add_argument('--flush-size', type=int, default=400)
    ap.add_argument('--flush-interval', type=int, default=90)
    ap.add_argument('--connect-timeout', type=float, default=8.0)
    ap.add_argument('--read-timeout', type=float, default=45.0)
    ap.add_argument('--insecure', action='store_true')
    args=ap.parse_args()

    ep,key=load_env()
    if args.disable_hf:
        os.environ['DISABLE_HF_FALLBACK']='1'
    from embedding_utils import batch_embed, provider_info
    print('Embedding provider:', provider_info(), flush=True)

    select=f"{ID_FIELD},{FLAG_FIELD},{TEXT_FIELD}," + ','.join([p for p in FACT_TEXT_PARTS if p not in (TEXT_FIELD, FLAG_FIELD)])
    if args.reset_progress and os.path.exists(PROGRESS_FILE):
        os.remove(PROGRESS_FILE)
        print('Deleted existing progress file.')

    session=requests.Session()
    verify_ssl=not args.insecure
    if not verify_ssl:
        print('WARNING: SSL verification disabled (--insecure).', flush=True)
    start_after, previously_embedded, previously_scanned = load_progress(args.resume)
    if start_after:
        print(f"Resuming after id={start_after} (embedded recorded: {previously_embedded}, scanned: {previously_scanned})")
    else:
        print('Starting fresh scan.')

    candidates=[]
    scanned=previously_scanned
    last_progress_save=time.time()
    batch_docs=[]; batch_texts=[]; done=0; total_to_embed=0
    streaming_mode=args.stream
    last_flush_time=time.time()

    def flush(force: bool=False):
        nonlocal batch_docs,batch_texts,done,previously_embedded,last_flush_time
        if not batch_docs: return
        if streaming_mode and not force:
            size_trigger = len(batch_docs) >= max(1,args.flush_size)
            interval_trigger = (time.time()-last_flush_time) >= args.flush_interval
            if not (size_trigger or interval_trigger):
                return
        vecs=batch_embed(batch_texts, batch_size=args.batch)
        for doc,vec in zip(batch_docs,vecs):
            if not doc.get(TEXT_FIELD):
                doc[TEXT_FIELD]=build_text(doc)
            doc[VECTOR_FIELD]=vec
            doc[FLAG_FIELD]=True
        for i in range(0,len(batch_docs),500):
            upload(ep,key,batch_docs[i:i+500])
        done_batch=len(batch_docs)
        done+=done_batch
        previously_embedded+=done_batch
        last_id=batch_docs[-1].get(ID_FIELD)
        save_progress(last_id, previously_embedded, scanned)
        print(f"Embedded {done}/{total_to_embed if total_to_embed else done} session; cumulative {previously_embedded}; last_id={last_id}", flush=True)
        batch_docs.clear(); batch_texts.clear()
        last_flush_time=time.time()

    if streaming_mode:
        print('Streaming embedding mode enabled.')
        try:
            for d, scanned in stream_docs(ep,key,select,start_after,scanned, session, args.connect_timeout, args.read_timeout, verify_ssl):
                if not args.force and d.get(FLAG_FIELD):
                    continue
                if not d.get(TEXT_FIELD):
                    d[TEXT_FIELD]=build_text(d)
                batch_docs.append(d)
                batch_texts.append(d[TEXT_FIELD])
                total_to_embed+=1
                flush()
                if args.limit and total_to_embed>=args.limit:
                    break
                if time.time()-last_progress_save > PROGRESS_SAVE_INTERVAL:
                    save_progress(d.get(ID_FIELD), previously_embedded, scanned)
                    last_progress_save=time.time()
                    print(f"Progress checkpoint saved id={d.get(ID_FIELD)} scanned={scanned}", flush=True)
        except KeyboardInterrupt:
            print('\nInterrupted; performing final flush...', flush=True)
            flush(force=True)
            print('Graceful interrupt complete.', flush=True)
            return
        flush(force=True)
        print('Completed streaming embedding session.')
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
                print(f"Progress checkpoint saved id={d.get(ID_FIELD)} scanned={scanned}", flush=True)
        if not candidates:
            print('Nothing to embed.')
            return
        total_to_embed=len(candidates)
        for d in candidates:
            if not d.get(TEXT_FIELD):
                d[TEXT_FIELD]=build_text(d)
            batch_docs.append(d)
            batch_texts.append(d[TEXT_FIELD])
            if len(batch_docs)>=PAGE:
                flush()
        flush(force=True)
        print('Completed batch embedding session.')

if __name__=='__main__':
    main()
