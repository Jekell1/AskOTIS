"""Backfill embeddings for symbol reference excerpts.

Adds vectors to documents lacking `has_vector` (excerpt_vector) in new_cobol_symbol_refs.

New flags (parity with program/copybook meta backfills):
    --force          Re-embed ALL docs (ignores has_vector flag)
    --ids A B C      Only process the specified ref_id values (still respects --force if combined)

Usage Examples:
    python backfill_symbol_ref_embeddings.py --batch 256 --limit 50000
    python backfill_symbol_ref_embeddings.py --ids RP001 RP002
    python backfill_symbol_ref_embeddings.py --force --ids RP001 RP002   # force just those ids
    python backfill_symbol_ref_embeddings.py --force                     # force entire corpus

Sharding / Parallel Example (8 shards):
    # In 8 terminals (or background jobs) run indices 0..7
    python backfill_symbol_ref_embeddings.py --hex-shards 8 --hex-shard-index 0 --embed-batch 128
    ... (repeat with shard-index 1..7)

Early stop / Validation:
    python backfill_symbol_ref_embeddings.py --stop-percent 100 --post-validate
"""
from __future__ import annotations
import os, json, argparse, time, requests, math, pathlib, sys, string
from embedding_utils import batch_embed

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_symbol_refs'
VECTOR_FIELD='excerpt_vector'
HAS_FIELD='has_vector'
SYMBOL_REF_VECTOR_DIM = int(os.getenv('SYMBOL_REF_VECTOR_DIM','3072'))  # set to 1536 before recreation to shrink vectors

def fetch_index_vector_dim(ep,key):
    try:
        r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API_VERSION}",headers={'api-key':key},timeout=30)
        if r.status_code==200:
            data=r.json()
            for f in data.get('fields',[]):
                if f.get('name')==VECTOR_FIELD:
                    return f.get('dimensions')
    except Exception:
        return None
    return None

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def count_docs(ep,key,flt=None):
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    body={'search':'*','top':0,'count':True}
    if flt: body['filter']=flt
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=120)
    if r.status_code!=200:
        raise SystemExit(f"Count failed {r.status_code}: {r.text[:200]}")
    return r.json().get('@odata.count',0)

def load_checkpoint(path:str):
    p=pathlib.Path(path)
    if not p.is_file():
        return None
    try:
        return json.loads(p.read_text(encoding='utf-8'))
    except Exception:
        return None

def save_checkpoint(path:str, data:dict):
    try:
        pathlib.Path(path).write_text(json.dumps(data,indent=2),encoding='utf-8')
    except Exception as e:
        print(f"[WARN] Failed to write checkpoint: {e}")

def fetch_batch(ep,key,skip,top, force=False, subset_ids=None, hex_prefixes=None):
    """Fetch a page of symbol ref docs.

    force: if True, ignore has_vector filter (retrieve all docs)
    subset_ids: optional set of ref_id values to restrict results (paged scan still used)
    """
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    # Build filter expression
    filters=[]
    if not force:
        filters.append(f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))")
    if subset_ids:
        # For small subsets we can OR them; if large, still acceptable because typical targeted usage
        or_list=' or '.join([f"ref_id eq '{rid}'" for rid in sorted(subset_ids)])
        if or_list:
            filters.append(f"({or_list})")
    if hex_prefixes:
        spans=[]
        for p in hex_prefixes:
            idx='0123456789abcdef'.index(p)
            next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
            spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
        if spans:
            filters.append('(' + ' or '.join(spans) + ')')
    filter_expr=' and '.join(filters) if filters else None
    # NOTE: 'excerpt' legacy field removed from index; only select existing fields.
    # Provide deterministic ordering to avoid cycling duplicates. Azure Search expects 'orderby' lowercase.
    body={'search':'*', 'top':top,'skip':skip,'select':'ref_id,excerpt_text','orderby':'ref_id asc'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Fetch failed {r.status_code}: {r.text[:300]}")
    vals=r.json().get('value',[])
    # If subset_ids provided and force True (no filter on has_vector) we still may fetch refs outside subset; trim
    if subset_ids:
        vals=[v for v in vals if v.get('ref_id') in subset_ids]
    return vals

def fetch_batch_keyset(ep,key,last_ref_id,top,force=False,subset_ids=None,hex_prefixes=None):
    """Keyset pagination fetch (ordered by ref_id asc) avoiding large skip costs.

    last_ref_id: exclusive lower bound; None for first page.
    hex_prefixes: optional iterable of starting characters to restrict shards.
    """
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    filters=[]
    if not force:
        filters.append(f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))")
    if subset_ids:
        or_list=' or '.join([f"ref_id eq '{rid}'" for rid in sorted(subset_ids)])
        if or_list:
            filters.append(f"({or_list})")
    if hex_prefixes:
        # use startswith for each prefix
        # Azure Search $filter lacks startswith for collections; emulate with search.ismatchscoring prefix queries in filter via OData function
        # However search.ismatchscoring only allowed in $filter in preview versions; fallback to broad filter using pattern match on ref_id starting letter
        # Approach: (ref_id ge 'p' and ref_id lt 'next_char') unioned. Build OR spans.
        spans=[]
        for p in hex_prefixes:
            idx='0123456789abcdef'.index(p)
            next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
            # range: p* <= ref_id < next_char (lexicographically); use 'p' and next char
            spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
        shard_or=' or '.join(spans)
        filters.append(f"({shard_or})")
    if last_ref_id:
        filters.append(f"ref_id gt '{last_ref_id}'")
    filter_expr=' and '.join(filters) if filters else None
    # Azure Search preview payload rejected orderBy inside search body; instead rely on scoring ordering not guaranteed.
    # Fallback: we cannot truly keyset without orderBy; as mitigation, keep ref_id gt filter and accept potential out-of-order segments.
    # Keyset pagination select list aligned with current schema (no legacy 'excerpt').
    # Use deterministic ordering; combined with ref_id gt filter this simulates keyset pagination.
    body={'search':'*','top':top,'select':'ref_id,excerpt_text','orderby':'ref_id asc'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        raise SystemExit(f"Keyset fetch failed {r.status_code}: {r.text[:300]}")
    vals=r.json().get('value',[])
    # Subset id trim if both subset and force
    if subset_ids:
        vals=[v for v in vals if v.get('ref_id') in subset_ids]
    return vals

def fetch_by_ids_direct(ep,key, ids):
    """Fetch exact docs for provided ref_id values (used when --ids supplied to avoid full scans).

    We chunk ids into manageable OR filters. Returns list of docs (ref_id, excerpt).
    """
    if not ids: return []
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    out=[]; id_list=list(ids)
    CHUNK=32
    for i in range(0,len(id_list),CHUNK):
        chunk=id_list[i:i+CHUNK]
        or_list=' or '.join([f"ref_id eq '{rid}'" for rid in chunk])
        body={'search':'*','top':len(chunk),'filter':f"({or_list})",'select':'ref_id,excerpt_text'}
        r=requests.post(url,headers=headers,json=body,timeout=60)
        if r.status_code!=200:
            raise SystemExit(f"ID fetch failed {r.status_code}: {r.text[:200]}")
        out.extend(r.json().get('value',[]))
    return out

def upload(ep,key,docs):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    MAX_BYTES=2*1024*1024
    MAX_DOCS=256
    batch=[]; size_est=0

    def send_batch(batch_list, attempt=1):
        if not batch_list: return
        for d in batch_list:
            d['@search.action']='merge'
        resp=requests.post(url,headers=headers,json={'value':batch_list})
        code=resp.status_code
        # Hard quota exhaustion (vector index size) => stop cleanly with guidance.
        if code==429:
            try:
                body=resp.json()
                msg=(body.get('error') or {}).get('message','')
            except Exception:
                msg=resp.text[:300]
            if 'Vector index size quota has been exceeded' in msg:
                guidance=(
                    "\n[QUOTA] Vector index size quota exceeded. Further uploads aborted.\n"
                    "Actions you can take:\n"
                    "  1. Scale OUT partitions (adds vector quota).\n"
                    "  2. Recreate service (if pre-Apr 2024) to obtain higher per-partition limits.\n"
                    "  3. Reduce embedding dimensionality (e.g., 1536) and recreate the index.\n"
                    "  4. Switch some workloads to exhaustive KNN (no in-memory vector quota) where acceptable.\n"
                    "  5. Split references across multiple indexes (shard by first hex).\n"
                    "  6. Consider vector compression / quantization options to shrink memory.\n"
                    "After mitigation, re-run the backfill (it will resume remaining docs).\n"
                )
                print(guidance)
                # Write a marker file so automation can detect termination cause.
                try:
                    pathlib.Path('.vector_quota_exceeded').write_text(json.dumps({'at':time.time(),'message':msg,'docs_in_batch':len(batch_list)},indent=2),encoding='utf-8')
                except Exception:
                    pass
                raise SystemExit('[FATAL] Vector quota exceeded (429).')
        if code in (200,201):
            return
        if code==207:  # multi-status, inspect individual results
            body=resp.json()
            failed=[]; conflicts=0
            for item in body.get('value',[]):
                if not item.get('status'):
                    msg=item.get('errorMessage','') or ''
                    # Treat version conflict as benign (another shard updated first)
                    if 'version conflict' in msg.lower():
                        conflicts+=1
                        continue
                    failed.append(item.get('key'))
            if failed:
                if attempt>=3:
                    raise SystemExit(f"Upload failed after retries; keys={failed[:5]}... (total {len(failed)})")
                # Retry failed keys individually (mergeOrUpload to be safe)
                retry_docs=[next(d for d in batch_list if d.get('ref_id')==k) for k in failed if any(d.get('ref_id')==k for d in batch_list)]
                for rd in retry_docs:
                    rd['@search.action']='mergeOrUpload'
                    r2=requests.post(url,headers=headers,json={'value':[rd]})
                    if r2.status_code not in (200,201):
                        # One more attempt if 207 with benign conflict
                        if r2.status_code==207:
                            continue
                        raise SystemExit(f"Retry upload failed {r2.status_code}: {r2.text[:180]}")
            return
        if code==413 and len(batch_list)>1:
            # Split batch recursively
            mid=len(batch_list)//2
            send_batch(batch_list[:mid],attempt)
            send_batch(batch_list[mid:],attempt)
            return
        # Other failure: retry a couple of times with backoff
        if attempt<3:
            time.sleep(0.5*attempt)
            send_batch(batch_list,attempt+1)
            return
        raise SystemExit(f"Upload failed {code}: {resp.text[:300]}")

    for doc in docs:
        vec=doc.get(VECTOR_FIELD)
        vec_bytes=len(vec)*4 if isinstance(vec,list) else 0
        doc_est=vec_bytes + len(str(doc.get('ref_id',''))) + 128
        if batch and (len(batch)>=MAX_DOCS or size_est+doc_est>MAX_BYTES):
            send_batch(batch); batch=[]; size_est=0
        batch.append(doc); size_est+=doc_est
    send_batch(batch)

def main():
    ap=argparse.ArgumentParser(description='Backfill symbol reference excerpt embeddings (resumable)')
    ap.add_argument('--batch',type=int,default=256,help='Scan page size (when not using direct id chunking)')
    ap.add_argument('--limit',type=int,default=0,help='Maximum docs to (re)embed (0 = unlimited)')
    ap.add_argument('--embed-batch',type=int,default=64,help='Embedding service batch size')
    ap.add_argument('--force',action='store_true',help='Re-embed all docs regardless of existing vector')
    ap.add_argument('--ids',nargs='*',help='Specific ref_id values to (re)embed')
    ap.add_argument('--checkpoint-file',default='.symbol_ref_backfill_checkpoint.json',help='Path to checkpoint JSON file')
    ap.add_argument('--reset-checkpoint',action='store_true',help='Ignore existing checkpoint and start fresh baseline')
    ap.add_argument('--recount-every-batches',type=int,default=50,help='Recount remaining missing docs every N batches for ETA refinement (0=never)')
    ap.add_argument('--keyset',action='store_true',help='Use keyset pagination (ref_id asc) instead of skip/scan for scalability')
    ap.add_argument('--hex-shards',type=int,default=0,help='Shard count (1-16) based on first hex char of ref_id')
    ap.add_argument('--hex-shard-index',type=int,default=0,help='Shard index (0-based) when using --hex-shards')
    ap.add_argument('--text-max-len',type=int,default=512,help='Max characters of excerpt_text to embed (truncate)')
    ap.add_argument('--grow-batch-after',type=int,default=5,help='Number of successful batches before growing scan batch (0=disable)')
    ap.add_argument('--grow-factor',type=float,default=2.0,help='Multiplier when growing scan batch')
    ap.add_argument('--max-batch',type=int,default=2048,help='Upper bound for grown scan batch size')
    ap.add_argument('--snapshot-every-batches',type=int,default=10,help='Write a progress snapshot JSON every N batches (0=disable)')
    ap.add_argument('--snapshot-file',default='.symbol_ref_backfill_progress.json',help='Progress snapshot file path')
    ap.add_argument('--stop-percent',type=float,default=100.0,help='Stop when this percent of baseline is embedded (<=100).')
    ap.add_argument('--post-validate',action='store_true',help='Run validate_recent_uploads.py after completion')
    ap.add_argument('--no-local-decrement',action='store_true',help='Do not locally decrement remaining; rely solely on recounts')
    ap.add_argument('--auto-keyset-after-skip',type=int,default=95000,help='Automatically switch to keyset pagination when skip reaches this value (0=disable)')
    # Option A: internal windowed pagination across first hex char segments to avoid large $skip values
    ap.add_argument('--windowed-scan-chars',action='store_true',help='Process each first-hex-character window sequentially (implicit internal 16-way window) to keep per-window skips <100k (force mode). Can combine with --hex-shards to window only that shard range.')
    # Option C: distinct ref_id tracking for reliable completion in force+keyset mode
    ap.add_argument('--track-ids',action='store_true',help='Track distinct ref_id values (memory heavy ~80MB per 1M docs) to ensure full coverage in force mode and avoid premature short-batch termination.')
    # Safeguard: abort a window if no new distinct ids embedded for N consecutive batches (only when tracking ids)
    ap.add_argument('--no-progress-exit-batches',type=int,default=200,help='When --track-ids is enabled, exit current window if this many consecutive batches add zero new embeddings (0=disable). Prevents infinite plateau re-scanning the same doc set.')
    args=ap.parse_args(); load(); ep,key=resolve()

    subset_ids=set(args.ids) if args.ids else None
    start_time=time.time(); updated=0; scanned=0

    # Determine shard allocation (hex-based on first character). Only activate when 1<hex-shards<=16.
    HEX_CHARS=list('0123456789abcdef')
    hex_prefixes=None
    if args.hex_shards:
        if not (1 <= args.hex_shards <= 16):
            print('[FATAL] --hex-shards must be between 1 and 16'); sys.exit(1)
        if not (0 <= args.hex_shard_index < args.hex_shards):
            print('[FATAL] --hex-shard-index must be within shard count'); sys.exit(1)
        span=math.ceil(len(HEX_CHARS)/args.hex_shards)
        shard_start=args.hex_shard_index*span
        shard_end=min(shard_start+span,len(HEX_CHARS))
        hex_prefixes=HEX_CHARS[shard_start:shard_end]
        print(f"[SHARD] hex shard {args.hex_shard_index+1}/{args.hex_shards} prefixes={''.join(hex_prefixes)}")

    # Establish baseline & resume state
    checkpoint=None
    if not args.reset_checkpoint and not args.force and not subset_ids:
        checkpoint=load_checkpoint(args.checkpoint_file)
    current_missing = None
    if subset_ids:
        initial_missing_total = len(subset_ids)
        embedded_so_far = 0
    elif args.force:
        # Force mode treats every doc as target
        flt=None
        if hex_prefixes:
            spans=[]
            for p in hex_prefixes:
                idx='0123456789abcdef'.index(p)
                next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
                spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
            shard_or=' or '.join(spans)
            flt=f"({shard_or})"
        initial_missing_total = count_docs(ep,key,flt)
        embedded_so_far = 0
    else:
        # Non-force, resume supported
        base_filter=f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))"
        if hex_prefixes:
            spans=[]
            for p in hex_prefixes:
                idx='0123456789abcdef'.index(p)
                next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
                spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
            shard_or=' or '.join(spans)
            base_filter=f"{base_filter} and ({shard_or})"
        current_missing = count_docs(ep,key,base_filter)
        if checkpoint and 'initial_missing_total' in checkpoint:
            initial_missing_total = checkpoint['initial_missing_total']
            prev_missing = checkpoint.get('last_remaining', initial_missing_total - checkpoint.get('embedded_so_far',0))
            # Recompute embedded_so_far relative to baseline using fresh missing count
            embedded_so_far = initial_missing_total - current_missing
        else:
            initial_missing_total = current_missing
            embedded_so_far = 0
    if initial_missing_total == 0:
        print('Nothing to embed (no missing vectors).'); return
    # Adjust baseline if a shard run mistakenly loaded a full-corpus checkpoint
    if hex_prefixes and not args.force and not subset_ids:
        spans=[]
        for p in hex_prefixes:
            idx='0123456789abcdef'.index(p)
            next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
            spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
        if spans:
            shard_filter=f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null)) and (" + ' or '.join(spans) + ")"
            shard_missing=count_docs(ep,key,shard_filter)
            if initial_missing_total > shard_missing * 1.5 and shard_missing>0:
                print(f"[ADJUST] Baseline {initial_missing_total} -> shard {shard_missing}")
                initial_missing_total=shard_missing
                embedded_so_far = 0  # recompute relative to shard baseline
    # Internal windowing adjustment (Option A)
    window_char_groups=None
    if args.windowed_scan_chars:
        # Build ordered list of first-hex chars to iterate. Restrict to shard prefixes if present.
        all_chars=list('0123456789abcdef')
        active_chars=hex_prefixes if hex_prefixes else all_chars
        # Sort to stable order
        window_char_groups=[[c] for c in active_chars]
        # Recompute baseline for windows (force mode or missing-vector mode) to use sum across windows to avoid skew
        if args.force and not subset_ids:
            total=0
            for grp in window_char_groups:
                spans=[]
                for p in grp:
                    idx='0123456789abcdef'.index(p)
                    next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
                    spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
                shard_or=' or '.join(spans)
                flt=f"({shard_or})"
                total+=count_docs(ep,key,flt)
            initial_missing_total=total
            embedded_so_far=0
        # Non-force path: baseline derived from missing counts only
        if (not args.force) and not subset_ids:
            total=0
            for grp in window_char_groups:
                spans=[]
                for p in grp:
                    idx='0123456789abcdef'.index(p)
                    next_char='g' if p=='f' else '0123456789abcdef'[idx+1]
                    spans.append(f"(ref_id ge '{p}' and ref_id lt '{next_char}')")
                shard_or=' or '.join(spans)
                flt=f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null)) and ({shard_or})"
                total+=count_docs(ep,key,flt)
            initial_missing_total=total
            embedded_so_far=initial_missing_total - total  # zero
    # Provide initial info (updated after optional window recomputation)
    pct = (embedded_so_far/initial_missing_total*100) if initial_missing_total else 0.0
    print(f"[INIT] baseline_missing={initial_missing_total} already_embedded={embedded_so_far} ({pct:.4f}%) force={args.force} subset={len(subset_ids) if subset_ids else 0} keyset={args.keyset} windows={'on' if args.windowed_scan_chars else 'off'}")
    starting_embedded=embedded_so_far
    # Distinct id tracking (Option C)
    seen_ids=set() if (args.track_ids and args.force and not subset_ids) else None
    # Recreate helper writers (previously earlier in original script)
    def update_checkpoint(last_remaining:int):
        if subset_ids: return
        data={
            'initial_missing_total': initial_missing_total,
            'embedded_so_far': embedded_so_far,
            'last_remaining': last_remaining,
            'updated_at': time.time(),
            'force': args.force,
            'batch': args.batch,
            'embed_batch': args.embed_batch
        }
        try:
            save_checkpoint(args.checkpoint_file,data)
        except Exception as e:
            print(f"[WARN] checkpoint write failed: {e}")
    def write_snapshot_safe(batch_count:int, remaining:int, rate:float, eta:float):
        if args.snapshot_every_batches<=0: return
        if batch_count % args.snapshot_every_batches!=0: return
        snap={
            'embedded_so_far': embedded_so_far,
            'baseline_missing': initial_missing_total,
            'percent': round(embedded_so_far/initial_missing_total*100,6) if initial_missing_total else 0.0,
            'remaining_estimate': remaining,
            'rate_docs_per_sec': rate,
            'eta_minutes': eta/60 if eta else None,
            'elapsed_seconds': time.time()-start_time,
            'batches_processed': batch_count,
            'timestamp': time.time()
        }
        try:
            pathlib.Path(args.snapshot_file).write_text(json.dumps(snap,indent=2),encoding='utf-8')
        except Exception as e:
            print(f"[WARN] snapshot write failed: {e}")
    # Helper to process a single window (list of hex prefixes) returning embedded count increment
    def process_window(active_hex_prefixes):
        nonlocal embedded_so_far, scanned, starting_embedded, seen_ids
        skip=0; last_ref=None; consecutive_empty=0; batch_counter=0; scan_batch=args.batch
        no_progress_batches=0; prev_embedded_window=embedded_so_far
        window_start_embedded=embedded_so_far
        while True:
            if args.limit and embedded_so_far>=args.limit: break
            if args.keyset and not subset_ids:
                rows=fetch_batch_keyset(ep,key,last_ref,scan_batch,force=args.force,subset_ids=subset_ids,hex_prefixes=active_hex_prefixes)
            else:
                rows=fetch_batch(ep,key, skip, scan_batch, force=args.force, subset_ids=subset_ids, hex_prefixes=active_hex_prefixes)
            if not rows:
                consecutive_empty+=1
                if args.keyset and consecutive_empty>=2:
                    print('[FALLBACK] Keyset yielded consecutive empty batches; switching to skip mode for remainder.')
                    args.keyset=False
                    last_ref=None
                    continue
                if consecutive_empty>=3:
                    break
                time.sleep(0.5)
                continue
            consecutive_empty=0
            if args.keyset and not subset_ids:
                last_ref=rows[-1].get('ref_id')
            else:
                if args.force or subset_ids:
                    skip+=len(rows)
                    if (not args.keyset) and args.auto_keyset_after_skip>0 and skip>=args.auto_keyset_after_skip:
                        args.keyset=True
                        last_ref=rows[-1].get('ref_id')
                        skip=0
                        print(f"[ADAPT] Switch to keyset pagination after skip={args.auto_keyset_after_skip} threshold to avoid $skip limit.")
                else:
                    skip=0
            if args.limit and (embedded_so_far+len(rows))>args.limit:
                rows=rows[:args.limit-embedded_so_far]
            if args.text_max_len and args.text_max_len > 0:
                texts=[(r.get('excerpt_text') or r.get('excerpt') or '')[:args.text_max_len] for r in rows]
            else:
                texts=[(r.get('excerpt_text') or r.get('excerpt') or '') for r in rows]
            vecs=batch_embed(texts,batch_size=args.embed_batch,target_dim=SYMBOL_REF_VECTOR_DIM)
            payload=[]; new_rows=0
            for r,v in zip(rows,vecs):
                rid=r['ref_id']
                if seen_ids is not None:
                    if rid in seen_ids:
                        continue
                    seen_ids.add(rid)
                payload.append({'ref_id':rid,VECTOR_FIELD:v,HAS_FIELD:True})
                new_rows+=1
            upload(ep,key,payload)
            scanned+=len(rows)
            embedded_so_far += new_rows
            if seen_ids is not None:
                if new_rows==0:
                    no_progress_batches+=1
                else:
                    no_progress_batches=0
            batch_counter+=1
            current_batch_cap=scan_batch
            elapsed=time.time()-start_time
            pct = embedded_so_far/initial_missing_total*100 if initial_missing_total else 0.0
            run_added = embedded_so_far - starting_embedded
            rate = run_added/elapsed if elapsed>0 else 0
            eta = (initial_missing_total-embedded_so_far)/rate if rate>0 else 0
            remaining_est = max(initial_missing_total - embedded_so_far,0)
            extra=f" uniq={len(seen_ids)}" if seen_ids is not None else ''
            print(f"Progress scanned={scanned} embedded={embedded_so_far}/{initial_missing_total} ({pct:.4f}%) remaining={remaining_est} elapsed={elapsed:.1f}s run_added={run_added} rate_run={rate:.1f}/s ETA={eta/60:.1f}m{extra}",flush=True)
            update_checkpoint(remaining_est)
            write_snapshot_safe(batch_counter, remaining_est, rate, eta)
            if pct >= args.stop_percent:
                print(f"[STOP] Reached stop-percent threshold {args.stop_percent}% (current={pct:.4f}%). Ending early.")
                break
            if seen_ids is not None and len(seen_ids)>=initial_missing_total:
                print('[DONE] All distinct ref_id values processed (tracked).')
                break
            if seen_ids is not None and args.no_progress_exit_batches>0 and no_progress_batches>=args.no_progress_exit_batches:
                print(f"[PLATEAU] No new distinct ids for {no_progress_batches} consecutive batches in this window; moving on.")
                break
            if not args.force and not subset_ids and remaining_est==0:
                break
            if (args.force or subset_ids) and seen_ids is None and len(rows)<current_batch_cap:
                break
            if args.grow_batch_after>0 and batch_counter==args.grow_batch_after:
                new_size=min(int(scan_batch*args.grow_factor), args.max_batch)
                if new_size>scan_batch:
                    print(f"[GROW] Increasing scan batch {scan_batch} -> {new_size}")
                    scan_batch=new_size
        return embedded_so_far - window_start_embedded

    if not subset_ids:
        if window_char_groups:
            total_windows=len(window_char_groups)
            for idx,grp in enumerate(window_char_groups, start=1):
                print(f"[WINDOW] Processing window {idx}/{total_windows} chars={''.join(grp)}")
                process_window(grp)
                if seen_ids is not None and len(seen_ids)>=initial_missing_total:
                    break
        else:
            process_window(hex_prefixes)

    final_missing = 0 if args.force else count_docs(ep,key,f"(({HAS_FIELD} eq false) or ({HAS_FIELD} eq null))")
    uniq_note=f" uniq={len(seen_ids)}" if seen_ids is not None else ''
    dur=time.time()-start_time
    print(f"Backfill complete embedded={embedded_so_far} baseline={initial_missing_total} coverage={(embedded_so_far/initial_missing_total*100):.4f}% remaining={final_missing} force={args.force} subset={(len(subset_ids) if subset_ids else 0)} windows={'on' if args.windowed_scan_chars else 'off'}{uniq_note} in {dur:.1f}s")
    try:
        update_checkpoint(final_missing)
    except Exception:
        pass
    if args.post_validate:
        try:
            print('[POST] Running validation script...')
            os.system('python validate_recent_uploads.py --json')
        except Exception as e:
            print(f"[WARN] post-validate failed: {e}")

if __name__=='__main__':
    # Pre-flight: ensure index dimension matches env expectation to avoid silent mismatch.
    load()
    try:
        ep,key=resolve()
        idx_dim=fetch_index_vector_dim(ep,key)
        if idx_dim and idx_dim!=SYMBOL_REF_VECTOR_DIM:
            print(f"[WARN] Index vector dim {idx_dim} != env SYMBOL_REF_VECTOR_DIM {SYMBOL_REF_VECTOR_DIM}. Set env or recreate index.")
    except SystemExit:
        pass
    # Defer to main (defined above)
    main()
