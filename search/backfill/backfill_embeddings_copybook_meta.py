"""Backfill embeddings + has_vector for copybook meta summary field."""
from __future__ import annotations
import os, sys, json, time, requests, traceback
ROOT=os.path.abspath(os.path.join(os.path.dirname(__file__),'..','..'))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)
from secrets_loader import load_secrets

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_copybook_meta'
# Desired canonical key (new schema). Legacy index may still use copybook_id.
DESIRED_KEY='copybook_name'
KEY_FIELD=DESIRED_KEY  # default reference (kept for backward compatibility with helper functions)
TEXT_FIELD='summary'
VEC_FIELD='summary_vector'
HAS_FIELD='has_vector'
TRUNC=int(os.getenv('COPYBOOK_SUMMARY_TRUNCATE','4000'))
BATCH=int(os.getenv('COPYBOOK_META_BACKFILL_BATCH','64'))
RETRIES=5; SLEEP=0.75
MISSING_FILTER="(has_vector eq false) or (has_vector eq null)"
FALLBACK_TOKEN_PREFIX = "FALLBACK SUMMARY: "

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI=None  # type: ignore
except Exception:
    openai=None; AzureOpenAI=None  # type: ignore


def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def build_embedder():
    if openai is None: raise SystemExit('openai library missing')
    dep=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    ver=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    ae=os.getenv('AZURE_OPENAI_ENDPOINT'); ak=os.getenv('AZURE_OPENAI_KEY')
    if ae and ak and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=ak, api_version=ver, azure_endpoint=ae)
        def _e(texts): r=client.embeddings.create(model=dep, input=texts); return [d.embedding for d in r.data]
        print(f"Azure embeddings deployment={dep}"); return dep,_e
    if ae and ak:
        openai.api_type='azure'; openai.azure_endpoint=ae; openai.api_key=ak; openai.api_version=ver
        def _e(texts): r=openai.embeddings.create(model=dep, input=texts); return [d.embedding for d in r.data]
        print(f"Legacy Azure embeddings deployment={dep}"); return dep,_e
    pk=os.getenv('OPENAI_API_KEY');
    if not pk: raise SystemExit('No embedding credentials')
    openai.api_key=pk
    def _e(texts): r=openai.embeddings.create(model=dep, input=texts); return [d.embedding for d in r.data]
    print(f"Public OpenAI embeddings model={dep}"); return dep,_e

def embed_retry(fn,texts):
    last=None
    for i in range(RETRIES):
        try: return fn(texts)
        except Exception as e:
            last=e; wait=SLEEP*(2**i); print(f"Embed fail {i+1}/{RETRIES}: {e} wait {wait:.2f}s"); time.sleep(wait)
    traceback.print_exception(last); raise last

def detect_remote_key(ep,key):
    """Return the key field name declared in the remote index schema or None."""
    try:
        r=requests.get(f"{ep}/indexes/{INDEX}?api-version={API}", headers={'api-key':key}, timeout=30)
        if r.status_code!=200:
            print('[WARN] Could not retrieve index schema to detect key field:', r.status_code)
            return None
        data=r.json();
        for f in data.get('fields',[]):
            if f.get('key') is True:
                return f.get('name')
    except Exception as e:
        print('[WARN] Exception detecting key field:', e)
    return None

def fetch(ep,key,skip,top, key_field, search_expr='*', filter_expr: str | None = None):
    body={'search':search_expr,'top':top,'skip':skip,'select':f'{key_field},{TEXT_FIELD},{HAS_FIELD}'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200: print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json={'value':docs}, timeout=60)
    if r.status_code not in (200,201): print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

if __name__=='__main__':
    import argparse
    ap=argparse.ArgumentParser(description='Backfill copybook_meta embeddings (idempotent unless overridden).')
    ap.add_argument('--force', action='store_true', help='Re-embed all docs regardless of has_vector flag')
    ap.add_argument('--ids', nargs='*', help='Optional subset of copybook_names to (re)embed (implies --force for those names)')
    ap.add_argument('--ids-file', help='Path to file containing newline-delimited copybook_names to (re)embed (merged with --ids)')
    ap.add_argument('--batch', type=int, default=BATCH, help='Override batch size (default env or 64)')
    ap.add_argument('--stop-after-first', action='store_true', help='Stop once at least one subset id has been updated (diagnostic speed-up)')
    ap.add_argument('--debug', action='store_true', help='Verbose debug logging for action construction')
    ap.add_argument('--auto-key', action='store_true', help='Automatically adopt remote key field if different (legacy support)')
    ap.add_argument('--all', dest='missing_only', action='store_false', help='Process all docs (not just missing)')
    ap.add_argument('--no-fallback-empty', dest='fallback_empty', action='store_false', help='Disable fallback embedding of empty summaries (uses key name)')
    ap.set_defaults(missing_only=True)
    ap.set_defaults(fallback_empty=True)
    args=ap.parse_args()
    load_secrets(); ep,key=resolve_search(); dep, emb=build_embedder()
    # Detect actual remote key
    remote_key=detect_remote_key(ep,key)
    active_key=DESIRED_KEY
    if remote_key and remote_key!=DESIRED_KEY:
        if not args.auto_key:
            print(f"[FATAL] Remote index key field is '{remote_key}' but script expects '{DESIRED_KEY}'.\n" \
                  f"Action required: (a) Recreate index with new schema via search/indexes/create_copybook_meta_index.py --overwrite, then rerun ingestion; or\n" \
                  f"(b) Re-run this script with --auto-key to proceed using legacy key (will embed legacy docs).")
            sys.exit(2)
        else:
            active_key=remote_key
            print(f"[INFO] Adopting remote key field '{active_key}' (legacy mode).")
    force_ids=set([x.upper() for x in (args.ids or [])])
    if args.ids_file:
        try:
            with open(args.ids_file,'r',encoding='utf-8') as f:
                for line in f:
                    line=line.strip()
                    if line:
                        force_ids.add(line.upper())
        except FileNotFoundError:
            print(f"[WARN] ids-file not found: {args.ids_file}")
    count_resp=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json={'search':'*','top':0,'count':True})
    if count_resp.status_code!=200: print('Count failed', count_resp.status_code, count_resp.text[:300]); sys.exit(1)
    total=count_resp.json().get('@odata.count',0); print(f"Total docs={total} batch={args.batch} force={args.force} subset={len(force_ids) if force_ids else 0}")
    skip=0; processed=0; updated=0; start=time.time(); batch_size=args.batch
    search_expr='*'
    if force_ids and len(force_ids)==1:
        # Narrow search expression slightly (best-effort) to reach target sooner; still need to scan if analyzer differs
        search_expr=list(force_ids)[0]
    filter_expr = None
    if args.missing_only and not args.force and not force_ids:
        # Only fetch missing vector docs
        filter_expr = MISSING_FILTER
        # When filtering, we don't rely on total for loop termination; we break when a short page appears
    while True:
        if not filter_expr and skip >= total:
            break
        rows=fetch(ep,key,skip,batch_size, key_field=active_key, search_expr=search_expr, filter_expr=filter_expr)
        if not rows:
            break
        texts=[]; ids=[]
        for r in rows:
            cid=r.get(active_key)
            if not cid:
                if args.debug: print('[WARN] Row missing key field, skipping row snippet=', {k:r.get(k) for k in (active_key,TEXT_FIELD,HAS_FIELD)})
                continue
            txt=(r.get(TEXT_FIELD) or '').strip()
            if not txt:
                if args.fallback_empty:
                    txt = (FALLBACK_TOKEN_PREFIX + cid)[:TRUNC]
                else:
                    continue
            if not args.force and not force_ids and r.get(HAS_FIELD) is True:
                continue
            if force_ids and cid.upper() not in force_ids and not args.force and r.get(HAS_FIELD) is True:
                continue
            texts.append(txt[:TRUNC]); ids.append(cid)
        vecs=[]
        if texts:
            vecs=embed_retry(emb,texts)
        actions=[{'@search.action':'mergeOrUpload',active_key:cid,VEC_FIELD:vec,HAS_FIELD:True} for cid,vec in zip(ids,vecs)]
        if args.debug and actions:
            print('[DEBUG] First action sample:', actions[0])
            # Validate key presence
            for a in actions:
                if active_key not in a or not a[active_key]:
                    print('[ERROR] Constructed action missing key field:', a)
        if actions:
            upload(ep,key,actions); updated+=len(actions)
        processed+=len(rows)
        if not filter_expr:
            skip+=batch_size
            pct=processed/total*100 if total else 100
            print(f"Processed {processed}/{total} ({pct:.2f}%) updated={updated}")
        else:
            print(f"Processed (missing-only) processed={processed} updated={updated}")
        time.sleep(0.05)
        if filter_expr and len(rows) < batch_size:
            break
        if args.stop_after_first and updated>0 and force_ids:
            print('Stopping early after first subset update (per --stop-after-first)')
            break
    dur=time.time()-start
    print(f"Backfill complete updated={updated} in {dur:.1f}s (force={args.force} subset={len(force_ids)})")
