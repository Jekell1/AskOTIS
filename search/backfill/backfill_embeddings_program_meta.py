"""Backfill embeddings + has_vector flag for program summaries (program_meta)."""
from __future__ import annotations
import os, sys, json, time, requests, traceback

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_meta'
TEXT_FIELD='program_summary'
VEC_FIELD='program_summary_vector'
HAS_FIELD='has_vector'
TRUNC=int(os.getenv('PROGRAM_META_SUMMARY_TRUNCATE','4000'))
BATCH=int(os.getenv('PROGRAM_META_BACKFILL_BATCH','64'))
RETRIES=5
SLEEP=0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI=None  # type: ignore
except Exception:
    openai=None; AzureOpenAI=None  # type: ignore

def load():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT','OPENAI_API_KEY','OPENAI_EMBEDDING_MODEL','AZURE_OPENAI_API_VERSION','OPENAI_API_VERSION']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key

def build_embedder():
    if openai is None: raise SystemExit('openai lib missing')
    dep=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    ver=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    ae=os.getenv('AZURE_OPENAI_ENDPOINT'); ak=os.getenv('AZURE_OPENAI_KEY')
    if ae and ak and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=ak, api_version=ver, azure_endpoint=ae)
        def _e(texts):
            r=client.embeddings.create(model=dep, input=texts)
            return [d.embedding for d in r.data]
        print(f"Azure embeddings deployment={dep}"); return dep,_e
    if ae and ak:
        openai.api_type='azure'; openai.azure_endpoint=ae; openai.api_key=ak; openai.api_version=ver
        def _e(texts):
            r=openai.embeddings.create(model=dep, input=texts)
            return [d.embedding for d in r.data]
        print(f"Legacy Azure embeddings deployment={dep}"); return dep,_e
    pk=os.getenv('OPENAI_API_KEY')
    if not pk: raise SystemExit('No embedding credentials')
    openai.api_key=pk
    def _e(texts):
        r=openai.embeddings.create(model=dep, input=texts)
        return [d.embedding for d in r.data]
    print(f"Public OpenAI embeddings model={dep}"); return dep,_e

def embed_retry(fn,texts):
    last=None
    for i in range(RETRIES):
        try: return fn(texts)
        except Exception as e:
            last=e; wait=SLEEP*(2**i); print(f"Embed fail {i+1}/{RETRIES}: {e} waiting {wait:.2f}s"); time.sleep(wait)
    traceback.print_exception(last); raise last

def fetch(ep,key,skip,top):
    body={'search':'*','top':top,'skip':skip,'select':f'program_id,{TEXT_FIELD},{HAS_FIELD}'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200: print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,docs):
    if not docs: return
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json={'value':docs}, timeout=60)
    if r.status_code not in (200,201): print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

if __name__=='__main__':
    import argparse
    ap=argparse.ArgumentParser(description='Backfill program_meta embeddings (idempotent unless --force).')
    ap.add_argument('--force', action='store_true', help='Re-embed all docs regardless of has_vector flag')
    ap.add_argument('--ids', nargs='*', help='Optional subset of program_ids to (re)embed (implies --force for those IDs)')
    args=ap.parse_args()
    load(); ep,key=resolve_search(); dep, emb=build_embedder()
    force_ids=set([x.upper() for x in (args.ids or [])])
    count_resp=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json={'search':'*','top':0,'count':True})
    if count_resp.status_code!=200: print('Count failed', count_resp.status_code, count_resp.text[:300]); sys.exit(1)
    total=count_resp.json().get('@odata.count',0); print(f"Total docs={total} batch={BATCH} force={args.force} subset={len(force_ids) if force_ids else 0}")
    skip=0; processed=0; updated=0; start=time.time()
    while skip<total:
        rows=fetch(ep,key,skip,BATCH)
        if not rows: break
        texts=[]; ids=[]
        for r in rows:
            pid=r['program_id']
            txt=(r.get(TEXT_FIELD) or '').strip()
            if not txt: continue
            if not args.force and not force_ids and r.get(HAS_FIELD) is True: continue
            if force_ids and pid.upper() not in force_ids and not args.force and r.get(HAS_FIELD) is True: continue
            texts.append(txt[:TRUNC]); ids.append(pid)
        vectors=[]
        if texts: vectors=embed_retry(emb,texts)
        actions=[{'@search.action':'mergeOrUpload','program_id':pid,VEC_FIELD:vec,HAS_FIELD:True} for pid,vec in zip(ids,vectors)]
        if actions: upload(ep,key,actions); updated+=len(actions)
        processed+=len(rows); skip+=BATCH; pct=processed/total*100 if total else 100
        print(f"Processed {processed}/{total} ({pct:.2f}%) updated={updated}")
        time.sleep(0.05)
    dur=time.time()-start
    print(f"Backfill complete updated={updated} in {dur:.1f}s (force={args.force} subset={len(force_ids)})")
