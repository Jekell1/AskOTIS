"""Backfill embeddings for new_cobol_program_deps.dependency_blob into dependency_blob_vector.

Pattern adapted from backfill_embeddings_program_meta.py with simplified field set.

Usage:
  python add_vector_fields_program_deps.py   # first time only
  python backfill_embeddings_program_deps.py --batch 64

Environment keys loaded from local.settings.json Values if present:
  AZURE_SEARCH_ENDPOINT / KEY
  AZURE_OPENAI_ENDPOINT / KEY / AZURE_OPENAI_EMBEDDING_DEPLOYMENT / AZURE_OPENAI_API_VERSION
Fallback to OPENAI_API_KEY + OPENAI_EMBEDDING_MODEL if Azure not configured.
"""
from __future__ import annotations
import os, json, sys, time, requests, traceback

SEARCH_API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_deps'
VECTOR_FIELD = 'dependency_blob_vector'
HAS_FIELD = 'has_vector'
DEFAULT_BATCH = 64
TEXT_TRUNCATE = int(os.getenv('PROGRAM_DEPS_TEXT_TRUNCATE','4000'))
EMBED_RETRIES = 5
EMBED_SLEEP_BASE = 0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:  # noqa: BLE001
        AzureOpenAI = None  # type: ignore
except ImportError:
    openai = None  # type: ignore
    AzureOpenAI = None  # type: ignore


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in [
            'AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY',
            'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT',
            'AZURE_OPENAI_API_VERSION','OPENAI_API_KEY','OPENAI_EMBEDDING_MODEL','OPENAI_API_VERSION'
        ]:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass


def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def build_client():
    if openai is None:
        raise RuntimeError('openai package not installed. pip install openai')
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client = AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(texts):
            resp = client.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        print(f"AzureOpenAI embeddings deployment={deployment} api_version={api_version}")
        return deployment, _embed
    if azure_ep and azure_key:  # legacy style
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(texts):
            resp = openai.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in resp.data]
        print(f"Legacy Azure embeddings deployment={deployment} api_version={api_version}")
        return deployment, _embed
    public_key = os.getenv('OPENAI_API_KEY')
    if not public_key:
        raise RuntimeError('No embedding credentials available.')
    openai.api_key = public_key
    model = deployment
    def _embed(texts):
        resp = openai.embeddings.create(model=model, input=texts)
        return [d.embedding for d in resp.data]
    print(f"Public OpenAI embeddings model={model}")
    return model, _embed


def embed_with_retry(fn, texts):
    last=None
    for i in range(EMBED_RETRIES):
        try:
            return fn(texts)
        except Exception as e:  # noqa: BLE001
            last=e
            wait=EMBED_SLEEP_BASE*(2**i)
            print(f"Embed attempt {i+1}/{EMBED_RETRIES} failed: {e}. Wait {wait:.2f}s")
            time.sleep(wait)
    print('Final embedding failure:'); traceback.print_exception(last); raise last


def fetch_batch(ep,key,skip,top):
    """Fetch a page of docs.

    Note: vector field is not retrievable (retrievable=false) so we don't request it.
    We only need program_id, dependency_blob and has_vector flag.
    """
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}"
    body={'search':'*','top':top,'skip':skip,'select':f'program_id,dependency_blob,{HAS_FIELD}','queryType':'simple'}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:400]); sys.exit(1)
    return r.json().get('value',[])


def upload_vectors(ep,key,docs):
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={SEARCH_API_VERSION}"
    payload={'value':docs}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=payload,timeout=60)
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

def count_has_vector(ep,key):
    body={'search':'*','filter':f'{HAS_FIELD} eq true','top':0,'count':True}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        print('Count(has_vector) failed', r.status_code, r.text[:300]); return None
    return r.json().get('@odata.count',0)

if __name__=='__main__':
    import argparse
    ap=argparse.ArgumentParser(description='Backfill dependency_blob embeddings')
    ap.add_argument('--batch',type=int,default=DEFAULT_BATCH)
    ap.add_argument('--reembed-all',action='store_true',help='Ignore existing has_vector flag')
    args=ap.parse_args()
    load_settings(); ep,key=resolve(); deployment, embed_fn = build_client()
    # Count
    count_resp=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json={'search':'*','top':0,'count':True})
    if count_resp.status_code!=200:
        print('Count failed', count_resp.status_code, count_resp.text[:300]); sys.exit(1)
    total=count_resp.json().get('@odata.count',0)
    print(f"Total docs: {total} batch={args.batch} deployment={deployment}")
    skip=0; processed=0; updated=0; start=time.time()
    while skip < total:
        rows=fetch_batch(ep,key,skip,args.batch)
        if not rows: break
        to_embed=[]; id_order=[]
        for r in rows:
            if not args.reembed_all and r.get(HAS_FIELD):
                continue
            blob=(r.get('dependency_blob') or '').strip()
            if not blob:
                continue
            to_embed.append(blob[:TEXT_TRUNCATE]); id_order.append(r['program_id'])
        vectors=[]
        if to_embed:
            vectors=embed_with_retry(embed_fn,to_embed)
        batch=[]
        for pid, vec in zip(id_order, vectors):
            batch.append({'@search.action':'mergeOrUpload','program_id':pid, VECTOR_FIELD:vec, HAS_FIELD: True})
        if batch:
            upload_vectors(ep,key,batch); updated+=len(batch)
        processed+=len(rows); skip+=args.batch
        pct=(processed/total*100) if total else 100
        print(f"Processed {processed}/{total} ({pct:.2f}%) updated={updated}")
        time.sleep(0.1)
    dur=time.time()-start
    hv=count_has_vector(ep,key)
    pct_vec = (hv/total*100) if (hv is not None and total) else None
    if pct_vec is not None:
        print(f"Embedding backfill complete: {updated} docs in {dur:.1f}s ; vector coverage={pct_vec:.2f}% ({hv}/{total})")
    else:
        print(f"Embedding backfill complete: {updated} docs in {dur:.1f}s (coverage unavailable)")
