"""Force re-embed ALL docs in new_cobol_program_deps.

Why a separate script? Clarity + safety. The existing backfill script supports --reembed-all,
but this dedicated tool:
  * Always re-embeds every document (unless --limit provided)
  * Provides --dry-run to show counts before spending tokens
  * Supports --limit N for a quick smoke test
  * Logs progress + total token estimate (rough heuristic)

Usage examples:
  python reembed_all_program_deps.py                # full re-embed
  python reembed_all_program_deps.py --batch 64     # control batch size
  python reembed_all_program_deps.py --limit 25     # test first 25 docs
  python reembed_all_program_deps.py --dry-run      # just show stats

Environment: loads local.settings.json Values for credentials.
Embedding model selection mirrors backfill_embeddings_program_deps.py.
"""
from __future__ import annotations
import os, json, sys, time, requests, traceback, argparse

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
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-large'
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


def estimate_tokens(texts):
    # Very rough heuristic: ~1 token ~= 4 chars
    chars=sum(len(t) for t in texts)
    return int(chars/4)


def main():
    ap=argparse.ArgumentParser(description='Force re-embed all dependency docs')
    ap.add_argument('--batch',type=int,default=DEFAULT_BATCH)
    ap.add_argument('--limit',type=int,default=0,help='Only process first N docs (after skip pagination)')
    ap.add_argument('--dry-run',action='store_true',help='Count & estimate only; no embedding')
    args=ap.parse_args()
    load_settings(); ep,key=resolve(); deployment, embed_fn = build_client()
    # Count
    count_resp=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json={'search':'*','top':0,'count':True})
    if count_resp.status_code!=200:
        print('Count failed', count_resp.status_code, count_resp.text[:300]); sys.exit(1)
    total=count_resp.json().get('@odata.count',0)
    target = args.limit if args.limit and args.limit < total else total
    print(f"Total docs: {total} (processing {target}) batch={args.batch} deployment={deployment}")
    if args.dry_run:
        print('Dry run: no embeddings generated.')
        return
    skip=0; processed=0; updated=0; start=time.time(); cumulative_chars=0
    while skip < total and processed < target:
        rows=fetch_batch(ep,key,skip,args.batch)
        if not rows: break
        to_embed=[]; id_order=[]
        for r in rows:
            if processed >= target:
                break
            blob=(r.get('dependency_blob') or '').strip()
            processed += 1
            if not blob:
                continue  # skip empty blobs
            blob_cut = blob[:TEXT_TRUNCATE]
            cumulative_chars += len(blob_cut)
            to_embed.append(blob_cut); id_order.append(r['program_id'])
        vectors=[]
        if to_embed:
            vectors=embed_with_retry(embed_fn,to_embed)
        batch=[]
        for pid, vec in zip(id_order, vectors):
            batch.append({'@search.action':'mergeOrUpload','program_id':pid, VECTOR_FIELD:vec, HAS_FIELD: True})
        if batch:
            upload_vectors(ep,key,batch); updated+=len(batch)
        pct=(processed/target*100) if target else 100
        est_tokens = estimate_tokens(to_embed)
        print(f"Processed {processed}/{target} ({pct:.2f}%) embedded_now={len(batch)} total_embedded={updated} est_tokens_batch={est_tokens}")
        skip+=args.batch
        time.sleep(0.05)
    dur=time.time()-start
    print(f"Re-embed complete: embedded {updated} docs in {dur:.1f}s. Rough total tokens ~{estimate_tokens([])} (batch estimates above).")

if __name__=='__main__':
    main()
