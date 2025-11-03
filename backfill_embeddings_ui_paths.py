"""Backfill embeddings for new_cobol_ui_paths.program_sequence_json into sequence_vector.

Hardening parallels program_meta backfill:
* Supports AzureOpenAI new SDK or legacy openai usage.
* Explicit api_version to avoid ValueError.
* Retry with exponential backoff.
* Configurable batch size, truncate length.
"""
import os, sys, json, time, requests, traceback

SEARCH_API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_ui_paths'
FIELD = os.getenv('UI_PATH_VECTOR_FIELD','sequence_vector')
BATCH = int(os.getenv('UI_PATH_BACKFILL_BATCH','128'))
TRUNCATE = int(os.getenv('UI_PATH_SEQUENCE_TRUNCATE','3500'))
EMBED_RETRIES = 5
EMBED_SLEEP_BASE = 0.75

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:  # noqa: BLE001
        AzureOpenAI = None  # type: ignore
except ImportError:
    openai=None  # type: ignore
    AzureOpenAI=None  # type: ignore

def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals=json.load(f).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','OPENAI_API_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT','OPENAI_EMBEDDING_MODEL','AZURE_OPENAI_API_VERSION','OPENAI_API_VERSION']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:  # noqa: BLE001
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def build_embed_client():
    if openai is None:
        raise RuntimeError('openai package not installed')
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL','text-embedding-3-small')
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    if azure_ep and azure_key:
        if AzureOpenAI is not None:
            client = AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
            def _embed(texts):
                r = client.embeddings.create(model=deployment, input=texts)
                return [d.embedding for d in r.data]
            print(f"Using AzureOpenAI (new SDK) deployment={deployment} api_version={api_version}")
            return _embed
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(texts):
            r = openai.embeddings.create(model=deployment, input=texts)
            return [d.embedding for d in r.data]
        print(f"Using legacy azure openai deployment={deployment} api_version={api_version}")
        return _embed
    public_key=os.getenv('OPENAI_API_KEY')
    if not public_key:
        raise RuntimeError('No OpenAI credentials available')
    openai.api_key=public_key
    def _embed(texts):
        r=openai.embeddings.create(model=deployment, input=texts)
        return [d.embedding for d in r.data]
    print(f"Using public OpenAI model={deployment}")
    return _embed

def embed_with_retry(embed_fn, texts):
    last=None
    for attempt in range(EMBED_RETRIES):
        try:
            return embed_fn(texts)
        except Exception as e:  # noqa: BLE001
            last=e
            wait=EMBED_SLEEP_BASE*(2**attempt)
            print(f"Embedding batch failed attempt {attempt+1}/{EMBED_RETRIES}: {e}. retry in {wait:.2f}s")
            time.sleep(wait)
    print('Final embedding failure:')
    traceback.print_exception(last)
    raise last

def fetch_docs(ep,key, skip, top):
    body={'search':'*','select':'path_id,program_sequence_json','top':top,'skip':skip,'queryType':'simple'}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        print('Fetch failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value',[])

def upload(ep,key,batch):
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/index?api-version={SEARCH_API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json={'value':batch})
    if r.status_code not in (200,201):
        print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

if __name__=='__main__':
    load_settings(); ep,key=resolve(); embed_fn=build_embed_client()
    count_resp=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json={'search':'*','top':0,'count':True}, timeout=60)
    if count_resp.status_code!=200:
        print('Count failed', count_resp.status_code, count_resp.text[:400]); sys.exit(1)
    total=count_resp.json().get('@odata.count',0); print(f'Total ui path docs: {total}; batch={BATCH}')
    skip=0; processed=0; updated=0; start=time.time()
    while skip < total:
        docs=fetch_docs(ep,key,skip,BATCH)
        if not docs: break
        texts=[]; doc_refs=[]
        for d in docs:
            seq=(d.get('program_sequence_json') or '').strip()
            if not seq:
                continue
            texts.append(seq[:TRUNCATE])
            doc_refs.append(d)
        if texts:
            vecs=embed_with_retry(embed_fn, texts)
            out=[]
            for d,v in zip(doc_refs,vecs):
                if not v: continue
                out.append({'@search.action':'mergeOrUpload','path_id':d['path_id'], FIELD: v})
            if out:
                upload(ep,key,out); updated+=len(out)
        processed+=len(docs); skip+=BATCH
        pct=(processed/total*100) if total else 100
        print(f"Processed {processed}/{total} ({pct:.2f}%) - updated {updated}")
        time.sleep(0.15)
    dur=time.time()-start
    print(f"Backfill complete: {updated} documents updated in {dur:.1f}s")
