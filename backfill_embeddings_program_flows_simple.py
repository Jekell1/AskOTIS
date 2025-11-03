#!/usr/bin/env python3
"""Backfill embeddings for new_cobol_program_flows.flow_vector.

Simple standalone script that follows same pattern as backfill_embeddings_program_meta.py.
"""
import os, sys, json, time, requests, traceback

SEARCH_API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_flows'
VECTOR_FIELD = 'flow_vector'
BATCH = int(os.getenv('PROGRAM_FLOWS_BACKFILL_BATCH','64'))
TEXT_TRUNCATE = int(os.getenv('PROGRAM_FLOWS_TEXT_TRUNCATE','4000'))
EMBED_RETRIES = 5
EMBED_SLEEP_BASE = 0.75
OVERWRITE = os.getenv('OVERWRITE_EMBEDDINGS', 'false').lower() in ('true', '1', 'yes')

try:
    import openai
    try:
        from openai import AzureOpenAI
    except Exception:
        AzureOpenAI = None
except ImportError:
    openai = None
    AzureOpenAI = None


def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals = json.load(f).get('Values', {})
        for k in [
            'AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_ENDPOINT','SEARCH_KEY',
            'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','OPENAI_API_KEY',
            'AZURE_OPENAI_EMBEDDING_DEPLOYMENT','OPENAI_EMBEDDING_MODEL',
            'AZURE_OPENAI_API_VERSION','OPENAI_API_VERSION'
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


def build_azure_openai_client():
    if openai is None:
        raise RuntimeError('openai package not installed. pip install openai')

    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'

    if azure_ep and azure_key:
        if AzureOpenAI is not None:
            client = AzureOpenAI(
                api_key=azure_key,
                api_version=api_version,
                azure_endpoint=azure_ep,
            )

            def _embed(texts):
                resp = client.embeddings.create(model=deployment, input=texts)
                return [d.embedding for d in resp.data]
            print(f"Using AzureOpenAI client deployment={deployment} api_version={api_version}")
            return deployment, _embed
        else:
            openai.api_type = 'azure'
            openai.azure_endpoint = azure_ep
            openai.api_key = azure_key
            openai.api_version = api_version

            def _embed(texts):
                resp = openai.embeddings.create(model=deployment, input=texts)
                return [d.embedding for d in resp.data]
            print(f"Using legacy Azure OpenAI deployment={deployment} api_version={api_version}")
            return deployment, _embed
    
    public_key = os.getenv('OPENAI_API_KEY')
    if not public_key:
        raise RuntimeError('No Azure or public OpenAI credentials available.')
    openai.api_key = public_key
    model = deployment

    def _embed(texts):
        resp = openai.embeddings.create(model=model, input=texts)
        return [d.embedding for d in resp.data]
    print(f"Using public OpenAI model={model}")
    return model, _embed

def embed_texts(embed_fn, texts):
    last_err = None
    for attempt in range(EMBED_RETRIES):
        try:
            return embed_fn(texts)
        except Exception as e:
            last_err = e
            wait = EMBED_SLEEP_BASE * (2 ** attempt)
            print(f"Embedding attempt {attempt+1}/{EMBED_RETRIES} failed: {e.__class__.__name__}: {e}. Retrying in {wait:.2f}s")
            time.sleep(wait)
    print('Final embedding failure:')
    traceback.print_exception(last_err)
    raise last_err


def fetch_docs(ep, key, skip, top):
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}"
    body = {
        'search': '*',
        'select': 'program_id,flow_summary,has_vector',
        'top': top,
        'skip': skip,
        'queryType': 'simple'
    }
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        print('Fetch docs failed', r.status_code, r.text[:300]); sys.exit(1)
    return r.json().get('value', [])


def upload_batch(ep, key, batch):
    url = f"{ep}/indexes/{INDEX}/docs/index?api-version={SEARCH_API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json={'value': batch})
    if r.status_code not in (200, 201):
        print('Upload failed', r.status_code, r.text[:400]); sys.exit(1)

if __name__ == '__main__':
    load_settings(); ep, key = resolve()
    deployment, embed_fn = build_azure_openai_client()
    
    count_resp = requests.post(
        f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json={'search': '*', 'top': 0, 'count': True}, timeout=60
    )
    if count_resp.status_code != 200:
        print('Count request failed', count_resp.status_code, count_resp.text[:400]); sys.exit(1)
    total = count_resp.json().get('@odata.count', 0)
    print(f"Total docs discovered: {total}; batch={BATCH}; deployment={deployment}; overwrite={OVERWRITE}")
    
    skip = 0; processed = 0; updated = 0; skipped_existing = 0
    start = time.time()
    while skip < total:
        docs = fetch_docs(ep, key, skip, BATCH)
        if not docs:
            break
        
        payload_docs = []
        payload_texts = []
        for d in docs:
            summary = (d.get('flow_summary') or '').strip()
            if not summary:
                continue
            
            has_vec = d.get('has_vector')
            if has_vec is True and not OVERWRITE:
                skipped_existing += 1
                continue
            payload_docs.append(d)
            payload_texts.append(summary[:TEXT_TRUNCATE])
        
        if payload_texts:
            vectors = embed_texts(embed_fn, payload_texts)
            out = []
            for d, vec in zip(payload_docs, vectors):
                if not vec:
                    continue
                out.append({'@search.action': 'mergeOrUpload', 'program_id': d['program_id'], VECTOR_FIELD: vec})
            if out:
                upload_batch(ep, key, out)
                updated += len(out)
        
        processed += len(docs)
        skip += BATCH
        pct = (processed / total * 100) if total else 100
        print(f"Processed {processed}/{total} ({pct:.2f}%) - updated vectors={updated} skipped_existing={skipped_existing}")
        time.sleep(0.15)
    
    elapsed = time.time() - start
    print(f"Backfill complete: {updated} documents updated in {elapsed:.1f}s")
