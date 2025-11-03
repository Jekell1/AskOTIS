"""Quick vector probe for new_cobol_program_deps.

Features:
  * Reports total docs, docs with has_vector true/false
  * Embeds a user query string and issues a vector search
  * Prints top matches (program_id + truncated dependency_blob)

Usage:
  python vector_probe_program_deps.py --query "What does APIPAY call?" --k 5

Environment: loads local.settings.json Values for creds like other scripts.
"""
from __future__ import annotations
import os, json, sys, requests, argparse, textwrap

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_program_deps'
VECTOR_FIELD = 'dependency_blob_vector'

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI = None  # type: ignore
except ImportError:
    openai = None  # type: ignore
    AzureOpenAI = None  # type: ignore


def load_settings():
    try:
        vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass


def resolve_search():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def build_embedder():
    if openai is None:
        raise RuntimeError('openai package not installed. pip install openai')
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or 'text-embedding-3-large'
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client = AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(text: str):
            r = client.embeddings.create(model=deployment, input=[text])
            return r.data[0].embedding
        return _embed, deployment
    if azure_ep and azure_key:  # legacy
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(text: str):
            r = openai.embeddings.create(model=deployment, input=[text])
            return r.data[0].embedding
        return _embed, deployment
    pub = os.getenv('OPENAI_API_KEY')
    if not pub:
        raise RuntimeError('No embedding credentials available.')
    openai.api_key = pub
    model = os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-large'
    def _embed(text: str):
        r = openai.embeddings.create(model=model, input=[text])
        return r.data[0].embedding
    return _embed, model


def count(ep,key,flt: str | None):
    body={'search':'*','top':0,'count':True}
    if flt:
        body['filter']=flt
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code!=200:
        return 0
    return r.json().get('@odata.count',0)


def vector_search(ep,key,vector,k,select):
    body={
        'vectorQueries':[{
            'kind':'vector',
            'vector': vector,
            'k': k,
            'fields': VECTOR_FIELD
        }],
        'select': select
    }
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        print('Vector search failed', r.status_code, r.text[:400])
        return []
    return r.json().get('value',[])


def main():
    ap=argparse.ArgumentParser(description='Vector probe for dependency index')
    ap.add_argument('--query', required=True, help='Natural language query to embed')
    ap.add_argument('--k', type=int, default=5)
    ap.add_argument('--truncate', type=int, default=220, help='Chars of dependency_blob to show')
    args=ap.parse_args()
    load_settings(); ep,key = resolve_search(); embed, model = build_embedder()
    total = count(ep,key,None)
    with_vec = count(ep,key,'has_vector eq true')
    without_vec = total - with_vec
    print(f"Index={INDEX} total={total} has_vector={with_vec} missing_vector={without_vec}")
    print(f"Embedding query with model={model} -> '{args.query}'")
    vec = embed(args.query)
    hits = vector_search(ep,key,vec,args.k,'program_id,has_vector,dependency_blob')
    if not hits:
        print('No hits (or vector search failed).')
        return
    print('\nTop matches:')
    for i,h in enumerate(hits, start=1):
        blob = (h.get('dependency_blob') or '')[:args.truncate].replace('\n',' ')
        blob = textwrap.shorten(blob, width=args.truncate, placeholder='…')
        print(f"{i:2d}. {h.get('program_id')} has_vector={h.get('has_vector')}  | {blob}")

if __name__=='__main__':
    main()
