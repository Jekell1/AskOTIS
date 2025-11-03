"""Semantic probe utility for new_cobol_program_deps dependency blobs.

Allows natural language style queries like:
  python probe_program_deps_vectors.py --query "programs that call tax calc and many copybooks" --top 15

It embeds the query and performs a vector similarity search over dependency_blob_vector
(with lexical fallback slice printed for context).

Environment:
  Loads search + embedding credentials from local.settings.json if present.

Limitations:
  Assumes vector field already added and embeddings backfilled (has_vector=true).
"""
from __future__ import annotations
import os, sys, json, argparse, requests, math
from typing import List, Dict, Any

SEARCH_API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'
VECTOR_FIELD='dependency_blob_vector'
HAS_FIELD='has_vector'
DEFAULT_TOP=10

try:
    import openai
    from openai import AzureOpenAI  # type: ignore
except Exception:  # noqa: BLE001
    openai=None
    AzureOpenAI=None


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in [
            'AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY',
            'AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT','AZURE_OPENAI_API_VERSION',
            'OPENAI_API_KEY','OPENAI_EMBEDDING_MODEL','OPENAI_API_VERSION'
        ]:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass


def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing search endpoint/key', file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key


def build_embedder():
    if openai is None:
        raise SystemExit('openai package not installed (pip install openai)')
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key=os.getenv('AZURE_OPENAI_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(text:str)->List[float]:
            return client.embeddings.create(model=deployment,input=[text]).data[0].embedding
        return _embed, f'AzureOpenAI({deployment})'
    if azure_ep and azure_key:  # legacy
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(text:str)->List[float]:
            return openai.embeddings.create(model=deployment,input=[text]).data[0].embedding
        return _embed, f'AzureOpenAI-legacy({deployment})'
    if os.getenv('OPENAI_API_KEY'):
        openai.api_key=os.getenv('OPENAI_API_KEY')
        model=deployment
        def _embed(text:str)->List[float]:
            return openai.embeddings.create(model=model,input=[text]).data[0].embedding
        return _embed, f'OpenAI({model})'
    raise SystemExit('No embedding credentials available.')


def vector_search(ep,key, vector:List[float], top:int)->List[Dict[str,Any]]:
    """Use new vectorQueries array format (2025 preview API)."""
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={SEARCH_API_VERSION}"
    body={
        'vectorQueries':[{
            'kind':'vector',
            'fields':VECTOR_FIELD,
            'k':top,
            'vector':vector
        }],
        'select':'program_id,dependency_blob,outgoing_count,incoming_count,copybook_count,external_count,screens_count',
        'filter':f'{HAS_FIELD} eq true'
    }
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise SystemExit(f'Vector search failed {r.status_code}: {r.text[:400]}')
    return r.json().get('value',[])


def format_row(r:Dict[str,Any])->str:
    blob=(r.get('dependency_blob') or '')
    snippet = (blob[:140] + '…') if len(blob)>140 else blob
    return f"{r.get('@search.score',0):7.4f}  {r.get('program_id'):<12} out={r.get('outgoing_count',0):3} in={r.get('incoming_count',0):3} copy={r.get('copybook_count',0):3} ext={r.get('external_count',0):2} scr={r.get('screens_count',0):2} | {snippet}".rstrip()


def main():
    ap=argparse.ArgumentParser(description='Semantic probe for program dependency aggregates')
    ap.add_argument('--query','-q',required=True)
    ap.add_argument('--top',type=int,default=DEFAULT_TOP)
    ap.add_argument('--show-json',action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve_search(); embed_fn,label=build_embedder()
    print(f"Embedding query with {label} ...")
    vec=embed_fn(args.query)
    rows=vector_search(ep,key,vec,args.top)
    if args.show_json:
        print(json.dumps(rows,indent=2)[:4000])
    else:
        print(f"Top {len(rows)} results:")
        for r in rows:
            print(format_row(r))

if __name__=='__main__':
    main()
