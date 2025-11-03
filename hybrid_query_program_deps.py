"""Hybrid directional retrieval over new_cobol_program_deps.

Combines vector similarity + lexical text search (multi-pass) and optional directional filters.

Directions:
  outgoing  -> emphasize programs with has_outgoing eq true (used for "What does X call?")
  incoming  -> emphasize programs with has_incoming eq true (used for "Who calls X?")
  both      -> no directional filter

Algorithm (simple RRF fusion):
  1. Embed query -> vector search top K
  2. Text search with same query & filter top K
  3. Merge scores via Reciprocal Rank Fusion (1/(k0 + rank))
  4. Fetch enriched fields for final ranked list

Usage:
  python hybrid_query_program_deps.py --query "Which programs call APIPAY?" --direction incoming
  python hybrid_query_program_deps.py --query "What does APIPAY call?" --direction outgoing --k 15

Optional:
  --program-focus APIPAY   (boost docs where program_id == focus or contains focus token)
  --no-text                (disable text leg, vector only)
  --no-vector              (disable vector leg, text only)
"""
from __future__ import annotations
import os, json, sys, argparse, requests, math, re

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_deps'
VECTOR_FIELD='dependency_blob_vector'

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI=None  # type: ignore
except ImportError:
    openai=None; AzureOpenAI=None  # type: ignore


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=str(v)
    except Exception:
        pass


def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key'); sys.exit(1)
    return ep.rstrip('/'), key


def build_embedder():
    if openai is None:
        raise RuntimeError('openai package not installed. pip install openai')
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key=os.getenv('AZURE_OPENAI_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or 'text-embedding-3-large'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _embed(q:str):
            return client.embeddings.create(model=deployment, input=[q]).data[0].embedding
        return _embed, deployment
    if azure_ep and azure_key:
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _embed(q:str):
            return openai.embeddings.create(model=deployment, input=[q]).data[0].embedding
        return _embed, deployment
    pub=os.getenv('OPENAI_API_KEY')
    if not pub:
        raise RuntimeError('No embedding credentials available.')
    openai.api_key=pub
    model=os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-large'
    def _embed(q:str):
        return openai.embeddings.create(model=model, input=[q]).data[0].embedding
    return _embed, model


def vector_search(ep,key,vec,k,filter_expr=None):
    body={'vectorQueries':[{'kind':'vector','vector':vec,'k':k,'fields':VECTOR_FIELD}] , 'select':'program_id,dependency_blob,outgoing_count,incoming_count,has_outgoing,has_incoming'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        print('Vector search failed', r.status_code, r.text[:200]); return []
    return r.json().get('value',[])


def text_search(ep,key,query,k,filter_expr=None):
    body={'search':query,'top':k,'select':'program_id,dependency_blob,outgoing_count,incoming_count,has_outgoing,has_incoming'}
    if filter_expr:
        body['filter']=filter_expr
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code!=200:
        print('Text search failed', r.status_code, r.text[:200]); return []
    return r.json().get('value',[])


def rrf_merge(vec_hits, text_hits, k0=60, focus=None):
    score={} ; info={}
    for rank,h in enumerate(vec_hits, start=1):
        pid=h['program_id']; score[pid]=score.get(pid,0)+1/(k0+rank); info.setdefault(pid,h)
    for rank,h in enumerate(text_hits, start=1):
        pid=h['program_id']; score[pid]=score.get(pid,0)+1/(k0+rank); info.setdefault(pid,h)
    if focus:
        focus_upper=focus.upper()
        for pid in list(score.keys()):
            if pid.upper()==focus_upper:
                score[pid]*=1.4  # strong exact boost
            elif focus_upper in pid.upper():
                score[pid]*=1.15 # partial boost
    merged=[(s,p) for p,s in score.items()]
    merged.sort(reverse=True)
    return [info[p] | {'fusion_score': round(s,6)} for s,p in merged]


def infer_direction(query:str, explicit:str|None):
    if explicit and explicit!='auto':
        return explicit
    q=query.lower()
    # Heuristics
    if 'who calls' in q or 'which programs call' in q or 'depend on' in q:
        return 'incoming'
    if 'what does' in q and 'call' in q:
        return 'outgoing'
    return 'both'


def build_filter(direction:str):
    if direction=='incoming':
        return 'has_incoming eq true'
    if direction=='outgoing':
        return 'has_outgoing eq true'
    return None


def main():
    ap=argparse.ArgumentParser(description='Hybrid directional dependency retrieval')
    ap.add_argument('--query', required=True)
    ap.add_argument('--direction', default='auto', choices=['auto','incoming','outgoing','both'])
    ap.add_argument('--k', type=int, default=20)
    ap.add_argument('--program-focus', help='Boost specific program id token')
    ap.add_argument('--no-text', action='store_true')
    ap.add_argument('--no-vector', action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve(); embed, model = build_embedder()
    direction=infer_direction(args.query, args.direction)
    filter_expr=build_filter(direction)
    print(f"Query='{args.query}' direction={direction} filter={filter_expr or 'NONE'} model={model}")
    vec_hits=[]; text_hits=[]
    if not args.no_vector:
        vec=embed(args.query)
        vec_hits=vector_search(ep,key,vec,args.k,filter_expr)
        print(f"Vector hits: {len(vec_hits)}")
    if not args.no_text:
        text_hits=text_search(ep,key,args.query,args.k,filter_expr)
        print(f"Text hits: {len(text_hits)}")
    if args.no_text and args.no_vector:
        print('Nothing to merge (both legs disabled)'); sys.exit(0)
    merged=rrf_merge(vec_hits,text_hits,focus=args.program_focus)
    print('\nTop merged results:')
    for i,h in enumerate(merged[:args.k], start=1):
        snippet=(h.get('dependency_blob') or '')[:140].replace('\n',' ')
        print(f"{i:2d}. {h['program_id']} out={h.get('outgoing_count')} in={h.get('incoming_count')} score={h['fusion_score']} | {snippet}")

if __name__=='__main__':
    main()
