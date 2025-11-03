"""Vector similarity probe for variable usage summaries.

Usage:
  python probe_variable_usage_vectors.py --query "late fee accumulation counters" --top 15
  python probe_variable_usage_vectors.py --variable CUSTOMER-ID --top 10  # lexical fallback only

Performs:
  1. Embeds natural language query (Azure OpenAI preferred, falls back to public OpenAI)
  2. Issues vector + lexical hybrid style search (vector first, optional lexical tie-in)
  3. Displays variable_id, read/write counts, top reader/writer programs (truncated)

Assumptions:
  - `usage_summary_vector` field exists on `new_cobol_variable_usage`
  - All (or most) docs have embeddings populated
  - Embedding dimension 3072 (OpenAI text-embedding-3* family)

Environment Variables (same as other scripts):
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
  AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY / AZURE_OPENAI_EMBEDDING_DEPLOYMENT
  OPENAI_API_KEY (fallback)

Exit code 0 on success, non-zero on initialization failures.
"""
from __future__ import annotations
import os, sys, json, argparse, time, requests, math

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX = 'new_cobol_variable_usage'
VECTOR_FIELD = 'usage_summary_vector'

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


def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key


def build_embedder():
    if openai is None:
        raise RuntimeError('openai not installed (pip install openai)')
    azure_ep=os.getenv('AZURE_OPENAI_ENDPOINT'); azure_key=os.getenv('AZURE_OPENAI_KEY')
    deployment=os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL') or 'text-embedding-3-small'
    api_version=os.getenv('AZURE_OPENAI_API_VERSION') or '2024-08-01-preview'
    if azure_ep and azure_key and AzureOpenAI is not None:
        client=AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
        def _emb(texts):
            r=client.embeddings.create(model=deployment,input=texts)
            return [d.embedding for d in r.data]
        return deployment,_emb
    if azure_ep and azure_key:
        # Legacy style
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _emb(texts):
            r=openai.embeddings.create(model=deployment,input=texts)
            return [d.embedding for d in r.data]
        return deployment,_emb
    pub=os.getenv('OPENAI_API_KEY')
    if not pub:
        raise RuntimeError('No embedding credentials found.')
    openai.api_key=pub
    def _emb(texts):
        r=openai.embeddings.create(model=deployment,input=texts)
        return [d.embedding for d in r.data]
    return deployment,_emb


def vector_query(ep,key,vec,top,select):
    body={
        'vectorQueries':[{
            'kind':'vector','vector':vec,'k':top,'fields':VECTOR_FIELD
        }],
        'select':select,
        'top':top
    }
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
    if r.status_code!=200:
        raise RuntimeError(f"Search failed {r.status_code} {r.text[:200]}")
    return r.json().get('value',[])


def lexical_query(ep,key,text,top,select):
    body={'search':text,'top':top,'select':select}
    r=requests.post(f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
    if r.status_code!=200:
        raise RuntimeError(f"Lexical search failed {r.status_code} {r.text[:200]}")
    return r.json().get('value',[])


def format_program_list(lst, limit=5):
    if not lst: return ''
    return ','.join(lst[:limit]) + ('' if len(lst)<=limit else '…')


def main():
    ap=argparse.ArgumentParser(description='Probe variable usage vectors')
    ap.add_argument('--query',help='Natural language query for vector search')
    ap.add_argument('--variable',help='Exact variable name (lexical)')
    ap.add_argument('--top',type=int,default=10)
    ap.add_argument('--show-programs',action='store_true')
    ap.add_argument('--lexical-fallback',action='store_true',help='Also issue lexical search with the same query text')
    args=ap.parse_args()
    if not args.query and not args.variable:
        ap.error('Provide --query or --variable')
    load_settings(); ep,key=resolve_search()
    select='variable_id,read_count,write_count,program_readers,program_writers,symbol_id_global'

    if args.variable:
        rows=lexical_query(ep,key,args.variable.upper(),args.top,select)
        print(f"Lexical matches for {args.variable}:")
        for r in rows:
            readers=format_program_list(r.get('program_readers'))
            writers=format_program_list(r.get('program_writers'))
            print(f"  {r.get('variable_id')}: R={r.get('read_count')} W={r.get('write_count')} readers=[{readers}] writers=[{writers}]")
        return

    # vector path
    try:
        deployment,embed = build_embedder()
    except Exception as e:
        print('Embedding init failed:',e, file=sys.stderr)
        sys.exit(2)
    vec=embed([args.query])[0]
    rows=vector_query(ep,key,vec,args.top,select)
    print(f"Vector results (deployment={deployment}) for query: {args.query}\n")
    for i,r in enumerate(rows,1):
        readers=format_program_list(r.get('program_readers'))
        writers=format_program_list(r.get('program_writers'))
        print(f"{i:02d}. {r.get('variable_id')}  R={r.get('read_count')} W={r.get('write_count')}  readers=[{readers}] writers=[{writers}]")

    if args.lexical_fallback:
        lex=lexical_query(ep,key,args.query,args.top,select)
        print('\nLexical fallback:')
        for i,r in enumerate(lex,1):
            readers=format_program_list(r.get('program_readers'))
            writers=format_program_list(r.get('program_writers'))
            print(f"L{i:02d}. {r.get('variable_id')}  R={r.get('read_count')} W={r.get('write_count')} readers=[{readers}] writers=[{writers}]")

if __name__=='__main__':
    main()
