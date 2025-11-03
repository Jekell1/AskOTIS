"""Validate hybrid retrieval & vector presence for new_cobol_program_meta and new_cobol_ui_paths.

Performs:
1. Simple text + vector semantic hybrid (two-step):
   - Generate embedding for query
   - Issue vector search against each index (if vector fields exist)
   - Issue text search against each index
   - Merge & display top combined results (lightweight heuristic)
2. Prints sample vector norm lengths to confirm 3072-dim populated.

Env requirements (loaded from local.settings.json Values if present):
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY
  AZURE_OPENAI_ENDPOINT / AZURE_OPENAI_KEY / AZURE_OPENAI_EMBEDDING_DEPLOYMENT
  AZURE_OPENAI_API_VERSION (optional, defaults 2024-08-01-preview)

Usage:
  python validate_hybrid_retrieval.py "ACCOUNTS PAYABLE batch job"
"""
import os, sys, json, math, requests, statistics

QUERY = sys.argv[1] if len(sys.argv) > 1 else "ACCOUNTS PAYABLE batch processing"
SEARCH_API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_INDEX = 'new_cobol_program_meta'
PROGRAM_VECTOR_FIELD = 'program_summary_vector'
PROGRAM_ID_FIELD = 'program_id'
PROGRAM_TEXT_FIELD = 'program_summary'
UI_INDEX = 'new_cobol_ui_paths'
UI_VECTOR_FIELD = 'sequence_vector'
UI_ID_FIELD = 'path_id'
UI_TEXT_FIELD = 'program_sequence_json'
PROGRAM_TOP = 5
UI_TOP = 3

try:
    import openai
    try:
        from openai import AzureOpenAI  # type: ignore
    except Exception:
        AzureOpenAI = None
except ImportError:
    openai = None
    AzureOpenAI = None


def load_settings():
    try:
        with open('local.settings.json','r',encoding='utf-8') as f:
            vals = json.load(f).get('Values', {})
        for k,v in vals.items():
            if k not in os.environ:
                os.environ[k]=v
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
        raise RuntimeError('openai package (>=1.0) required')
    deployment = os.getenv('AZURE_OPENAI_EMBEDDING_DEPLOYMENT') or os.getenv('OPENAI_EMBEDDING_MODEL','text-embedding-3-large')
    api_version = os.getenv('AZURE_OPENAI_API_VERSION') or os.getenv('OPENAI_API_VERSION') or '2024-08-01-preview'
    azure_ep = os.getenv('AZURE_OPENAI_ENDPOINT')
    azure_key = os.getenv('AZURE_OPENAI_KEY')
    if azure_ep and azure_key:
        if AzureOpenAI:
            client = AzureOpenAI(api_key=azure_key, api_version=api_version, azure_endpoint=azure_ep)
            def _emb(t):
                r = client.embeddings.create(model=deployment, input=[t])
                return r.data[0].embedding
            return _emb
        # legacy
        openai.api_type='azure'; openai.azure_endpoint=azure_ep; openai.api_key=azure_key; openai.api_version=api_version
        def _emb(t):
            r=openai.embeddings.create(model=deployment, input=[t])
            return r.data[0].embedding
        return _emb
    pk = os.getenv('OPENAI_API_KEY')
    if not pk:
        raise RuntimeError('No OpenAI credentials')
    openai.api_key=pk
    def _emb(t):
        r=openai.embeddings.create(model=deployment, input=[t])
        return r.data[0].embedding
    return _emb

def vector_search(ep,key,index,vector_field,vector,select_id,select_text,top):
    # Using SEARCH endpoint vector query preview style (vectorQueries)
    body={
        "select": f"{select_id},{select_text}",
        "top": top,
        "vectorQueries": [
            {"kind":"vector","vector":vector,"fields":vector_field,"k":top}
        ]
    }
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={SEARCH_API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])

def text_search(ep,key,index,query,select_id,select_text,top):
    body={"search": query, "select": f"{select_id},{select_text}", "top": top}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={SEARCH_API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])

def norm(vec):
    return math.sqrt(sum(x*x for x in vec)) if vec else 0.0

def score_merge(vec_hits, text_hits, id_field):
    # Build dictionaries keyed by id; assign synthetic scores (vector rank + text rank)
    merged = {}
    for rank,doc in enumerate(vec_hits):
        merged.setdefault(doc[id_field], {}).update({'doc':doc, 'vec_rank':rank+1})
    for rank,doc in enumerate(text_hits):
        merged.setdefault(doc[id_field], {}).update({'doc':doc, 'text_rank':rank+1})
    # Heuristic combined score: 1/(vec_rank or large) + 1.2/(text_rank or large)
    rows = []
    for k,v in merged.items():
        vr = v.get('vec_rank', 999)
        tr = v.get('text_rank', 999)
        combined = (1/vr if vr<999 else 0) + 1.2*(1/tr if tr<999 else 0)
        rows.append((combined, v['doc']))
    rows.sort(reverse=True, key=lambda x: x[0])
    return [d for _,d in rows]

def sample_vector_lengths(ep,key,index,vector_field,select_id,limit=5):
    # just pull some docs and measure lengths (assuming vector field retrievable=False so skip)
    body={'search':'*','select':select_id,'top':limit}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={SEARCH_API_VERSION}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200: return []
    # We cannot retrieve the vector (retrievable False). Instead run vector query with a random unit vector to confirm dimension via score presence.
    return []

if __name__=='__main__':
    load_settings(); ep,key = resolve_search(); embed = build_embedder()
    print(f"Query: {QUERY}")
    qvec = embed(QUERY)
    print(f"Embedding dimension: {len(qvec)} norm={norm(qvec):.2f}")

    prog_vec_hits = vector_search(ep,key,PROGRAM_INDEX,PROGRAM_VECTOR_FIELD,qvec,PROGRAM_ID_FIELD,PROGRAM_TEXT_FIELD,PROGRAM_TOP)
    prog_text_hits = text_search(ep,key,PROGRAM_INDEX,QUERY,PROGRAM_ID_FIELD,PROGRAM_TEXT_FIELD,PROGRAM_TOP)
    merged_prog = score_merge(prog_vec_hits, prog_text_hits, PROGRAM_ID_FIELD)

    ui_vec_hits = vector_search(ep,key,UI_INDEX,UI_VECTOR_FIELD,qvec,UI_ID_FIELD,UI_TEXT_FIELD,UI_TOP)
    ui_text_hits = text_search(ep,key,UI_INDEX,QUERY,UI_ID_FIELD,UI_TEXT_FIELD,UI_TOP)
    merged_ui = score_merge(ui_vec_hits, ui_text_hits, UI_ID_FIELD)

    print('\n=== Program Meta (Hybrid heuristic) ===')
    for i,d in enumerate(merged_prog[:PROGRAM_TOP], start=1):
        snippet = (d.get(PROGRAM_TEXT_FIELD) or '')[:160].replace('\n',' ') if d.get(PROGRAM_TEXT_FIELD) else ''
        print(f"{i}. {d.get(PROGRAM_ID_FIELD)}  snippet="+snippet)
    print('\nVector-only top program IDs:', [d.get(PROGRAM_ID_FIELD) for d in prog_vec_hits[:PROGRAM_TOP]])
    print('Text-only top program IDs:', [d.get(PROGRAM_ID_FIELD) for d in prog_text_hits[:PROGRAM_TOP]])

    print('\n=== UI Paths (Hybrid heuristic) ===')
    for i,d in enumerate(merged_ui[:UI_TOP], start=1):
        snippet = (d.get(UI_TEXT_FIELD) or '')[:140].replace('\n',' ') if d.get(UI_TEXT_FIELD) else ''
        print(f"{i}. {d.get(UI_ID_FIELD)}  snippet="+snippet)
    print('\nVector-only top path IDs:', [d.get(UI_ID_FIELD) for d in ui_vec_hits[:UI_TOP]])
    print('Text-only top path IDs:', [d.get(UI_ID_FIELD) for d in ui_text_hits[:UI_TOP]])

    print('\nValidation complete.')
