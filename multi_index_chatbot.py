#!/usr/bin/env python3
"""Enhanced multi-index COBOL RAG chatbot.

Features added over chatbot_smoke_test.py:
  - Multi-index retrieval: query several Azure AI Search indexes in one call (sequential fan‑out) and merge results.
  - Optional hybrid mode (vector + lexical) per index.
  - Optional OData filter expression applied (if supported by index) via --filter.
  - Lightweight normalization + merge scoring across indexes.
  - Optional second‑stage local rerank (embedding similarity) if --rerank > 0.
    * If vector field is not retrievable, we re-embed truncated doc text (costly). You can disable via --rerank 0.
  - Conversation mode: persistent session history stored under .rag_sessions/<session>.json
    * Include the most recent N turns (configurable) in the chat context.
  - Source grouping by index and compact citation list.
  - Basic safety: truncates prompt to target max chars, with defensive fallbacks.

Environment / config (env or local.settings.json Values):
  AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY
  AZURE_OPENAI_ENDPOINT (or OPENAI_ENDPOINT), AZURE_OPENAI_KEY (or OPENAI_API_KEY)
  EMBEDDING_DEPLOYMENT (default text-embedding-3-large)
  CHAT_DEPLOYMENT (default gpt-4o-mini)

Example:
  python multi_index_chatbot.py \
      --query "How is ACUMEM program invoked?" \
      --indexes code-chunks,cobol-paragraphs,cobol-symbols \
      --k 6 --rerank 8 --hybrid --filter "program_id eq 'ACUMEM'" \
      --session acumem --turns 6

Notes:
  - Rerank re-embeds candidate document text. This can be expensive; adjust --rerank.
  - If you want to avoid re-embedding, omit --rerank or set it to 0.
  - Filters are applied verbatim; ensure fields exist in each index or Azure Search will ignore / error depending on schema.
  - You can add semantic config or Azure Reranker later; this script keeps everything client-side.
"""
import os, json, argparse, sys, math, time, pathlib, hashlib
from typing import List, Dict, Any
import requests

API_VERSION_SEARCH = "2024-07-01"
API_VERSION_OPENAI = "2024-08-01-preview"

DEFAULT_INDEXES = ["code-chunks"]
# Alias mapping for user convenience / legacy names
INDEX_ALIASES = {
    'cobol-chunks': 'code-chunks',
    'chunks': 'code-chunks',
    'chunk': 'code-chunks',
    'logic': 'code-chunks',
    'code': 'code-chunks',
    'codechunks': 'code-chunks',
    'symbols': 'cobol-symbols',
    'symbol': 'cobol-symbols',
    'paragraphs': 'cobol-paragraphs',
    'paras': 'cobol-paragraphs',
    'calls': 'cobol-calls',
    'call': 'cobol-calls',
    'xrefs': 'cobol-xrefs',
    'xref': 'cobol-xrefs'
}
# Defaults (will be overridden per-index via schema discovery)
VECTOR_FIELD_DEFAULT = "text_vector"
TEXT_FIELD_DEFAULT = "text"
KEY_CANDIDATES = ["chunk_id", "id", "key", "document_id"]  # attempt in order

SESSION_DIR = ".rag_sessions"


def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    cfg = {
        'search_endpoint': first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT'),
        'search_key': first('AZURE_SEARCH_KEY','SEARCH_KEY'),
        'aoai_endpoint': first('AZURE_OPENAI_ENDPOINT','OPENAI_ENDPOINT'),
        'aoai_key': first('AZURE_OPENAI_KEY','OPENAI_API_KEY'),
        'embedding_deployment': first('EMBEDDING_DEPLOYMENT') or 'text-embedding-3-large',
        'chat_deployment': first('CHAT_DEPLOYMENT') or 'gpt-4o-mini'
    }
    missing = [k for k,v in cfg.items() if not v]
    if missing:
        print(f"Missing required configuration values: {missing}", file=sys.stderr)
        sys.exit(1)
    cfg['search_endpoint'] = cfg['search_endpoint'].rstrip('/')
    cfg['aoai_endpoint'] = cfg['aoai_endpoint'].rstrip('/')
    return cfg


def embed(cfg, text: str) -> List[float]:
    url = f"{cfg['aoai_endpoint']}/openai/deployments/{cfg['embedding_deployment']}/embeddings?api-version={API_VERSION_OPENAI}"
    body = {"input": text}
    r = requests.post(url, headers={'api-key':cfg['aoai_key'],'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Embedding failed {r.status_code}: {r.text[:200]}")
    return r.json()['data'][0]['embedding']


def choose_key(doc: Dict[str,Any]) -> str:
    for k in KEY_CANDIDATES:
        if k in doc:
            return doc[k]
    return doc.get('@search.documentId') or '<no-key>'


def get_index_schema(cfg, index: str):
    url = f"{cfg['search_endpoint']}/indexes/{index}?api-version={API_VERSION_SEARCH}"
    r = requests.get(url, headers={'api-key':cfg['search_key']}, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Schema fetch failed index={index} {r.status_code}: {r.text[:160]}")
    return r.json()


_SCHEMA_CACHE: Dict[str,Dict[str,Any]] = {}


def discover_fields(cfg, index: str):
    if index in _SCHEMA_CACHE:
        return _SCHEMA_CACHE[index]
    schema = get_index_schema(cfg, index)
    fields = schema.get('fields', [])
    vector_field = None
    text_field = None
    key_field = None
    for f in fields:
        name = f.get('name')
        if f.get('key'):
            key_field = name
        # detect vector (collection of single type?) presence via vectorSearchProfile reference or dimensions
        if f.get('searchable') and f.get('type') == 'Collection(Edm.Single)':
            # candidate vector field
            vector_field = vector_field or name
        # pick a text-like field
        if f.get('searchable') and f.get('type') == 'Edm.String':
            # prefer names containing 'text' or 'content'
            lower = name.lower()
            if not text_field or ('text' in lower or 'content' in lower):
                text_field = name
    if not vector_field:
        vector_field = VECTOR_FIELD_DEFAULT
    if not text_field:
        # fallback: first string field
        for f in fields:
            if f.get('type') == 'Edm.String':
                text_field = f.get('name')
                break
    if not key_field:
        # last resort heuristic
        for c in KEY_CANDIDATES:
            if any(f.get('name') == c for f in fields):
                key_field = c
                break
        key_field = key_field or 'id'
    field_names = {f.get('name') for f in fields}
    discovered = {'vector_field': vector_field, 'text_field': text_field, 'key_field': key_field, 'field_names': field_names}
    _SCHEMA_CACHE[index] = discovered
    return discovered


def vector_search(cfg, index: str, query_embedding: List[float], k: int, hybrid: bool, text_query: str, filter_expr: str, select_extra: str) -> List[Dict[str,Any]]:
    flds = discover_fields(cfg, index)
    vector_field = flds['vector_field']
    text_field = flds['text_field']
    key_field = flds['key_field']
    field_names = flds['field_names']
    # Safely filter select_extra list against schema
    safe_extra = []
    if select_extra:
        for part in select_extra.split(','):
            p = part.strip()
            if p and p in field_names and p not in (text_field,):
                safe_extra.append(p)
    select_clause = text_field
    if safe_extra:
        select_clause = text_field + "," + ",".join(safe_extra)
    url = f"{cfg['search_endpoint']}/indexes/{index}/docs/search?api-version={API_VERSION_SEARCH}"
    body = {
        "vectorQueries": [
            {
                "kind": "vector",
                "vector": query_embedding,
                "fields": vector_field,
                "k": k
            }
        ],
        "top": k,
        # For hybrid we set search=text_query, else wildcard to avoid 0 lexical weight
        "search": text_query if hybrid else "*",
    "select": select_clause
    }
    if filter_expr:
        body['filter'] = filter_expr
    headers = {'api-key':cfg['search_key'],'Content-Type':'application/json'}
    r = requests.post(url, headers=headers, json=body, timeout=60)
    if r.status_code != 200:
        raise RuntimeError(f"Search failed index={index} {r.status_code}: {r.text[:200]}")
    results = r.json().get('value', [])
    for d in results:
        d['_index'] = index
        # prefer discovered key field
        if key_field in d:
            d['_key'] = d.get(key_field)
        else:
            d['_key'] = choose_key(d)
        d['_text_field'] = text_field
    return results


def normalize_scores(all_docs: List[Dict[str,Any]]) -> None:
    # Group by index then scale scores per index for fair-ish merging
    by_index: Dict[str,List[float]] = {}
    for d in all_docs:
        by_index.setdefault(d['_index'], []).append(d.get('@search.score', 0.0))
    max_per_index = {idx: max(scores) if scores else 1.0 for idx,scores in by_index.items()}
    for d in all_docs:
        base = d.get('@search.score',0.0)
        denom = max_per_index.get(d['_index'], 1.0) or 1.0
        d['_norm_score'] = base / denom


def second_stage_rerank(cfg, docs: List[Dict[str,Any]], query_embedding: List[float], top_rerank: int, rerank_chars: int = 900) -> None:
    # Re-embed truncated text content for top_rerank docs and compute cosine similarity
    # Only operate on provided docs (in-place adds _rerank_score)
    subset = docs[:top_rerank]
    q_norm = math.sqrt(sum(x*x for x in query_embedding)) or 1.0
    for d in subset:
        # use per-doc discovered text field marker
        text_field = d.get('_text_field') or TEXT_FIELD_DEFAULT
        text = (d.get(text_field) or '')[:rerank_chars]
        emb = embed(cfg, text)
        dot = sum(a*b for a,b in zip(query_embedding, emb))
        e_norm = math.sqrt(sum(x*x for x in emb)) or 1.0
        cos = dot / (q_norm * e_norm)
        d['_rerank_score'] = cos
    # For docs beyond rerank set fallback
    for d in docs[top_rerank:]:
        d['_rerank_score'] = d.get('_norm_score',0.0) * 0.95  # slight penalty
    docs.sort(key=lambda x: x['_rerank_score'], reverse=True)


def build_prompt(query: str, docs: List[Dict[str,Any]], max_chars: int, max_docs: int, conversation: List[Dict[str,str]]):
    header = [
        "You are a COBOL code expert answering only from provided sources.",
        "Cite sources as (idx:<index>|key:<key>) for each fact.",
        "If insufficient info, say so honestly.",
    ]
    convo_snippets = []
    for turn in conversation[-6:]:  # safe slice
        role = turn.get('role')
        content = turn.get('content','')
        if role in ('user','assistant'):
            convo_snippets.append(f"{role.upper()}: {content[:500]}")
    if convo_snippets:
        header.append("Previous conversation context:\n" + "\n".join(convo_snippets))
    header.append("User Question: " + query)
    header.append("Sources:")

    body_parts = []
    used = 0
    for i,d in enumerate(docs[:max_docs],1):
        key = d.get('_key')
        idx = d.get('_index')
        text_field = d.get('_text_field') or TEXT_FIELD_DEFAULT
        text = (d.get(text_field) or '')
        snippet = text.replace('\n',' ')[:700]
        entry = f"[{i}] index={idx} key={key} : {snippet}"
        body_parts.append(entry)
        used += len(entry)
        if used > max_chars:
            break
    footer = ["Answer:"]
    prompt = "\n".join(header + body_parts + footer)
    return prompt


def chat(cfg, prompt: str, temperature: float, max_tokens: int, history: List[Dict[str,str]]):
    url = f"{cfg['aoai_endpoint']}/openai/deployments/{cfg['chat_deployment']}/chat/completions?api-version={API_VERSION_OPENAI}"
    messages = [{"role":"system","content":"You are a concise, accurate COBOL code assistant."}]
    # preserve prior assistant/user turns (already trimmed in build_prompt). We add as a single user summary chunk implicitly, so just pass prompt.
    messages.append({"role":"user","content": prompt})
    body = {
        "model": cfg['chat_deployment'],
        "messages": messages,
        "temperature": temperature,
        "max_tokens": max_tokens
    }
    r = requests.post(url, headers={'api-key':cfg['aoai_key'],'Content-Type':'application/json'}, json=body, timeout=120)
    if r.status_code != 200:
        raise RuntimeError(f"Chat failed {r.status_code}: {r.text[:250]}")
    data = r.json()
    return data['choices'][0]['message']['content']


def load_session(session_name: str) -> List[Dict[str,str]]:
    if not session_name:
        return []
    os.makedirs(SESSION_DIR, exist_ok=True)
    path = os.path.join(SESSION_DIR, f"{session_name}.json")
    if os.path.exists(path):
        try:
            return json.load(open(path,'r',encoding='utf-8'))
        except Exception:
            return []
    return []


def save_session(session_name: str, history: List[Dict[str,str]]):
    if not session_name:
        return
    os.makedirs(SESSION_DIR, exist_ok=True)
    path = os.path.join(SESSION_DIR, f"{session_name}.json")
    tmp = path + ".tmp"
    with open(tmp,'w',encoding='utf-8') as f:
        json.dump(history, f, ensure_ascii=False, indent=2)
    os.replace(tmp, path)


def dedupe_docs(docs: List[Dict[str,Any]]) -> List[Dict[str,Any]]:
    seen = set()
    out = []
    for d in docs:
        key = (d.get('_index'), d.get('_key'))
        if key in seen:
            continue
        seen.add(key)
        out.append(d)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--query', required=True, help='User question')
    ap.add_argument('--indexes', default=','.join(DEFAULT_INDEXES), help='Comma-separated list of indexes to query')
    ap.add_argument('--k', type=int, default=6, help='Top K per index to retrieve BEFORE merge')
    ap.add_argument('--merge-top', type=int, default=18, help='Top documents after merge BEFORE rerank')
    ap.add_argument('--final-top', type=int, default=10, help='Final number of docs to show in prompt')
    ap.add_argument('--rerank', type=int, default=0, help='If >0, number of docs (post-merge) to re-embed and rerank')
    ap.add_argument('--hybrid', action='store_true', help='Use lexical+vector hybrid (sets search=<query>)')
    ap.add_argument('--filter', default='', help='OData filter expression applied to each index (optional)')
    ap.add_argument('--temperature', type=float, default=0.2)
    ap.add_argument('--max-tokens', type=int, default=900)
    ap.add_argument('--prompt-max-chars', type=int, default=12000)
    ap.add_argument('--session', default='', help='Conversation session name (persist history)')
    ap.add_argument('--reset', action='store_true', help='Reset session history before answering')
    ap.add_argument('--turns', type=int, default=6, help='Max prior turns to carry forward')
    ap.add_argument('--select-extra', default='path,program_id,scope,name,start_line,end_line', help='Extra fields to request')
    ap.add_argument('--debug-fields', action='store_true', help='Print discovered schema fields per index')
    ap.add_argument('--augment-calls', action='store_true', help='Synthesize richer snippets for call indexes (e.g., cobol-calls)')
    ap.add_argument('--show-debug', action='store_true', help='Print detailed retrieval, merge, rerank, and final selection diagnostics')
    ap.add_argument('--enrich-records', action='store_true', help='If set, for xref read/write docs attempt to pull the matching symbol definition (record layout) and inject as extra source')
    ap.add_argument('--list-indexes', action='store_true', help='List available indexes from the search service and exit')
    ap.add_argument('--enforce-diversity', action='store_true', help='Guarantee at least one high-value chunk/paragraph doc in final set if available')
    ap.add_argument('--enrich-logic', action='store_true', help='Fetch surrounding code-chunks for paragraph/call/xref docs (path + line heuristics)')
    ap.add_argument('--expand-query', action='store_true', help='Expand user query with synonym set (accumulate, add, increment, total, arithmetic) for hybrid lexical and slight embedding blend')
    ap.add_argument('--ensure-arithmetic', action='store_true', help='If no arithmetic COBOL verbs (ADD/COMPUTE/etc.) found in final docs, run fallback lexical searches on code-chunks to inject at least one arithmetic logic snippet')
    ap.add_argument('--facts-fallback', action='store_true', help='If logic coverage is shallow, pull supporting fact snippets from cobol-facts and inject synthetic doc(s)')
    # Flow summarization (Option B)
    ap.add_argument('--flow-summarize', action='store_true', help='Summarize control flow using cobol-flow-edges index and inject as a synthesized doc')
    ap.add_argument('--flow-program', default='', help='Program or paragraph token to focus flow summarization (e.g., TIM360)')
    ap.add_argument('--flow-max-edges', type=int, default=120, help='Maximum edges to fetch / process for flow summarization')
    ap.add_argument('--flow-debug-dump', type=int, default=0, help='If >0, dump first N raw edge docs (caller->target kind line) for debugging before summarization')
    args = ap.parse_args()

    cfg = load_settings()
    # List indexes mode
    if args.list_indexes:
        cfg_tmp = load_settings()
        url_list = f"{cfg_tmp['search_endpoint']}/indexes?api-version={API_VERSION_SEARCH}"
        r_list = requests.get(url_list, headers={'api-key':cfg_tmp['search_key']}, timeout=30)
        if r_list.status_code != 200:
            print(f"Failed to list indexes: {r_list.status_code} {r_list.text[:160]}", file=sys.stderr)
            sys.exit(2)
        names = [it.get('name') for it in r_list.json().get('value', [])]
        print("Available indexes:")
        for n in names:
            print(f"  - {n}")
        return

    indexes_raw = [x.strip() for x in args.indexes.split(',') if x.strip()]
    # Apply aliases
    indexes = []
    for ix in indexes_raw:
        mapped = INDEX_ALIASES.get(ix.lower(), ix)
        if mapped not in indexes:
            indexes.append(mapped)
    # Guaranteed logic enrichment: if --enrich-logic flag set and code-chunks not specified, force add it.
    if args.enrich_logic and 'code-chunks' not in indexes:
        indexes.append('code-chunks')
        if args.show_debug:
            print("[DEBUG] --enrich-logic active: forcibly added 'code-chunks' index")
    else:
        # Legacy auto-inject (best-effort) if user omitted it entirely
        try:
            if 'code-chunks' not in indexes:
                url_list = f"{cfg['search_endpoint']}/indexes?api-version={API_VERSION_SEARCH}"
                r_list = requests.get(url_list, headers={'api-key':cfg['search_key']}, timeout=20)
                if r_list.status_code == 200:
                    service_indexes = {it.get('name') for it in r_list.json().get('value', [])}
                    if 'code-chunks' in service_indexes:
                        indexes.append('code-chunks')
                        print("[INFO] Auto-added 'code-chunks' index (logic source) – you can omit explicit listing; alias synonyms: chunks, logic, code.")
                else:
                    if args.show_debug:
                        print(f"[DEBUG] Unable to list indexes for auto-inject: {r_list.status_code}")
        except Exception as ex:
            if args.show_debug:
                print(f"[DEBUG] Auto-inject logic index check failed: {ex}")
    if not indexes:
        print("No indexes provided", file=sys.stderr)
        sys.exit(1)

    history = [] if args.reset else load_session(args.session)
    # Warn if no chunk-like index included
    if not any('chunk' in i for i in indexes):
        print("[WARN] No chunk index specified. Answers may be shallow (add code-chunks).", file=sys.stderr)

    # Query expansion (lexical) if requested
    base_query = args.query
    expansion_terms = []
    if args.expand_query:
        # Simple static synonym set tuned for accumulation/update semantics
        SYNS = ["accumulate", "accumulated", "add", "addition", "increment", "update", "total", "totals", "sum", "arithmetic"]
        # Include only those not already present (case-insensitive containment)
        lower_q = base_query.lower()
        expansion_terms = [s for s in SYNS if s not in lower_q]
        if expansion_terms:
            # Append for lexical search effect; embeddings still done on original base query for precision
            expanded_for_search = base_query + " " + " ".join(expansion_terms)
        else:
            expanded_for_search = base_query
    else:
        expanded_for_search = base_query

    print(f"Query: {base_query}")
    if args.expand_query:
        print(f"Expanded lexical query: {expanded_for_search}")
    print(f"Indexes: {indexes}")
    # Embedding only on original query (could consider blended embedding later)
    query_embedding = embed(cfg, base_query)
    print(f"Query embedding length={len(query_embedding)}")

    all_results: List[Dict[str,Any]] = []
    for idx in indexes:
        try:
            # If hybrid + expand-query, pass expanded_for_search; else original
            text_query = expanded_for_search if (args.hybrid and args.expand_query) else base_query
            res = vector_search(cfg, idx, query_embedding, args.k, args.hybrid, text_query, args.filter, args.select_extra)
            if args.debug_fields:
                # Show discovered field info
                disc = _SCHEMA_CACHE.get(idx, {})
                print(f"[DEBUG] Index={idx} discovered fields: text_field={disc.get('text_field')} vector_field={disc.get('vector_field')} key_field={disc.get('key_field')}")
                # Print an abbreviated field list
                f_names = sorted(list(disc.get('field_names', [])))
                print(f"[DEBUG] Index={idx} field names (truncated to 40): {f_names[:40]}")
            print(f"  Retrieved {len(res)} from {idx}")
            all_results.extend(res)
        except Exception as ex:
            print(f"  WARN index {idx} failed: {ex}", file=sys.stderr)

    if not all_results:
        print("No results from any index; aborting")
        return

    all_results = dedupe_docs(all_results)
    if args.show_debug:
        print(f"[DEBUG] Total raw docs after dedupe: {len(all_results)}")
        if args.expand_query:
            print(f"[DEBUG] Query expansion terms used: {expansion_terms}")
    normalize_scores(all_results)
    all_results.sort(key=lambda x: x.get('_norm_score',0.0), reverse=True)
    merged_slice = all_results[:args.merge_top]
    if args.show_debug:
        print(f"[DEBUG] Top {len(merged_slice)} after merge (pre-rerank):")
        for i,d in enumerate(merged_slice,1):
            print(f"  M{i:02d} idx={d.get('_index')} key={d.get('_key')} score={d.get('@search.score'):.4f} norm={d.get('_norm_score'):.4f}")

    if args.rerank > 0:
        print(f"Running second-stage rerank on top {min(args.rerank, len(merged_slice))} docs (may incur embedding cost)...")
        second_stage_rerank(cfg, merged_slice, query_embedding, min(args.rerank, len(merged_slice)))
    else:
        for d in merged_slice:
            d['_rerank_score'] = d.get('_norm_score',0.0)

    final_docs = merged_slice[:args.final_top]
    # Guarantee at least one logic (code-chunks) doc if --enrich-logic is active
    if args.enrich_logic:
        have_logic = any(d.get('_index','') == 'code-chunks' or 'chunk' in d.get('_index','') for d in final_docs)
        if not have_logic:
            # Search remainder of merged_slice first
            candidate = None
            for d in merged_slice[args.final_top:]:
                if d.get('_index','') == 'code-chunks' or 'chunk' in d.get('_index',''):
                    candidate = d; break
            # If still none, attempt a targeted lightweight search directly on code-chunks
            if candidate is None and 'code-chunks' in indexes:
                try:
                    # Reuse existing embedding / query and pull a few more chunk docs
                    extra = vector_search(cfg, 'code-chunks', query_embedding, k=max(3, args.k), hybrid=args.hybrid, text_query=base_query, filter_expr=args.filter, select_extra=args.select_extra)
                    if extra:
                        candidate = extra[0]
                        if args.show_debug:
                            print(f"[DEBUG] Injected code-chunks candidate via fallback search key={candidate.get('_key')}")
                except Exception as ex:
                    if args.show_debug:
                        print(f"[DEBUG] Fallback chunk retrieval failed: {ex}")
            if candidate:
                # Replace lowest scoring doc (based on rerank or norm)
                sorted_current = sorted(final_docs, key=lambda x: x.get('_rerank_score', x.get('_norm_score',0.0)))
                replaced = sorted_current[0]
                final_docs.remove(replaced)
                final_docs.append(candidate)
                if args.show_debug:
                    print(f"[DEBUG] Logic guarantee replaced {replaced.get('_index')}|{replaced.get('_key')} with {candidate.get('_index')}|{candidate.get('_key')}")
    # Diversity enforcement: ensure at least one chunk/paragraph doc
    if args.enforce_diversity:
        have_logic = any(d.get('_index','').endswith('chunks') or 'chunk' in d.get('_index','') for d in final_docs)
        if not have_logic:
            # search earlier merged_slice for a chunk doc not already included
            logic_candidate = None
            for d in merged_slice[args.final_top:]:
                if d.get('_index','').endswith('chunks') or 'chunk' in d.get('_index',''):
                    logic_candidate = d
                    break
            if logic_candidate:
                # replace the lowest scoring non-chunk doc
                final_docs_sorted = sorted(final_docs, key=lambda x: x.get('_rerank_score', x.get('_norm_score',0.0)))
                replaced = final_docs_sorted[0]
                final_docs.remove(replaced)
                final_docs.append(logic_candidate)
                if args.show_debug:
                    print(f"[DEBUG] Diversity enforcement replaced doc {replaced.get('_index')}|{replaced.get('_key')} with {logic_candidate.get('_index')}|{logic_candidate.get('_key')}")
    if args.show_debug:
        print(f"[DEBUG] Final {len(final_docs)} docs (post-rerank if applied):")
        for i,d in enumerate(final_docs,1):
            rr = d.get('_rerank_score')
            ns = d.get('_norm_score')
            print(f"  F{i:02d} idx={d.get('_index')} key={d.get('_key')} norm={ns:.4f} rerank={rr:.4f}")

    # Optional augmentation for call indexes where text is uninformative (e.g., just key value)
    if args.augment_calls:
        CALL_INDEX_HINTS = {'calls', 'cobol-calls'}
        call_field_candidates_priority = [
            'caller_program','calling_program','program_id','source_program','caller','from_program',
            'callee_program','called_program','target_program','target','callee','name','scope','paragraph','para',
            'line','line_number','start_line','end_line','path'
        ]
        import re
        key_pattern = re.compile(r'^([A-Za-z]\d+)-(\d+)-([A-Za-z0-9_\-$]+)$')
        for d in final_docs:
            idx_name = d.get('_index','')
            if not any(h in idx_name for h in CALL_INDEX_HINTS):
                continue
            txt_field = d.get('_text_field') or TEXT_FIELD_DEFAULT
            current = (d.get(txt_field) or '').strip()
            # Heuristic: if current text is very short or equals key, build synthetic snippet
            if not current or len(current) < 12 or current == d.get('_key'):
                parts = []
                data_keys = set(d.keys())
                # Build a structured summary
                # Identify caller / callee
                caller = None
                callee = None
                for ck in ['caller_program','calling_program','program_id','source_program','caller','from_program']:
                    if ck in d and d[ck]:
                        caller = d[ck]; break
                for tk in ['callee_program','called_program','target_program','target','callee','name']:
                    if tk in d and d[tk]:
                        callee = d[tk]; break
                if caller or callee:
                    parts.append(f"CALL: {caller or '?'} -> {callee or '?'}")
                # Add paragraph / scope context
                for pk in ['scope','paragraph','para']:
                    if pk in d and d[pk] and d[pk] not in (caller, callee):
                        parts.append(f"CTX:{pk}={d[pk]}")
                # Add line range
                sl = d.get('start_line') or d.get('line') or d.get('line_number')
                el = d.get('end_line')
                if sl and el and sl != el:
                    parts.append(f"LINES:{sl}-{el}")
                elif sl:
                    parts.append(f"LINE:{sl}")
                # Add path if present
                if 'path' in d and d['path']:
                    parts.append(f"FILE:{d['path']}")
                if not parts:
                    # fallback enumerating a few candidate fields
                    extracted = []
                    for cand in call_field_candidates_priority:
                        if cand in d and d[cand] and d[cand] not in extracted:
                            extracted.append(f"{cand}={d[cand]}")
                        if len(extracted) >= 4:
                            break
                    if extracted:
                        parts = extracted
                # If still nothing meaningful, attempt to parse key pattern like f001016-206-ACUMEM
                if not parts:
                    k = d.get('_key','')
                    m = key_pattern.match(k)
                    if m:
                        file_token, line_no, callee_guess = m.groups()
                        # treat callee_guess as callee if not already discovered
                        if not callee:
                            callee = callee_guess
                        parts.append(f"CALL:? -> {callee or callee_guess}")
                        parts.append(f"LINE:{line_no}")
                        parts.append(f"TOKEN:{file_token}")
                        parts.append(f"KEY:{k}")
                if parts:
                    synthesized = ' | '.join(parts)[:700]
                    d['_synth_snippet'] = synthesized
                else:
                    d['_synth_snippet'] = current  # nothing better

    # Optional enrichment: For xref read/write entries pull symbol definition doc
    if args.enrich_records:
        # Heuristic: keys like f000001-199-TCLP-ACUMEM-REC-write
        import re
        xref_pattern = re.compile(r'^[A-Za-z]\d+-\d+-([A-Za-z0-9_\-]+)-(read|write)$', re.IGNORECASE)
        symbol_added_keys = set()
        enriched_docs: List[Dict[str,Any]] = []
        # Provide lightweight cache to avoid duplicate symbol lookups
        symbol_cache: Dict[str,Dict[str,Any]] = {}
        def fetch_symbol(symbol_name: str) -> Dict[str,Any] | None:
            if symbol_name in symbol_cache:
                return symbol_cache[symbol_name]
            symbol_index = 'cobol-symbols'
            try:
                flds = discover_fields(cfg, symbol_index)
                txt_field = flds['text_field']
                url = f"{cfg['search_endpoint']}/indexes/{symbol_index}/docs/search?api-version={API_VERSION_SEARCH}"
                body = {"search": symbol_name, "top": 1, "select": txt_field + ",path,program_id"}
                r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
                if r.status_code != 200:
                    if args.show_debug:
                        print(f"[DEBUG] Symbol fetch failed for {symbol_name}: {r.status_code}")
                    symbol_cache[symbol_name] = None
                    return None
                vals = r.json().get('value', [])
                if not vals:
                    symbol_cache[symbol_name] = None
                    return None
                doc = vals[0]
                doc['_index'] = symbol_index
                doc['_key'] = choose_key(doc)
                doc['_text_field'] = txt_field
                symbol_cache[symbol_name] = doc
                return doc
            except Exception as ex:
                if args.show_debug:
                    print(f"[DEBUG] Exception symbol fetch {symbol_name}: {ex}")
                symbol_cache[symbol_name] = None
                return None

        for d in list(final_docs):  # iterate over snapshot
            if d.get('_index') not in ('cobol-xrefs','xrefs'):
                continue
            k = d.get('_key','')
            m = xref_pattern.match(k)
            if not m:
                continue
            sym_name = m.group(1)
            if sym_name in symbol_added_keys:
                continue
            sym_doc = fetch_symbol(sym_name)
            if sym_doc:
                # Create concise synthetic snippet (first 300 chars)
                txt_field = sym_doc.get('_text_field') or TEXT_FIELD_DEFAULT
                snippet = (sym_doc.get(txt_field) or '')[:300].replace('\n',' ')
                clone = dict(sym_doc)
                clone['_synth_snippet'] = f"DEF:{sym_name} {snippet}"
                enriched_docs.append(clone)
                symbol_added_keys.add(sym_name)
                if args.show_debug:
                    print(f"[DEBUG] Enriched with symbol definition for {sym_name}")
        # Append enriched docs while respecting final_top limit (replace tail if needed)
        if enriched_docs:
            # Keep existing order, just extend and then trim by final_top
            final_docs.extend(enriched_docs)
            # Deduplicate by (index,key)
            seen_pairs = set()
            deduped = []
            for dd in final_docs:
                pair = (dd.get('_index'), dd.get('_key'))
                if pair in seen_pairs:
                    continue
                seen_pairs.add(pair)
                deduped.append(dd)
            final_docs = deduped[:args.final_top]
            # Rebuild prompt_ready_docs after enrichment later

    # Optional logic enrichment: fetch related code-chunks for structural docs
    if args.enrich_logic:
        # Only attempt if code-chunks index exists (was auto-added earlier OR user specified)
        if 'code-chunks' in indexes:
            try:
                chunk_fields = discover_fields(cfg, 'code-chunks')
                chunk_text_field = chunk_fields['text_field']
                # Build quick lookup to avoid repeated identical searches (keyed by (path,start_line,end_line) bucket)
                logic_cache: Dict[str, Dict[str,Any]] = {}
                injected_logic: List[Dict[str,Any]] = []
                def fetch_logic(path: str, line_hint: int|None):
                    if not path:
                        return None
                    bucket_key = f"{path}:{line_hint or 'NA'}"
                    if bucket_key in logic_cache:
                        return logic_cache[bucket_key]
                    # Strategy: lexical search on path plus maybe line number token if stored; fallback to path only
                    url = f"{cfg['search_endpoint']}/indexes/code-chunks/docs/search?api-version={API_VERSION_SEARCH}"
                    # Use small query to keep cost low
                    search_terms = path
                    body = {"search": search_terms, "top": 3, "select": f"{chunk_text_field},path,start_line,end_line,program_id"}
                    r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
                    if r.status_code != 200:
                        logic_cache[bucket_key] = None
                        return None
                    vals = r.json().get('value', [])
                    # Heuristic pick: choose doc whose line range bracket contains line_hint if available, else first
                    chosen = None
                    if line_hint is not None:
                        for cand in vals:
                            sl = cand.get('start_line'); el = cand.get('end_line') or sl
                            if isinstance(sl,int) and isinstance(el,int) and sl <= line_hint <= el:
                                chosen = cand; break
                    if not chosen and vals:
                        chosen = vals[0]
                    if chosen:
                        chosen['_index'] = 'code-chunks'
                        chosen['_key'] = choose_key(chosen)
                        chosen['_text_field'] = chunk_text_field
                    logic_cache[bucket_key] = chosen
                    return chosen
                # Iterate current final docs; for structural ones fetch logic
                STRUCTURAL = {'cobol-paragraphs','cobol-calls','cobol-xrefs'}
                for d in list(final_docs):
                    idxn = d.get('_index')
                    if idxn not in STRUCTURAL:
                        continue
                    path = d.get('path') or ''
                    # choose representative line
                    line_hint = d.get('start_line') or d.get('line') or d.get('line_number')
                    logic_doc = fetch_logic(path, line_hint if isinstance(line_hint,int) else None)
                    if logic_doc:
                        # Create a concise synth snippet referencing original structural doc key
                        txt_field = logic_doc.get('_text_field') or chunk_text_field
                        raw = (logic_doc.get(txt_field) or '')[:500].replace('\n',' ')
                        clone = dict(logic_doc)
                        clone['_synth_snippet'] = f"LOGIC:{os.path.basename(path)}:{line_hint or '?'} {raw}"
                        injected_logic.append(clone)
                if injected_logic:
                    # Append and then dedupe + trim
                    final_docs.extend(injected_logic)
                    seen_pairs = set()
                    new_list = []
                    for dd in final_docs:
                        pair = (dd.get('_index'), dd.get('_key'))
                        if pair in seen_pairs:
                            continue
                        seen_pairs.add(pair)
                        new_list.append(dd)
                    # Keep earlier ordering priority; logic injections added near tail
                    final_docs = new_list[:args.final_top]
                    if args.show_debug:
                        print(f"[DEBUG] Logic enrichment injected {len(injected_logic)} chunk doc(s)")
            except Exception as ex:
                if args.show_debug:
                    print(f"[DEBUG] Logic enrichment failed: {ex}")
        else:
            if args.show_debug:
                print("[DEBUG] Skipping --enrich-logic because 'code-chunks' not in active index list")

    # Arithmetic fallback injection
    if args.ensure_arithmetic:
        import re
        # Detect presence of arithmetic verbs in current final_docs
        arith_re = re.compile(r'\b(ADD|COMPUTE|MULTIPLY|DIVIDE|SUBTRACT)\b', re.IGNORECASE)
        def doc_has_arith(doc):
            txt_field = doc.get('_text_field') or TEXT_FIELD_DEFAULT
            raw = (doc.get('_text_override') or doc.get(txt_field) or '')
            return bool(arith_re.search(raw))
        have_arith = any(doc_has_arith(d) for d in final_docs)
        if args.show_debug:
            print(f"[DEBUG] Arithmetic detection before fallback: have_arith={have_arith}")
        if not have_arith and 'code-chunks' in indexes:
            # Fallback lexical searches on verbs (scoped by probable program token if present in query)
            probable_program = None
            # Simple heuristic: uppercase token of length 5-12 in query
            for tok in re.findall(r'[A-Z][A-Z0-9]{3,}', args.query):
                if tok not in ('ADD','COMPUTE','MULTIPLY','DIVIDE','SUBTRACT','ACCUMULATED','USAGE','TOTAL','ARITHMETIC'):
                    probable_program = tok
                    break
            verbs = ["ADD","COMPUTE","MULTIPLY","DIVIDE","SUBTRACT"]
            fallback_docs = []
            def lexical_search(cfg, index: str, search_text: str, top: int, select_extra: str):
                flds = discover_fields(cfg, index)
                text_field = flds['text_field']
                url = f"{cfg['search_endpoint']}/indexes/{index}/docs/search?api-version={API_VERSION_SEARCH}"
                body = {"search": search_text, "top": top, "select": f"{text_field},path,start_line,end_line,program_id"}
                r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
                if r.status_code != 200:
                    if args.show_debug:
                        print(f"[DEBUG] Lexical fallback search failed ({search_text}): {r.status_code}")
                    return []
                vals = r.json().get('value', [])
                for v in vals:
                    v['_index'] = index
                    v['_key'] = choose_key(v)
                    v['_text_field'] = text_field
                return vals
            seen_pairs = {(d.get('_index'), d.get('_key')) for d in final_docs}
            for verb in verbs:
                if any(verb in (d.get('_text_override') or d.get(d.get('_text_field') or TEXT_FIELD_DEFAULT,'')).upper() for d in final_docs):
                    continue  # already have this verb
                search_term = f"{probable_program} {verb}" if probable_program else verb
                docs = lexical_search(cfg, 'code-chunks', search_term, 6, args.select_extra)
                for d in docs:
                    pair = (d.get('_index'), d.get('_key'))
                    if pair in seen_pairs:
                        continue
                    # Keep only docs that actually contain arithmetic verb
                    if not doc_has_arith(d):
                        continue
                    fallback_docs.append(d)
                    seen_pairs.add(pair)
                if fallback_docs:
                    break  # stop after first successful verb injection
            if fallback_docs:
                # Assign high rerank scores to ensure retention
                max_rr = max([fd.get('_rerank_score', fd.get('_norm_score',0.0)) for fd in final_docs] + [0])
                for d in fallback_docs:
                    txt_field = d.get('_text_field') or TEXT_FIELD_DEFAULT
                    raw_txt = (d.get(txt_field) or '')[:500].replace('\n',' ')
                    d['_rerank_score'] = max_rr + 0.05
                    d['_synth_snippet'] = f"ARITH:{raw_txt}"
                    d['_text_override'] = d['_synth_snippet']
                final_docs.extend(fallback_docs)
                # Re-trim respecting final_top while keeping highest rerank
                final_docs.sort(key=lambda x: x.get('_rerank_score', x.get('_norm_score',0.0)), reverse=True)
                final_docs[:] = final_docs[:args.final_top]
                if args.show_debug:
                    print(f"[DEBUG] Injected {len(fallback_docs)} arithmetic fallback doc(s) using verbs={verbs}")
            else:
                if args.show_debug:
                    print("[DEBUG] Arithmetic fallback search found nothing relevant.")
        elif not have_arith and args.show_debug:
            print("[DEBUG] Arithmetic fallback skipped: 'code-chunks' index not available.")

    # Flow summarization (Option B) – executed after logic/arithmetic enrichment so we can see context gaps
    if args.flow_summarize:
        # Only attempt if flow edges index exists in service (do not require user to list it among --indexes)
        try:
            # Lightweight service index list (cached) to confirm presence
            url_list = f"{cfg['search_endpoint']}/indexes?api-version={API_VERSION_SEARCH}"
            r_list = requests.get(url_list, headers={'api-key':cfg['search_key']}, timeout=20)
            flow_available = False
            if r_list.status_code == 200:
                flow_available = any(it.get('name') == 'cobol-flow-edges-v2' for it in r_list.json().get('value', []))
            if flow_available:
                focus_token = args.flow_program.strip()
                if not focus_token:
                    # Try to infer from query: grab uppercase alphanumeric token containing TIM or ending with 360/365
                    import re
                    cand = re.findall(r'[A-Z][A-Z0-9-]{2,}', args.query.upper())
                    for c in cand:
                        if 'TIM' in c or c.endswith('360') or c.endswith('365'):
                            focus_token = c
                            break
                # Build search terms: include variants with dash removal
                search_terms = focus_token or 'TIM360'
                variants = []
                if focus_token and '-' in focus_token:
                    variants.append(focus_token.replace('-',''))
                if focus_token and 'TIM' in focus_token and '360' not in focus_token:
                    variants.append('TIM360')
                # Aggregate lexical tries
                def fetch_edges(term: str):
                    # Only select fields that exist in schema (create_indexes.py) – path/program_id not defined here
                    url = f"{cfg['search_endpoint']}/indexes/cobol-flow-edges-v2/docs/search?api-version={API_VERSION_SEARCH}"
                    body = {"search": term, "top": min(args.flow_max_edges, 200), "select": "edge_id,caller_para,target_para,line,kind,file_id"}
                    rr = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
                    if rr.status_code != 200:
                        return []
                    vals = rr.json().get('value', [])
                    for v in vals:
                        v['_index'] = 'cobol-flow-edges-v2'
                        v['_key'] = v.get('edge_id') or choose_key(v)
                        v['_text_field'] = 'caller_para'  # placeholder for snippet injection
                    return vals
                edge_docs = []
                seen_keys = set()
                for t in [search_terms] + variants:
                    docs = fetch_edges(t)
                    for d in docs:
                        k = d.get('_key')
                        if k in seen_keys:
                            continue
                        seen_keys.add(k)
                        edge_docs.append(d)
                    if len(edge_docs) >= args.flow_max_edges:
                        break
                # Fallback broadening: if still empty, attempt additional heuristic searches
                if not edge_docs:
                    broaden_terms = []
                    base = focus_token or 'TIM360'
                    # If token contains digits, try stripping trailing digits to capture generic prefix (e.g., TIM360 -> TIM)
                    import re as _re
                    m = _re.match(r'([A-Z]+)\d+$', base)
                    if m:
                        broaden_terms.append(m.group(1))
                    # Try hyphen split pieces
                    if '-' in base:
                        broaden_terms.extend([p for p in base.split('-') if len(p) > 2])
                    # Always add wildcard * search as last resort
                    broaden_terms.append('*')
                    tried = set()
                    for bt in broaden_terms:
                        if bt in tried:
                            continue
                        tried.add(bt)
                        docs = fetch_edges(bt)
                        for d in docs:
                            k = d.get('_key')
                            if k in seen_keys:
                                continue
                            # Light client-side relevance: keep only edges whose caller/target contains base prefix when bt='*'
                            if bt == '*' and base and (base[:3] not in (d.get('caller_para','')) and base[:3] not in (d.get('target_para',''))):
                                continue
                            seen_keys.add(k)
                            edge_docs.append(d)
                        if len(edge_docs) >= max(10, args.flow_max_edges // 2):  # stop early if we gathered a minimum set
                            break
                # If still empty, derive heuristic seeds from already retrieved paragraph / chunk keys
                if not edge_docs:
                    para_like_tokens = set()
                    for d in final_docs:
                        if d.get('_index') in ('cobol-paragraphs','paragraphs') or (d.get('_index') == 'code-chunks' and 'TIMER' in (d.get('_text_override') or d.get(d.get('_text_field') or TEXT_FIELD_DEFAULT,''))):
                            k = d.get('_key','')
                            for part in k.split('-'):
                                if part.isalpha() and 3 <= len(part) <= 14:
                                    para_like_tokens.add(part.upper())
                    # Prioritize those containing parts of focus token or generic TIMER
                    heur_order = []
                    for t in sorted(para_like_tokens):
                        if focus_token and focus_token[:3] in t:
                            heur_order.append(t)
                    if 'TIMER' in para_like_tokens and 'TIMER' not in heur_order:
                        heur_order.append('TIMER')
                    # Limit attempts
                    for h in heur_order[:6]:
                        docs = fetch_edges(h)
                        for d in docs:
                            k = d.get('_key')
                            if k in seen_keys:
                                continue
                            seen_keys.add(k); edge_docs.append(d)
                        if len(edge_docs) >= args.flow_max_edges:
                            break
                if edge_docs:
                    if args.flow_debug_dump > 0 and args.show_debug:
                        print(f"[DEBUG] Flow debug dump (first {args.flow_debug_dump} edge docs):")
                        for e in edge_docs[:args.flow_debug_dump]:
                            print(f"  EDGE {e.get('_key')} {e.get('caller_para')} -> {e.get('target_para')} kind={e.get('kind')} line={e.get('line')}")
                    # Build adjacency
                    from collections import defaultdict, deque
                    outgoing = defaultdict(set)
                    incoming = defaultdict(set)
                    nodes = set()
                    for e in edge_docs:
                        c = e.get('caller_para') or ''
                        t = e.get('target_para') or ''
                        if c and t:
                            outgoing[c].add(t)
                            incoming[t].add(c)
                            nodes.add(c); nodes.add(t)
                    # Identify roots: nodes with no incoming, prefer ones containing focus token (case-insensitive)
                    focus_lower = (focus_token or '').lower()
                    roots = [n for n in nodes if not incoming[n]] or list(nodes)
                    if focus_lower:
                        prioritized = [n for n in roots if focus_lower in n.lower()]
                        roots = prioritized or roots
                    # BFS limited traversal to produce ordered chains (avoid explosion)
                    chains = []
                    max_chain = 12
                    for r in roots[:4]:
                        visited = set([r])
                        q = deque([(r, [r])])
                        while q and len(chains) < 6:
                            node, path = q.popleft()
                            children = sorted(list(outgoing.get(node, [])))
                            extended = False
                            for ch in children[:5]:
                                if ch in visited:
                                    continue
                                visited.add(ch)
                                new_path = path + [ch]
                                if len(new_path) <= max_chain:
                                    q.append((ch, new_path))
                                    extended = True
                            if not extended and len(path) > 1:
                                chains.append(path)
                    # Deduplicate chain list
                    uniq = []
                    seen_sig = set()
                    for ch in chains:
                        sig = tuple(ch)
                        if sig in seen_sig:
                            continue
                        seen_sig.add(sig)
                        uniq.append(ch)
                    chains = uniq[:5]
                    # Build summary lines
                    summary_lines = []
                    if focus_token:
                        summary_lines.append(f"FLOW for {focus_token} (edges={len(edge_docs)} roots={len(roots)}):")
                    else:
                        summary_lines.append(f"FLOW summary (edges={len(edge_docs)} roots={len(roots)}):")
                    if not chains:
                        # fallback: list top outgoing adjacency counts
                        degree = sorted(((n, len(outgoing.get(n, []))) for n in nodes), key=lambda x: x[1], reverse=True)[:8]
                        summary_lines.append("Key nodes:" + ", ".join(f"{n}->{d}" for n,d in degree))
                    else:
                        for i,ch in enumerate(chains,1):
                            summary_lines.append(f"Path{i}: " + " -> ".join(ch))
                    # Inject synthetic flow doc
                    flow_doc = {
                        '_index': 'cobol-flow-edges-v2',
                        '_key': f"FLOW-{focus_token or 'GLOBAL'}",
                        '_text_field': 'caller_para',
                        '_synth_snippet': ("FLOW:" + " | ".join(summary_lines))[:900],
                        '_rerank_score': max([d.get('_rerank_score', d.get('_norm_score',0.0)) for d in final_docs] + [0]) + 0.04
                    }
                    flow_doc['_text_override'] = flow_doc['_synth_snippet']
                    final_docs.append(flow_doc)
                    # Re-trim preserving high rerank
                    final_docs.sort(key=lambda x: x.get('_rerank_score', x.get('_norm_score',0.0)), reverse=True)
                    final_docs[:] = final_docs[:args.final_top]
                    if args.show_debug:
                        print(f"[DEBUG] Flow summarizer injected flow doc (edges={len(edge_docs)}) focus={focus_token}")
                else:
                    if args.show_debug:
                        print("[DEBUG] Flow summarizer found no edges for focus token.")
            else:
                if args.show_debug:
                    print("[DEBUG] Flow summarizer skipped: 'cobol-flow-edges-v2' index not present.")
        except Exception as ex:
            if args.show_debug:
                print(f"[DEBUG] Flow summarizer error: {ex}")

    # Facts fallback (Option A) – inject synthesized fact-level evidence when logic context thin
    if args.facts_fallback:
        try:
            # Determine if we are missing rich logic: count chunk docs and inspect verbs
            import re as _re
            logic_verb_re = _re.compile(r'\b(ADD|COMPUTE|MULTIPLY|DIVIDE|SUBTRACT|PERFORM|CALL|EVALUATE|GO\s+TO)\b', _re.IGNORECASE)
            def doc_has_logic(d):
                txt_field = d.get('_text_field') or TEXT_FIELD_DEFAULT
                raw = (d.get('_text_override') or d.get(txt_field) or '')
                return bool(logic_verb_re.search(raw))
            chunk_docs = [d for d in final_docs if d.get('_index') == 'code-chunks']
            have_logic_verb = any(doc_has_logic(d) for d in chunk_docs)
            # Trigger if no chunk docs OR no verbs OR fewer than 2 paragraph docs
            para_docs = [d for d in final_docs if d.get('_index') in ('cobol-paragraphs','paragraphs')]
            trigger_facts = (len(chunk_docs) == 0) or (not have_logic_verb) or (len(para_docs) < 2)
            if trigger_facts:
                # Check index existence
                url_list = f"{cfg['search_endpoint']}/indexes?api-version={API_VERSION_SEARCH}"
                r_list = requests.get(url_list, headers={'api-key':cfg['search_key']}, timeout=15)
                if r_list.status_code == 200 and any(it.get('name') == 'cobol-facts' for it in r_list.json().get('value', [])):
                    # Extract candidate search terms from query (uppercase tokens) and paragraph keys
                    terms = []
                    for tok in _re.findall(r'[A-Z][A-Z0-9-]{3,}', args.query.upper()):
                        if tok not in terms and len(terms) < 4:
                            terms.append(tok)
                    # Add stems from paragraph keys (portion after first '-')
                    for d in para_docs:
                        k = d.get('_key','')
                        parts = k.split('-')
                        if len(parts) >= 2:
                            cand = parts[1].upper()
                            if 3 <= len(cand) <= 16 and cand not in terms and len(terms) < 6:
                                terms.append(cand)
                    if not terms:
                        terms = ['*']
                    def search_facts(term: str):
                        url = f"{cfg['search_endpoint']}/indexes/cobol-facts/docs/search?api-version={API_VERSION_SEARCH}"
                        body = {"search": term, "top": 12, "select": "fact_id,para,line,kind,snippet,callee,target"}
                        rr = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
                        if rr.status_code != 200:
                            return []
                        vals = rr.json().get('value', [])
                        for v in vals:
                            v['_index'] = 'cobol-facts'; v['_key'] = v.get('fact_id') or choose_key(v); v['_text_field'] = 'snippet'
                        return vals
                    fact_docs = []
                    seen_fact = set()
                    for t in terms:
                        docs = search_facts(t)
                        for fd in docs:
                            if fd['_key'] in seen_fact:
                                continue
                            seen_fact.add(fd['_key'])
                            fact_docs.append(fd)
                        if len(fact_docs) >= 18:  # cap raw collection
                            break
                    # Light scoring: prioritize facts with CALL/PERFORM/EVALUATE kind or snippet verbs
                    def fact_priority(fd):
                        snip = (fd.get('snippet') or '').upper()
                        kind = (fd.get('kind') or '').upper()
                        score = 0
                        if any(k in kind for k in ('CALL','PERFORM','EVALUATE','COMPUTE','ADD','SUBTRACT','MULTIPLY','DIVIDE')):
                            score += 3
                        if any(v in snip for v in ('ADD','COMPUTE','PERFORM','CALL','EVALUATE')):
                            score += 2
                        return score
                    fact_docs.sort(key=fact_priority, reverse=True)
                    injected = []
                    max_inject = 4
                    current_max_rr = max([d.get('_rerank_score', d.get('_norm_score',0.0)) for d in final_docs] + [0])
                    boost_step = 0.03
                    for i,fd in enumerate(fact_docs[:max_inject]):
                        para = fd.get('para') or '?'
                        kind = fd.get('kind') or ''
                        line = fd.get('line')
                        snip = (fd.get('snippet') or '')[:300].replace('\n',' ')
                        synth = f"FACT:{para}:{line or '?'}:{kind} {snip}"[:700]
                        fd['_synth_snippet'] = synth
                        fd['_text_override'] = synth
                        fd['_rerank_score'] = current_max_rr + boost_step * (i+1)
                        injected.append(fd)
                    if injected:
                        final_docs.extend(injected)
                        # Re-trim by rerank
                        final_docs.sort(key=lambda x: x.get('_rerank_score', x.get('_norm_score',0.0)), reverse=True)
                        final_docs[:] = final_docs[:args.final_top]
                        if args.show_debug:
                            print(f"[DEBUG] Facts fallback injected {len(injected)} fact doc(s) using terms={terms}")
                else:
                    if args.show_debug:
                        print("[DEBUG] Facts fallback skipped: 'cobol-facts' index not present.")
            else:
                if args.show_debug:
                    print("[DEBUG] Facts fallback not triggered (sufficient logic coverage detected).")
        except Exception as ex:
            if args.show_debug:
                print(f"[DEBUG] Facts fallback error: {ex}")

    # Inject synthesized snippet into prompt build by temporarily overriding text via _text_override
    for d in final_docs:
        if '_synth_snippet' in d:
            d['_text_override'] = d['_synth_snippet']

    # Adapt build_prompt to prefer _text_override if present by shallow copy transformation
    prompt_ready_docs = []
    for d in final_docs:
        # choose override precedence: _text_override > _synth_snippet
        if '_text_override' in d:
            clone = dict(d)
            txt_field = clone.get('_text_field') or TEXT_FIELD_DEFAULT
            clone[txt_field] = d['_text_override']
            prompt_ready_docs.append(clone)
        elif '_synth_snippet' in d:
            clone = dict(d)
            txt_field = clone.get('_text_field') or TEXT_FIELD_DEFAULT
            clone[txt_field] = d['_synth_snippet']
            prompt_ready_docs.append(clone)
        else:
            prompt_ready_docs.append(d)

    prompt = build_prompt(args.query, prompt_ready_docs, args.prompt_max_chars, args.final_top, history[-args.turns:])
    if args.show_debug:
        print("--- Prompt Preview (truncated) ---")
        print(prompt[:1500])
        print("--- End Prompt Preview ---")

    answer = chat(cfg, prompt, args.temperature, args.max_tokens, history[-args.turns:])
    print("\n=== Answer ===\n")
    print(answer)
    print("\n=== Citations (index|key) ===")
    for d in final_docs:
        print(f"{d.get('_index')}|{d.get('_key')}")
    if args.show_debug:
        # Emit synthetic snippet info if present
        for d in final_docs:
            if '_synth_snippet' in d:
                print(f"[DEBUG] SynthSnippet idx={d.get('_index')} key={d.get('_key')}: {d.get('_synth_snippet')}")

    # Update conversation history
    new_turns = [
        {"role":"user","content": args.query},
        {"role":"assistant","content": answer}
    ]
    history.extend(new_turns)
    # Trim
    if len(history) > args.turns * 2 + 4:
        history = history[-(args.turns * 2 + 4):]
    save_session(args.session, history)
    print(f"Session stored ({args.session})" if args.session else "(no session persistence)")


if __name__ == '__main__':
    main()
