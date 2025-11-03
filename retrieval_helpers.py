#!/usr/bin/env python3
"""Helper utilities for chatbot retrieval over COBOL indexes.

Responsibilities:
  - Normalize user routine / paragraph queries
  - Lookup alias doc in cobol-routine-aliases
  - Expand to candidate target paragraph names
  - Build search queries for:
      * cobol-flow-edges-v2 (by raw_target, family_key, resolved_target_para)
      * cobol-paragraphs (paragraph definitions)
      * cobol-calls (to show outbound calls from candidate routines)
  - Provide a high-level function returning a structured retrieval bundle for answer synthesis.

This module uses direct REST calls (requests) to avoid adding SDK dependencies.

Environment:
  AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY (or SEARCH_*)

Example:
  from retrieval_helpers import retrieve_routine_context
  bundle = retrieve_routine_context("TIM360")
  print(bundle['summary_inputs'])
"""
from __future__ import annotations
import os, json, time, re, requests, math
from typing import Dict, List, Optional, Tuple

try:  # lazy embedding import
    from embedding_utils import batch_embed, provider_info  # type: ignore
except Exception:
    batch_embed = None  # type: ignore
    provider_info = lambda: 'unavailable'  # type: ignore

API_VERSION = "2024-07-01"
ALIAS_INDEX = "cobol-routine-aliases"
FLOW_INDEX = "cobol-flow-edges-v2"
PARA_INDEX = "cobol-paragraphs"
CALL_INDEX = "cobol-calls"

# Hybrid / vector retrieval configuration (env overrides)
FLOW_EDGES_HYBRID = os.getenv('FLOW_EDGES_VECTOR_HYBRID','').lower() in {'1','true','yes'}
FLOW_VECTOR_FIELD = os.getenv('FLOW_EDGES_VECTOR_FIELD','edge_vector')
FLOW_TEXT_FIELD = os.getenv('FLOW_EDGES_TEXT_FIELD','edge_text')
FLOW_VECTOR_K = int(os.getenv('FLOW_EDGES_VECTOR_K','50'))
FLOW_VECTOR_WEIGHT = float(os.getenv('FLOW_EDGES_VECTOR_WEIGHT','1.0'))
FLOW_KEYWORD_WEIGHT = float(os.getenv('FLOW_EDGES_KEYWORD_WEIGHT','1.0'))
FLOW_VECTOR_REQUIRE_FLAG = os.getenv('FLOW_EDGES_VECTOR_REQUIRE_FLAG','true').lower() in {'1','true','yes'}
FLOW_VECTOR_FLAG_FIELD = os.getenv('FLOW_EDGES_VECTOR_FLAG_FIELD','has_vector')

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]

def _load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT') or vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT') or '').rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY') or vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    if not endpoint or not key:
        raise RuntimeError('Missing Azure Search endpoint/key')
    return endpoint, key

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    t = re.sub(r"[^A-Z0-9]+","-", t)
    return t.strip('-')

def _search(index: str, body: Dict, endpoint: str, key: str) -> Dict:
    url = f"{endpoint}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code != 200:
        return {"error": r.text, "value": []}
    return r.json()

def _vector_search_flow(query_text: str, endpoint: str, key: str, family_key: Optional[str], k: int) -> List[Dict]:
    if batch_embed is None:
        return []
    try:
        vec = batch_embed([query_text], batch_size=1)[0]
    except Exception:
        return []
    body: Dict = {
        "select": f"edge_id,raw_target,resolved_target_para,caller_para,edge_subkind,resolution_strategy,{FLOW_TEXT_FIELD}",
        "vectorQueries": [
            {"kind":"vector", "vector": vec, "fields": FLOW_VECTOR_FIELD, "k": k}
        ]
    }
    filters = []
    if family_key:
        filters.append(f"family_key eq '{family_key}'")
    if FLOW_VECTOR_REQUIRE_FLAG:
        filters.append(f"{FLOW_VECTOR_FLAG_FIELD} eq true")
    if filters:
        body['filter'] = ' and '.join(filters)
    res = _search(FLOW_INDEX, body, endpoint, key)
    return res.get('value',[])

def lookup_alias(norm_query: str, endpoint: str, key: str) -> Optional[Dict]:
    body = {"search": norm_query, "top": 5, "filter": f"alias eq '{norm_query}'"}
    res = _search(ALIAS_INDEX, body, endpoint, key)
    vals = res.get('value',[])
    for v in vals:
        if v.get('alias') == norm_query:
            return v
    return vals[0] if vals else None

def fetch_flow_edges(candidates: List[str], family_key: Optional[str], endpoint: str, key: str, limit: int = 200) -> List[Dict]:
    search_terms = ' OR '.join(set(candidates)) if candidates else '*'
    body = {"search": search_terms, "top": limit}
    if family_key:
        body['filter'] = f"family_key eq '{family_key}'"
    res = _search(FLOW_INDEX, body, endpoint, key)
    return res.get('value',[])

def _merge_hybrid(keyword_edges: List[Dict], vector_edges: List[Dict]) -> Tuple[List[Dict], Dict[str,float]]:
    scores: Dict[str,float] = {}
    for rank,d in enumerate(keyword_edges, start=1):
        eid=d.get('edge_id')
        if not eid: continue
        base = d.get('@search.score') or (1.0/(rank+5))
        scores[eid] = max(scores.get(eid,0.0), base * FLOW_KEYWORD_WEIGHT)
    for rank,d in enumerate(vector_edges, start=1):
        eid=d.get('edge_id')
        if not eid: continue
        vscore = d.get('@search.score') or (1.0/(rank+5))
        scores[eid] = max(scores.get(eid,0.0), vscore * FLOW_VECTOR_WEIGHT)
    merged: Dict[str,Dict] = {d.get('edge_id'): d for d in keyword_edges if d.get('edge_id')}
    for d in vector_edges:
        eid=d.get('edge_id')
        if not eid: continue
        if eid in merged:
            if FLOW_TEXT_FIELD in d and FLOW_TEXT_FIELD not in merged[eid]:
                merged[eid][FLOW_TEXT_FIELD]=d[FLOW_TEXT_FIELD]
        else:
            merged[eid]=d
    ordered = sorted(merged.values(), key=lambda x: scores.get(x.get('edge_id'),0.0), reverse=True)
    return ordered, scores

def fetch_paragraph_defs(candidates: List[str], endpoint: str, key: str, limit: int = 50) -> List[Dict]:
    para_terms = ' OR '.join(set(candidates)) if candidates else '*'
    body = {"search": para_terms, "top": limit}
    res = _search(PARA_INDEX, body, endpoint, key)
    return res.get('value',[])

def fetch_calls(paras: List[str], endpoint: str, key: str, limit: int = 100) -> List[Dict]:
    if not paras:
        return []
    call_terms = ' OR '.join(set(paras))
    body = {"search": call_terms, "top": limit}
    res = _search(CALL_INDEX, body, endpoint, key)
    return res.get('value',[])

def retrieve_routine_context(user_query: str, max_flow: int = 200, *, use_hybrid: Optional[bool] = None) -> Dict:
    endpoint, key = _load_config()
    norm = normalize_token(user_query)
    alias_doc = lookup_alias(norm, endpoint, key)

    candidate_paras: List[str] = []
    family_key = None
    if alias_doc:
        family_key = alias_doc.get('family_key')
        # canonical + explicit candidates + alias itself
        candidate_paras.extend([alias_doc.get('canonical_target')] if alias_doc.get('canonical_target') else [])
        candidate_paras.extend(alias_doc.get('candidate_targets') or [])
    # Always include raw normalized form (some edges only have raw_target)
    candidate_paras.append(norm)
    candidate_paras = [c for c in {c for c in candidate_paras if c}]

    flow_edges = fetch_flow_edges(candidate_paras, family_key, endpoint, key, limit=max_flow)
    hybrid_enabled = (FLOW_EDGES_HYBRID if use_hybrid is None else use_hybrid)
    vector_edges: List[Dict] = []
    merged_edges: List[Dict] = flow_edges
    hybrid_scores: Dict[str,float] = {}
    if hybrid_enabled and batch_embed is not None:
        try:
            vector_edges = _vector_search_flow(user_query, endpoint, key, family_key, FLOW_VECTOR_K)
            merged_edges, hybrid_scores = _merge_hybrid(flow_edges, vector_edges)
        except Exception:
            vector_edges = []
            merged_edges = flow_edges
    para_defs = fetch_paragraph_defs(candidate_paras, endpoint, key)
    calls = fetch_calls(candidate_paras, endpoint, key)

    bundle = {
        'query': user_query,
        'normalized': norm,
        'alias_doc': alias_doc,
        'candidate_paragraphs': candidate_paras,
        'flow_edges': flow_edges,
        'flow_edges_vector': vector_edges,
        'flow_edges_merged': merged_edges,
        'hybrid_used': hybrid_enabled,
        'hybrid_scores': hybrid_scores if hybrid_enabled else {},
        'paragraphs': para_defs,
        'calls': calls,
        'summary_inputs': {
            'routine_name': user_query,
            'family_key': family_key,
            'paragraph_count': len(para_defs),
            'edge_count': len(flow_edges),
            'vector_edge_count': len(vector_edges),
            'call_count': len(calls),
        }
    }
    return bundle

if __name__ == '__main__':
    import argparse, pprint
    ap = argparse.ArgumentParser()
    ap.add_argument('query', help='Routine or paragraph token (e.g., TIM360)')
    ap.add_argument('--max-flow', type=int, default=200)
    ap.add_argument('--hybrid', action='store_true', help='Force hybrid (keyword + vector) retrieval')
    args = ap.parse_args()
    pp = pprint.PrettyPrinter(depth=3, width=130)
    out = retrieve_routine_context(args.query, max_flow=args.max_flow, use_hybrid=(True if args.hybrid else None))
    pp.pprint(out['summary_inputs'])
    print(f"Candidates: {out['candidate_paragraphs']}")
    print(f"Flow edges returned: {len(out['flow_edges'])}")
    if out.get('hybrid_used'):
        print(f"Vector edges returned: {len(out.get('flow_edges_vector',[]))} | merged: {len(out.get('flow_edges_merged',[]))}")
