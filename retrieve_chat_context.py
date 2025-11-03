"""Retrieval orchestrator for chatbot.

Given a user natural language question, we:
  1. Embed the query.
  2. Run semantic/hybrid queries across key indexes:
       - new_code_chunks (code context)
       - new_cobol_program_deps (dependency summaries)
       - new_cobol_ui_paths (navigation / path semantics)
       - new_cobol_screen_nodes (screen definitions)
       - new_cobol_variable_usage (variable semantic usage)
  3. Perform lightweight joins / enrichment:
       - From variable usage -> symbol_id join to symbol refs (if needed)
       - From ui path -> expand screens (optionally hitting screen_nodes)
       - From deps -> collect outward/inward call programs for follow-up expansion
  4. Return a structured JSON payload ready for answer synthesis.

Phase 1: Fast implementation focusing on vector + keyword fallback.
You can extend with caching, scoring fusion, guard analysis, clustering.

Usage:
  python retrieve_chat_context.py --query "How is customer ID validated on the claim update screen?"
"""
from __future__ import annotations
import os, json, argparse, requests, math, time
from typing import Any, Dict, List
from embedding_utils import embed_one

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

INDEX_CONFIG = [
    # Code chunks index (assumes existing chunk_vector field)
    {
        'name':'new_code_chunks',
        'vector_field':'chunk_vector',
        'text_field':'text',
        'key':'chunk_id',
        'select':'chunk_id,program_id,copybook_name,text,content,symbols_json'
    },
    # Program paragraph/perform flow graphs (new_cobol_program_flows)
    {
        'name':'new_cobol_program_flows',
        'vector_field':'flow_vector',
        'text_field':'flow_summary',
        'key':'program_id',
        'select':'program_id,flow_summary,mermaid_flow,condensed_mermaid,node_count,edge_count,max_depth,has_cycles,entry_nodes_json,exit_nodes_json,high_fanout_nodes_json,path_samples_json,risk_score,perform_thru_unexpanded_count'
    },
    # Program dependencies (vector field actually named dependency_blob_vector; key is program_id)
    {
        'name':'new_cobol_program_deps',
        'vector_field':'dependency_blob_vector',
        'text_field':'dependency_blob',
        'key':'program_id',
        'select':'program_id,program_role,dependency_blob,outgoing_programs_json,incoming_programs_json,copybooks_used_json,outgoing_count,incoming_count,copybook_count'
    },
    # UI navigation paths
    {
        'name':'new_cobol_ui_paths',
        'vector_field':'path_vector',
        'text_field':'program_sequence_json',
        'key':'path_id',
        'select':'path_id,root_program_id,leaf_program_id,start_program_id,end_program_id,program_sequence_json,screen_sequence_json,path_json,length,ui_program_count,guards_json,guard_summary,screen_ids_json,avg_edge_freq,min_edge_freq,frequency_score,score,updated_at'
    },
    # Concrete screen nodes (summary_vector added by add_vector_field_screen_nodes.py)
    {
        'name':'new_cobol_screen_nodes',
        'vector_field':'summary_vector',
        'text_field':'summary_text',
        'key':'screen_id',
        'select':'screen_id,program_id,screen_name,summary_text,raw_span_text,field_count,action_count,transition_count'
    },
    # Variable usage (if embeddings added; fallback to keyword only if vector field missing)
    {
        'name':'new_cobol_variable_usage',
        'vector_field':'usage_vector',
        'text_field':'sample_refs_json',
        'key':'variable_id',
        'select':'variable_id,read_count,write_count,total_refs,program_readers,program_writers,usage_role'
    }
]

SYMBOL_REFS_INDEX='new_cobol_symbol_refs'
ALIAS_INDEX='new_cobol_name_aliases'


def load_local_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY','AZURE_OPENAI_ENDPOINT','AZURE_OPENAI_KEY','AZURE_OPENAI_EMBEDDING_DEPLOYMENT']:
            if k in vals and k not in os.environ:
                os.environ[k]=vals[k]
    except Exception:
        pass

def resolve_search():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key env vars.')
    return ep.rstrip('/'), key


def vector_query(ep,key,index,vector,vector_field,select,top=5):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API}"
    body={
        'vectorQueries':[{
            'kind':'vector','vector':vector,'k':top,'fields':vector_field
        }],
        'select':select,
    }
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return {'error':r.text[:200]}
    return r.json().get('value',[])


def keyword_fallback(ep,key,index,query,select,top=3):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API}"
    body={'search':query,'top':top,'select':select}
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
    if r.status_code!=200:
        return []
    return r.json().get('value',[])


def fetch_symbol_refs(ep,key,symbol_ids,max_per_symbol=3):
    out=[]
    if not symbol_ids: return out
    # Build filter OR chain (Azure Search filter clause length caution)
    # We'll chunk symbol_ids to avoid overly long filters.
    symbol_ids=list(symbol_ids)
    for i in range(0,len(symbol_ids),20):
        subset=symbol_ids[i:i+20]
        filt=' or '.join([f"symbol_id eq '{sid}'" for sid in subset])
        url=f"{ep}/indexes/{SYMBOL_REFS_INDEX}/docs/search?api-version={API}"
        body={'search':'*','filter':filt,'top':max_per_symbol*len(subset),
              'select':'ref_id,program_id,symbol_name,symbol_id,symbol_id_global,kind,op,normalized_kind,line_number,paragraph_name,file_path,excerpt,context_before,context_after'}
        r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=60)
        if r.status_code==200:
            out.extend(r.json().get('value',[]))
    return out

def fetch_alias_variants(ep,key,tokens, max_variants=50):
    """Fetch alias variants for tokens and return mapping canonical -> set(aliases)."""
    if not tokens: return {}
    collected={}
    # chunk tokens to avoid long filters
    norm=[t.upper() for t in tokens if len(t)>=3]
    for i in range(0,len(norm),15):
        subset=norm[i:i+15]
        flt=' or '.join([f"alias eq '{t}' or canonical_name eq '{t}'" for t in subset])
        url=f"{ep}/indexes/{ALIAS_INDEX}/docs/search?api-version={API}"
        body={'search':'*','filter':flt,'top':max_variants,'select':'canonical_name,alias'}
        try:
            r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=30)
            if r.status_code!=200: continue
            for row in r.json().get('value',[]):
                can=row.get('canonical_name')
                al=row.get('alias')
                if not can or not al: continue
                s=collected.setdefault(can,set())
                s.add(al)
        except Exception:
            continue
    return collected


RISK_TERMS={'risky','complex','most complex','high risk','hotspot','complicated'}

def is_risk_query(q:str)->bool:
    l=q.lower()
    return any(t in l for t in RISK_TERMS)

def orchestrate(query:str, per_index:int=5, keyword_top:int=3, include_symbol_refs:bool=True):
    load_local_settings(); ep,key=resolve_search()
    qvec=embed_one(query)
    # Tokenize query for alias expansion
    import re
    tokens=[t for t in re.split(r'[^A-Za-z0-9_-]+',query) if t]
    alias_map=fetch_alias_variants(ep,key,tokens)
    # Expanded keyword terms for fallback (dedupe)
    expanded_terms=set(tokens)
    for can,als in alias_map.items():
        expanded_terms.add(can)
        expanded_terms.update(als)
    expanded_query=' '.join(sorted(expanded_terms))[:512]
    results={'query':query,'expanded_query':expanded_query,'aliases':{k:sorted(list(v)) for k,v in alias_map.items()},'indexes':{},'symbol_refs':[], 'meta':{'api_version':API,'ts':time.time()}}
    collected_symbol_ids=set()
    risk_mode=is_risk_query(query)
    for cfg in INDEX_CONFIG:
        idx_name=cfg['name']
        try:
            vec_hits=[]
            vq=vector_query(ep,key,idx_name,qvec,cfg['vector_field'],cfg['select'],top=per_index)
            if isinstance(vq,list):
                vec_hits=vq
            if not vec_hits:
                kw_source=expanded_query if alias_map else query
                kw_hits=keyword_fallback(ep,key,idx_name,kw_source,cfg['select'],top=keyword_top)
            else:
                kw_hits=[]
            # Post-process program flows enrichment for rendering convenience
            if idx_name=='new_cobol_program_flows' and vec_hits:
                for h in vec_hits:
                    try:
                        full=h.get('mermaid_flow') or ''
                        condensed=h.get('condensed_mermaid') or ''
                        # Choose condensed if it saves >30% length and graph is large
                        if full and condensed and len(full)>400 and len(condensed)/len(full) < 0.7:
                            h['render_mermaid']=condensed
                            h['flow_hint']='condensed'
                        else:
                            h['render_mermaid']=full or condensed
                            h['flow_hint']='full'
                    except Exception:
                        h['render_mermaid']=h.get('mermaid_flow')
                        h['flow_hint']='full'
                if risk_mode:
                    # Re-rank by risk_score descending (fallback keep order)
                    scored=[(h.get('risk_score',0),h) for h in vec_hits]
                    scored.sort(key=lambda x:x[0],reverse=True)
                    vec_hits=[h for _,h in scored]
            # Guard awareness for UI paths (future: integrate real guards_json semantics)
            if idx_name=='new_cobol_ui_paths' and vec_hits and 'validate' in query.lower():
                # naive boost: paths with guards_json length get earlier placement
                def guard_weight(h):
                    gj=h.get('guards_json') or ''
                    return len(gj)
                vec_hits=sorted(vec_hits,key=guard_weight,reverse=True)
            # Prefer higher frequency_score if present and user implies common/typical path
            if idx_name=='new_cobol_ui_paths':
                if any(t in query.lower() for t in ['common','typical','frequent','usual','often']):
                    vec_hits=sorted(vec_hits,key=lambda h: h.get('frequency_score',0), reverse=True)
                    if kw_hits:
                        kw_hits=sorted(kw_hits,key=lambda h: h.get('frequency_score',0), reverse=True)
                # Attach a short human readable snippet for each path
                for h in vec_hits[:10]:
                    try:
                        seq=json.loads(h.get('program_sequence_json') or '[]')
                        h['path_preview']=' -> '.join(seq[:6]) + (' ...' if len(seq)>6 else '')
                        if h.get('guard_summary'):
                            h['path_preview'] += f" | guards: {h['guard_summary'][:80]}"
                    except Exception:
                        pass
            results['indexes'][idx_name]={
                'vector_hits':vec_hits,
                'keyword_fallback':kw_hits
            }
            # collect symbol ids only if variable usage doc includes symbol references (older schema may not)
            if idx_name=='new_cobol_variable_usage':
                for h in vec_hits:
                    for maybe_sid_field in ('symbol_id','symbol_id_global'):
                        sid=h.get(maybe_sid_field)
                        if sid:
                            collected_symbol_ids.add(sid)
        except Exception as e:
            results['indexes'][idx_name]={'error':str(e)}
    if include_symbol_refs and collected_symbol_ids:
        refs=fetch_symbol_refs(ep,key,collected_symbol_ids)
        results['symbol_refs']=refs
    return results

if __name__=='__main__':
    ap=argparse.ArgumentParser(description='Chat retrieval orchestrator')
    ap.add_argument('--query',required=True)
    ap.add_argument('--per-index',type=int,default=5)
    ap.add_argument('--keyword-top',type=int,default=3)
    ap.add_argument('--no-symbol-refs',action='store_true')
    ap.add_argument('--out')
    args=ap.parse_args()
    data=orchestrate(args.query,args.per_index,args.keyword_top,not args.no_symbol_refs)
    js=json.dumps(data,indent=2)[:200000]
    if args.out:
        open(args.out,'w',encoding='utf-8').write(js)
        print('Wrote',args.out)
    else:
        print(js)
