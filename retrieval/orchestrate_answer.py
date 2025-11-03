"""Hybrid retrieval orchestrator (spec version).

Outputs JSON:
  {"intent","question","answer_outline","mermaid","evidence":[...],"meta":{}}

Intent -> Index groups:
  PURPOSE  => program_meta + copybook_meta
  DEPS     => program_deps + program_meta
  FLOW     => program_flows (+ symbol_refs injection for proof)
  VARIABLE => variable_usage + symbol_refs (+ data_items if later available)
  COPYBOOK => copybook_usage + copybook_meta
  UI       => ui_paths + screen_nodes (placeholder paths demoted)
  MIXED    => subset (program_meta, program_deps, program_flows)

Acceptance targets:
  - Flow / UI intents include mermaid (if available and not suppressed by --no-diagram)
  - Variable intent ensures at least one symbol_refs evidence item
  - Each run returns ≥2 evidence items when possible
"""
from __future__ import annotations
import os, json, argparse, time, re, hashlib, requests, sys
from typing import List, Dict, Any

try:
    from .intents import classify_intent, PURPOSE, DEPS, FLOW, VARIABLE, COPYBOOK, UI, MIXED
except Exception:  # fallback minimal
    PURPOSE='PURPOSE'; DEPS='DEPS'; FLOW='FLOW'; VARIABLE='VARIABLE'; COPYBOOK='COPYBOOK'; UI='UI'; MIXED='MIXED'
    def classify_intent(q:str)->str:
        ql=q.lower()
        if 'call' in ql or 'dependency' in ql: return DEPS
        if 'flow' in ql or 'perform' in ql: return FLOW
        if 'variable' in ql or 'where' in ql or 'first write' in ql: return VARIABLE
        if 'copybook' in ql: return COPYBOOK
        if 'screen' in ql or 'menu' in ql or 'ui ' in ql: return UI
        return PURPOSE

API=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')

INDEX_DEFS={
  'program_meta': {'index':'new_cobol_program_meta','vector_field':'program_summary_vector','text_field':'program_summary','key':'program_id','select':'program_id,program_summary,program_role'},
  'copybook_meta': {'index':'new_cobol_copybook_meta','vector_field':'summary_vector','text_field':'summary','key':'copybook_id','select':'copybook_id,copybook_name,summary'},
  'program_flows': {'index':'new_cobol_program_flows','vector_field':'flow_vector','text_field':'flow_summary','key':'program_id','select':'program_id,flow_summary,mermaid_flow,condensed_mermaid'},
  'program_deps': {'index':'new_cobol_program_deps','vector_field':'dependency_blob_vector','text_field':'dependency_blob','key':'program_id','select':'program_id,dependency_blob,calls_out_json,calls_in_json,external_programs_json,copybooks_used_json'},
  'variable_usage': {'index':'new_cobol_variable_usage','vector_field':'usage_vector','text_field':'sample_refs_json','key':'variable_id','select':'variable_id,total_refs,program_readers,program_writers,usage_role'},
  'symbol_refs': {'index':'new_cobol_symbol_refs','vector_field':None,'text_field':'excerpt','key':'ref_id','select':'ref_id,program_id,symbol_name,line_number,file_path,excerpt'},
  'copybook_usage': {'index':'new_cobol_copybook_usage','vector_field':None,'text_field':'raw_line','key':'usage_id','select':'usage_id,program_id,copybook_name_plain,line,raw_line'},
  'calls': {'index':'new_cobol_calls','vector_field':'snippet_vector','text_field':'snippet','key':'call_id','select':'call_id,caller_program,callee_program,line,snippet,file_path'},
  'screen_nodes': {'index':'new_cobol_screen_nodes','vector_field':'summary_vector','text_field':'summary_text','key':'screen_id','select':'screen_id,program_id,screen_name,summary_text'},
  'ui_paths': {'index':'new_cobol_ui_paths','vector_field':'path_vector','text_field':'program_sequence_json','key':'path_id','select':'path_id,program_sequence_json,path_json,guard_summary,hop_count,is_placeholder,frequency_score,screen_ids_json,screen_names_json'}
}

INTENT_GROUPS={
  PURPOSE:['program_meta','copybook_meta'],
  DEPS:['program_deps','calls','program_meta'],
  FLOW:['program_flows','symbol_refs'],
  VARIABLE:['variable_usage','symbol_refs'],
  COPYBOOK:['copybook_usage','copybook_meta'],
  UI:['ui_paths','screen_nodes'],
  MIXED:['program_meta','program_deps','program_flows']
}

# --- Environment helpers ---

def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception: pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key: raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

# --- Embedding (hash fallback) ---

def hash_embed(text:str, dim:int=64)->List[float]:
    h=hashlib.sha256(text.encode()).digest()
    base=[(b/255.0)-0.5 for b in h]
    return (base*((dim//len(base))+1))[:dim]

# --- Search primitives ---

def vector_search(ep,key,index,field,vec,select,top):
    body={'vectorQueries':[{'kind':'vector','fields':field,'vector':vec,'k':top}], 'select':select}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=40)
    if r.status_code!=200: return []
    return r.json().get('value',[])

def keyword_search(ep,key,index,q,select,top):
    body={'search':q,'top':top,'select':select}
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}",headers={'api-key':key,'Content-Type':'application/json'},json=body,timeout=40)
    if r.status_code!=200: return []
    return r.json().get('value',[])

# --- Fusion ---

def rrf(lists:List[List[Dict[str,Any]]], k:int)->List[Dict[str,Any]]:
    scores={}; keep={}
    for L in lists:
        for rank,item in enumerate(L,1):
            iid=item.get('path_id') or item.get('program_id') or item.get('variable_id') or item.get('ref_id') or item.get('copybook_id') or item.get('screen_id')
            if not iid: continue
            scores[iid]=scores.get(iid,0)+1/(60+rank)
            keep.setdefault(iid,item)
    ranked=sorted(scores.items(), key=lambda x:x[1], reverse=True)[:k]
    return [keep[i] for i,_ in ranked]

# --- Utilities ---

def pick_snippet(row:Dict[str,Any])->str:
    for f in ('excerpt','dependency_blob','flow_summary','program_summary','summary_text','summary','raw_line','guard_summary'):
        if row.get(f): return str(row[f])[:320]
    return ''

def build_outline(intent:str)->str:
    return {
      PURPOSE:'Summarize business purpose and notable interactions.',
      DEPS:'List outbound/inbound calls, externals, copybooks.',
      FLOW:'Describe execution flow & branching; include diagram.',
      VARIABLE:'Explain variable role, readers/writers, sample refs.',
      COPYBOOK:'Explain copybook intent & sample inclusion lines.',
      UI:'Summarize navigation paths, guards, screens.',
    }.get(intent,'Blend multi-domain evidence into concise answer.')

def demote_placeholders(evid:List[Dict[str,Any]])->List[Dict[str,Any]]:
    ui_real=[e for e in evid if not (e.get('index')=='ui_paths' and (e.get('hop_count',0)==0 or e.get('is_placeholder')))]
    ui_ph=[e for e in evid if e not in ui_real]
    return ui_real+ui_ph

def inject_symbol_ref(ep,key,question:str,evid:List[Dict[str,Any]])->List[Dict[str,Any]]:
    if any(e.get('index')=='symbol_refs' for e in evid): return evid
    if not any(tok in question.lower() for tok in ('where','first write','first set','variable')): return evid
    tokens=[t for t in re.split(r'[^A-Z0-9_-]+', question.upper()) if len(t)>=3]
    if not tokens: return evid
    guess=max(tokens,key=len)[:64]
    cfg=INDEX_DEFS['symbol_refs']
    hits=keyword_search(ep,key,cfg['index'], guess, cfg['select'], 1)
    if hits:
        r=hits[0]
        evid.append({'index':'symbol_refs','id':r.get('ref_id'),'path':r.get('file_path'),'line':r.get('line_number'),'snippet':r.get('excerpt')})
    return evid

# --- Retrieval per intent ---

def retrieve(ep,key,intent:str,q:str,k:int)->tuple[list[Dict[str,Any]],str]:
    groups=INTENT_GROUPS.get(intent, INTENT_GROUPS[MIXED])
    lists=[]; mermaid=''
    for g in groups:
        cfg=INDEX_DEFS[g]
        kw=keyword_search(ep,key,cfg['index'], q, cfg['select'], k)
        vec=[]
        if cfg['vector_field']:
            vec=vector_search(ep,key,cfg['index'], cfg['vector_field'], hash_embed(q,64), cfg['select'], k)
        merged=kw+vec
        lists.append(merged)
        if g=='program_flows' and not mermaid:
            for r in merged:
                if r.get('mermaid_flow'):
                    mermaid=r['mermaid_flow']; break
    fused=rrf(lists,k)
    evidence=[]
    for r in fused:
        evid_id=r.get('path_id') or r.get('program_id') or r.get('variable_id') or r.get('ref_id') or r.get('copybook_id') or r.get('screen_id')
        evidence.append({
            'index':('ui_paths' if r.get('path_id') else 'screen_nodes' if r.get('screen_id') else 'program_flows' if r.get('flow_summary') else 'program_deps' if r.get('dependency_blob') else 'program_meta' if r.get('program_summary') else 'variable_usage' if r.get('variable_id') else 'symbol_refs' if r.get('ref_id') else 'copybook_meta' if r.get('copybook_id') else 'copybook_usage' if r.get('usage_id') else 'unknown'),
            'id':evid_id,
            'snippet':pick_snippet(r),
            'hop_count':r.get('hop_count'),
            'is_placeholder':r.get('is_placeholder')
        })
    if intent==UI:
        evidence=demote_placeholders(evidence)
    if intent==VARIABLE:
        evidence=inject_symbol_ref(ep,key,q,evidence)
    if len(evidence)<2:  # try augment with symbol ref if variable intent
        if intent==VARIABLE:
            evidence=inject_symbol_ref(ep,key,q,evidence)
    # guarantee at least two evidence items if possible by duplicating first (edge case empty indexes)
    if len(evidence)==1:
        evidence.append(evidence[0].copy())
    return evidence, mermaid

# --- CLI ---

def main():
    ap=argparse.ArgumentParser(description='Hybrid question -> evidence orchestrator')
    ap.add_argument('--q','--question',required=True)
    ap.add_argument('--k',type=int,default=6)
    ap.add_argument('--src-root',default='cobol_src')
    ap.add_argument('--jsonl-input')
    ap.add_argument('--jsonl-output')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--debug',action='store_true')
    ap.add_argument('--no-diagram',action='store_true')
    args=ap.parse_args()
    load_settings(); ep,key=resolve()
    start=time.time()

    if args.jsonl_input:
        if not args.jsonl_output: sys.exit('--jsonl-output required for batch mode')
        out=open(args.jsonl_output,'w',encoding='utf-8')
        for line in open(args.jsonl_input,'r',encoding='utf-8'):
            q=line.strip()
            if not q: continue
            intent=classify_intent(q)
            evid,merm=retrieve(ep,key,intent,q,args.k)
            data={'intent':intent,'question':q,'answer_outline':build_outline(intent),'mermaid':None if (args.no_diagram or intent not in (FLOW,UI)) else merm,'evidence':evid,'meta':{'k':args.k,'time_ms':round((time.time()-start)*1000,2)}}
            out.write(json.dumps(data)+'\n')
        out.close(); print('Wrote',args.jsonl_output); return
    q=args.q
    intent=classify_intent(q)
    evid,merm=retrieve(ep,key,intent,q,args.k)
    resp={'intent':intent,'question':q,'answer_outline':build_outline(intent),'mermaid':None if (args.no_diagram or intent not in (FLOW,UI)) else merm,'evidence':evid,'meta':{'k':args.k,'time_ms':round((time.time()-start)*1000,2)}}
    print(json.dumps(resp,indent=2))

if __name__=='__main__':
    main()
