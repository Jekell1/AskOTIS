#!/usr/bin/env python3
"""qa_probe.py

Utility to pull structured evidence from Azure AI Search indexes to answer
diagnostic questions about COBOL programs:
  - Explain how <PROGRAM> works
  - Purpose of <PROGRAM>
  - Variable definition & usage (e.g., BT-BRANCH)
  - External program calls / dependencies
  - Program usage of external target (e.g., TIM360)

This does NOT generate natural language answers; it assembles JSON evidence
you can feed into an LLM or inspect manually.

Examples:
    python qa_probe.py program-summary ORDERS --limit-chunks 40
  python qa_probe.py program-dependencies LONPF2
  python qa_probe.py variable-provenance BT-BRANCH
  python qa_probe.py variable-usage BT-BRANCH --max 200
    python qa_probe.py external-calls INVENTORY
  python qa_probe.py who-calls TIM360
  python qa_probe.py program-summary TIM360 --fail-if-missing

Environment via local.settings.json or env vars:
  AZURE_SEARCH_ENDPOINT / SEARCH_ENDPOINT
  AZURE_SEARCH_KEY / SEARCH_KEY
API version: 2024-07-01
"""
from __future__ import annotations
import os, json, sys, argparse, requests
from typing import Any, Dict, List, Set, Tuple

API='2024-07-01'

def load_conf():
    vals={}
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    except Exception:
        pass
    def first(*names):
        for n in names:
            v=os.getenv(n) or vals.get(n)
            if v: return v
        return None
    ep=first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT')
    key=first('AZURE_SEARCH_KEY','SEARCH_KEY')
    if not ep or not key:
        print('Missing search configuration', file=sys.stderr)
        sys.exit(2)
    return ep.rstrip('/'), key

def search(ep,key,index,body):
    r=requests.post(f"{ep}/indexes/{index}/docs/search?api-version={API}", headers={'api-key':key,'Content-Type':'application/json'}, json=body, timeout=60)
    if r.status_code!=200:
        raise RuntimeError(f"Search {index} {r.status_code}: {r.text[:300]}")
    return r.json()

def _strict_program_file_ids(ep:str,key:str,program_id:str)->Tuple[Set[str],str]:
    """Attempt authoritative resolution of file_ids for a program.

    Strategies (first non-empty wins):
      A. code-chunks: scope == 'file' AND name == <PROGRAM>.CBL  (preferred authoritative)
      B. cobol-files index: name == <PROGRAM>.CBL
      C. Fallback: legacy cascade (program_id / lexical / flow) then post-filter by actual file-level names.

    Returns: (file_ids, strategy_label)
    """
    want_name = (program_id + '.CBL').upper()
    # A. Direct file-level chunk(s)
    try:
        body={'search':'*', 'filter':f"scope eq 'file' and name eq '{program_id}.CBL'", 'select':'file_id,name','top':50}
        vals=search(ep,key,'code-chunks',body).get('value',[])
        ids={v.get('file_id') for v in vals if (v.get('name') or '').upper()==want_name and v.get('file_id')}
        if ids:
            return ids, 'chunks:scope-file-name'
    except Exception:
        pass
    # B. cobol-files index
    try:
        body={'search':'*','filter':f"name eq '{program_id}.CBL'",'select':'file_id,name','top':50}
        vals=search(ep,key,'cobol-files',body).get('value',[])
        ids={v.get('file_id') for v in vals if (v.get('name') or '').upper()==want_name and v.get('file_id')}
        if ids:
            return ids, 'cobol-files:name'
    except Exception:
        pass
    # C. fallback legacy (will be further filtered)
    legacy = _legacy_fetch_program_file_ids(ep,key,program_id)
    if not legacy:
        return set(), 'unresolved'
    # Build OR filter to fetch file-level chunk names for those candidates
    if len(legacy) <= 100:
        filt = ' or '.join([f"file_id eq '{fid}'" for fid in legacy]) + " and scope eq 'file'"
        try:
            body={'search':'*','filter':filt,'select':'file_id,name','top':len(legacy)*2}
            vals=search(ep,key,'code-chunks',body).get('value',[])
            ids={v.get('file_id') for v in vals if (v.get('name') or '').upper()==want_name and v.get('file_id')}
            if ids:
                return ids, 'legacy+filter'
        except Exception:
            pass
    # If we reach here we didn't positively confirm names – return original legacy for transparency.
    return legacy, 'legacy-unfiltered'

def _legacy_fetch_program_file_ids(ep:str,key:str,program_id:str)->Set[str]:
    """Original heuristic cascade kept for fallback/reference."""
    out:set[str]=set()
    # 1 program_id match
    try:
        body={'search':'*','filter':f"program_id eq '{program_id}'",'select':'file_id','top':1000}
        vals=search(ep,key,'code-chunks',body).get('value',[])
        out={v.get('file_id') for v in vals if v.get('file_id')}
    except Exception:
        out=set()
    if out: return out
    # 2 file_id direct match
    try:
        body={'search':'*','filter':f"file_id eq '{program_id}'",'select':'file_id','top':1000}
        vals=search(ep,key,'code-chunks',body).get('value',[])
        out={v.get('file_id') for v in vals if v.get('file_id')}
    except Exception:
        out=set()
    if out: return out
    # 3 lexical search
    try:
        body={'search':program_id,'select':'file_id,name','top':50}
        vals=search(ep,key,'code-chunks',body).get('value',[])
        out={v.get('file_id') for v in vals if v.get('file_id')}
    except Exception:
        out=set()
    if out: return out
    # 4 flow edges
    try:
        body={'search':'*','filter':f"caller_program_id eq '{program_id}' or file_program_id eq '{program_id}'",'select':'file_id','top':1000}
        vals=search(ep,key,'cobol-flow-edges-v2',body).get('value',[])
        out={v.get('file_id') for v in vals if v.get('file_id')}
    except Exception:
        out=set()
    return out

def fetch_program_file_ids(ep,key,program_id)->Tuple[List[str],Dict[str,Any]]:
    ids, strategy = _strict_program_file_ids(ep,key,program_id)
    meta={'strategy':strategy,'raw_count':len(ids)}
    return sorted(ids), meta

def fetch_program_chunks(ep,key,program_id,limit,file_ids:List[str]):
    if not file_ids:
        return []
    if len(file_ids)==1:
        filt=f"file_id eq '{file_ids[0]}'"
    else:
        filt=' or '.join([f"file_id eq '{fid}'" for fid in file_ids])
    body={'search':'*','filter':filt, 'select':'chunk_id,file_id,name,scope,start_line,end_line,text','top':limit,'orderby':'start_line asc'}
    return search(ep,key,'code-chunks',body).get('value',[])

def fetch_paragraphs(ep,key,program_id,limit,file_ids:List[str]):
    if not file_ids: return []
    if len(file_ids)==1:
        filt=f"file_id eq '{file_ids[0]}'"
    else:
        filt=' or '.join([f"file_id eq '{fid}'" for fid in file_ids])
    body={'search':'*','filter':filt, 'top':limit, 'orderby':'start_line asc'}
    return search(ep,key,'cobol-paragraphs',body).get('value',[])

def fetch_calls(ep,key,program_id,file_ids:List[str]|None=None):
    # If we have explicit file_ids use them, otherwise fall back to program-oriented filters
    if file_ids:
        if len(file_ids)==1:
            filt=f"file_id eq '{file_ids[0]}'"
        else:
            filt=' or '.join([f"file_id eq '{fid}'" for fid in file_ids])
    else:
        # broader net: program_id OR file_id OR (defensive) callee_program
        filt=f"program_id eq '{program_id}' or file_id eq '{program_id}'"
    body={'search':'*','filter':filt,'top':3000,'orderby':'line asc'}
    return search(ep,key,'cobol-calls',body).get('value',[])

def fetch_flow_edges(ep,key,program_id,limit,file_ids:List[str]):
    # prefer caller_program_id match when present, else fallback to file_ids
    prog_filt=f"caller_program_id eq '{program_id}' or file_program_id eq '{program_id}'"
    # also include any of the file_ids explicitly (for safety)
    if file_ids:
        file_clause=' or '.join([f"file_id eq '{fid}'" for fid in file_ids])
        filt=f"({prog_filt}) or ({file_clause})"
    else:
        filt=prog_filt
    body={'search':'*','filter':filt, 'top':limit,'orderby':'line asc'}
    return search(ep,key,'cobol-flow-edges-v2',body).get('value',[])

def fetch_facts(ep,key,program_id,limit,file_ids:List[str],para_names:List[str]|None=None)->Tuple[List[Dict[str,Any]],Dict[str,Any]]:
    """Retrieve fact snippets relevant to a program using layered fallbacks.

    Observation: current cobol-facts file_id values use a different namespace (e.g. f000123)
    than code-chunks (sha-like). Direct filtering by the resolved file_ids produces zero hits.

    Strategy cascade (first non-empty wins):
      1. file_id filter (future compatibility if namespaces align)
      2. paragraph name filter: para IN collected paragraph names (capped)
      3. lexical search for PROGRAM (program id token)
      4. lexical search for PROGRAM.CBL

    Returns (facts, meta) where meta lists each attempted strategy and counts.
    """
    attempted:List[Dict[str,Any]]=[]

    # 1. file_id filter attempt
    if file_ids:
        if len(file_ids)==1:
            filt=f"file_id eq '{file_ids[0]}'"
        else:
            filt=' or '.join([f"file_id eq '{fid}'" for fid in file_ids])
        body={'search':'*','filter':filt,'top':limit,'orderby':'line asc'}
        vals=search(ep,key,'cobol-facts',body).get('value',[])
        attempted.append({'strategy':'file_id','count':len(vals)})
        if vals:
            return vals, {'strategy':'file_id','attempts':attempted}

    # 2. paragraph names filter (limit to first 60 to keep filter size reasonable)
    if para_names:
        limited=[p for p in para_names if p][:60]
        if limited:
            filt=' or '.join([f"para eq '{p}'" for p in limited])
            body={'search':'*','filter':filt,'top':limit,'orderby':'line asc'}
            try:
                vals=search(ep,key,'cobol-facts',body).get('value',[])
            except Exception:
                vals=[]
            attempted.append({'strategy':'para-names','count':len(vals),'para_sample':limited[:5]})
            if vals:
                return vals, {'strategy':'para-names','attempts':attempted}

    # 3. lexical search for program id token
    body={'search':program_id,'top':limit,'orderby':'line asc'}
    try:
        vals=search(ep,key,'cobol-facts',body).get('value',[])
    except Exception:
        vals=[]
    attempted.append({'strategy':'lexical-program','count':len(vals)})
    if vals:
        return vals, {'strategy':'lexical-program','attempts':attempted}

    # 4. lexical search for program filename token PROGRAM.CBL
    body={'search':f"{program_id}.CBL",'top':limit,'orderby':'line asc'}
    try:
        vals=search(ep,key,'cobol-facts',body).get('value',[])
    except Exception:
        vals=[]
    attempted.append({'strategy':'lexical-program-cbl','count':len(vals)})
    if vals:
        return vals, {'strategy':'lexical-program-cbl','attempts':attempted}

    return [], {'strategy':'none','attempts':attempted}

def fetch_variable_symbol(ep,key,name_like):
    filt=f"name eq '{name_like}' or qualified_name eq '{name_like}'"
    body={'search':'*','filter':filt,'select':'item_id,file_id,program_id,name,qualified_name,section,level,pic,usage,start_line,end_line,value','top':50}
    return search(ep,key,'cobol-symbols',body).get('value',[])

def fetch_variable_xrefs(ep,key,name_like,max_refs):
    # try simple_name and qualified_name search via filter OR lexical fallback
    filt=f"simple_name eq '{name_like}' or qualified_name eq '{name_like}'"
    body={'search':'*','filter':filt,'select':'xref_id,file_id,program_id,simple_name,qualified_name,kind,direction,line,snippet','top':max_refs}
    return search(ep,key,'cobol-xrefs',body).get('value',[])

def fetch_copybooks(ep,key,program_id,file_ids:List[str],chunks:List[Dict[str,Any]]|None=None,limit:int|None=None)->Tuple[List[Dict[str,Any]],Dict[str,Any]]:
    """Retrieve copybook inclusion events with layered heuristics.

    Strategy order:
      1. file_id (already resolved from code-chunks strict scope) – authoritative
      2. host-file lookup via cobol-files (program name -> file_id) if #1 empty
      3. lexical-derived-names from COPY statements inside provided chunks

    Limit handling:
      If *limit* is provided, the raw candidate set for the first successful
      strategy is truncated to that size in the returned list while diagnostics
      still record the full raw count. This keeps program-summary output
      manageable for very large programs (e.g., hundreds of COPY inclusions)
      without losing transparency.
    """
    attempts:List[Dict[str,Any]]=[]

    # Helper to execute file_id query
    def query_by_ids(ids:List[str],label:str):
        if not ids:
            return []
        if len(ids)==1:
            filt=f"file_id eq '{ids[0]}'"
        else:
            filt=' or '.join([f"file_id eq '{fid}'" for fid in ids])
        body={'search':'*','filter':filt,'select':'copybook_id,file_id,parent_path,copybook_name,line,replacing_clause','top':500,'orderby':'line asc'}
        try:
            vals=search(ep,key,'cobol-copybooks',body).get('value',[])
        except Exception:
            vals=[]
        returned = vals[:limit] if (limit is not None and len(vals)>limit) else vals
        attempts.append({'strategy':label,'count':len(vals),'returned':len(returned)})
        return returned

    # 1. Existing resolved file_ids (from code-chunks strict scope resolution)
    primary_ids=file_ids or []
    vals=query_by_ids(primary_ids,'file_id')
    if vals:
        return vals, {'strategy':'file_id','attempts':attempts}

    # 2. Host file resolution via cobol-files (in case code-chunks missing or mismatched)
    host_ids:List[str]=[]
    prog_name=f"{program_id}.CBL" if not program_id.upper().endswith('.CBL') else program_id.upper()
    try:
        body={'search':'*','filter':f"name eq '{prog_name}'",'select':'file_id,name','top':10}
        host_vals=search(ep,key,'cobol-files',body).get('value',[])
        for hv in host_vals:
            fid=hv.get('file_id')
            if fid and fid not in host_ids:
                host_ids.append(fid)
    except Exception:
        host_vals=[]
    if host_ids:
        vals=query_by_ids(host_ids,'cobol-files:name')
        if vals:
            return vals, {'strategy':'cobol-files:name','attempts':attempts}

    # 3. Lexical fallback (parse COPY statements from chunks)
    derived:set[str]=set()
    if chunks:
        import re
        pat=re.compile(r'COPY\s+"([A-Za-z0-9_./-]+)"', re.IGNORECASE)
        for ch in chunks:
            txt=ch.get('text') or ''
            for m in pat.finditer(txt):
                raw=m.group(1).strip().upper()
                raw_base=raw[:-4] if raw.endswith('.CPY') else raw
                seg=raw_base.split('/')[-1]
                variants={raw_base, seg}
                if not raw_base.endswith('.CPY'):
                    variants.add(raw_base+'.CPY')
                if not seg.endswith('.CPY'):
                    variants.add(seg+'.CPY')
                variants.add(raw)
                for v in variants:
                    if len(derived)<400:
                        derived.add(v)
        if derived:
            collected:Dict[str,Dict[str,Any]]={}
            acceptable:set[str]=set()
            for d in derived:
                up=d.upper()
                if up.endswith('.CPY'):
                    acceptable.add(up[:-4])
                acceptable.add(up)
                last=up.split('/')[-1]
                if last.endswith('.CPY'):
                    acceptable.add(last[:-4])
                acceptable.add(last)
            tried=0
            for d in list(sorted(derived))[:120]:
                tried+=1
                try:
                    body={'search':d,'select':'copybook_id,file_id,parent_path,copybook_name,line,replacing_clause','top':40}
                    hits=search(ep,key,'cobol-copybooks',body).get('value',[])
                except Exception:
                    hits=[]
                for h in hits:
                    name=(h.get('copybook_name') or '').upper()
                    core=name[:-4] if name.endswith('.CPY') else name
                    if name in acceptable or core in acceptable:
                        collected[h.get('copybook_id')]=h
                if len(collected)>=750:
                    break
            out=list(collected.values())
            out.sort(key=lambda r: r.get('line', 10**9))
            limited_out = out[:limit] if (limit is not None and len(out)>limit) else out
            attempts.append({'strategy':'lexical-derived-names','derived':len(derived),'variants_tried':tried,'count':len(out),'returned':len(limited_out),'sample':list(sorted(list(derived)))[:5]})
            if limited_out:
                return limited_out, {'strategy':'lexical-derived-names','attempts':attempts}

    attempts.append({'strategy':'none','count':0})
    return [], {'strategy':'none','attempts':attempts}

def program_summary(ep,key,program_id,args):
    file_ids,file_meta=fetch_program_file_ids(ep,key,program_id)
    chunks=fetch_program_chunks(ep,key,program_id,args.limit_chunks,file_ids)
    paras=fetch_paragraphs(ep,key,program_id,args.limit_paragraphs,file_ids)
    calls=fetch_calls(ep,key,program_id,file_ids)
    flows=fetch_flow_edges(ep,key,program_id,args.limit_flow,file_ids)
    facts,facts_meta=fetch_facts(ep,key,program_id,args.limit_facts,file_ids,[p.get('name') for p in paras if p.get('name')])
    copyb,copyb_meta=fetch_copybooks(ep,key,program_id,file_ids,chunks,limit=args.limit_copybooks)
    print(json.dumps({
        'type':'program-summary','program':program_id,
        'counts':{
            'file_ids':len(file_ids),'chunks':len(chunks),'paragraphs':len(paras),'calls':len(calls),'flow_edges':len(flows),'facts':len(facts),'copybooks':len(copyb)
        },
    'file_id_resolution': file_meta,
    'facts_resolution': facts_meta,
    'copybook_resolution': copyb_meta,
        'file_ids':file_ids,
        'chunks':chunks,
        'paragraphs':paras,
        'calls':calls,
        'flow_edges':flows,
    'facts':facts,
        'copybooks':copyb
    },indent=2))

def program_dependencies(ep,key,program_id,args):
    calls=fetch_calls(ep,key,program_id)
    copyb=fetch_copybooks(ep,key,program_id)
    targets=sorted({c.get('callee_program') for c in calls if c.get('callee_program')})
    dyn=sum(1 for c in calls if c.get('is_dynamic'))
    print(json.dumps({
        'type':'program-dependencies','program':program_id,
        'call_targets':targets,
        'dynamic_calls':dyn,
        'copybooks':[cb.get('copybook_name') for cb in copyb],
        'totals':{'calls':len(calls),'copybooks':len(copyb)}
    },indent=2))

def variable_provenance(ep,key,var,args):
    syms=fetch_variable_symbol(ep,key,var)
    print(json.dumps({'type':'variable-provenance','variable':var,'definitions':syms},indent=2))

def variable_usage(ep,key,var,args):
    refs=fetch_variable_xrefs(ep,key,var,args.max)
    by_prog={}
    for r in refs:
        p=r.get('program_id') or 'UNKNOWN'
        by_prog.setdefault(p,0); by_prog[p]+=1
    print(json.dumps({'type':'variable-usage','variable':var,'reference_count':len(refs),'by_program':by_prog,'samples':refs[:args.sample]},indent=2))

def external_calls(ep,key,program_id,args):
    calls=fetch_calls(ep,key,program_id)
    targets={c.get('callee_program') for c in calls if c.get('callee_program')}
    print(json.dumps({'type':'external-calls','program':program_id,'targets':sorted(targets),'call_count':len(calls)},indent=2))

def who_calls(ep,key,target,args):
    # Invert calls: search calls index for callee_program == target
    body={'search':'*','filter':f"callee_program eq '{target}'",'top':5000}
    hits=search(ep,key,'cobol-calls',body).get('value',[])
    callers=set()
    for h in hits:
        prog=h.get('program_id') or h.get('file_id')
        if prog: callers.add(prog)
    print(json.dumps({'type':'who-calls','target':target,'callers':sorted(callers),'call_sites':hits[:args.sample],'call_count':len(hits)},indent=2))

def main():
    ap=argparse.ArgumentParser()
    sub=ap.add_subparsers(dest='cmd', required=True)

    ps=sub.add_parser('program-summary'); ps.add_argument('program'); ps.add_argument('--limit-chunks',type=int,default=40); ps.add_argument('--limit-paragraphs',type=int,default=120); ps.add_argument('--limit-flow',type=int,default=400); ps.add_argument('--limit-facts',type=int,default=400); ps.add_argument('--limit-copybooks',type=int,default=400)
    pd=sub.add_parser('program-dependencies'); pd.add_argument('program')
    vp=sub.add_parser('variable-provenance'); vp.add_argument('variable')
    vu=sub.add_parser('variable-usage'); vu.add_argument('variable'); vu.add_argument('--max',type=int,default=500); vu.add_argument('--sample',type=int,default=25)
    ec=sub.add_parser('external-calls'); ec.add_argument('program')
    wc=sub.add_parser('who-calls'); wc.add_argument('target'); wc.add_argument('--sample',type=int,default=30)

    args=ap.parse_args()
    ep,key=load_conf()
    try:
        if args.cmd=='program-summary': program_summary(ep,key,args.program,args)
        elif args.cmd=='program-dependencies': program_dependencies(ep,key,args.program,args)
        elif args.cmd=='variable-provenance': variable_provenance(ep,key,args.variable,args)
        elif args.cmd=='variable-usage': variable_usage(ep,key,args.variable,args)
        elif args.cmd=='external-calls': external_calls(ep,key,args.program,args)
        elif args.cmd=='who-calls': who_calls(ep,key,args.target,args)
    except Exception as ex:
        print(json.dumps({'error':str(ex)},indent=2), file=sys.stderr)
        sys.exit(1)

if __name__=='__main__':
    main()
