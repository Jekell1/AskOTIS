"""Build per-program paragraph/PERFORM flow graphs and ingest into new_cobol_program_flows.

Spec (Upgraded):
    * Universe of programs via ingest.program_catalog.discover_program_ids() unless --programs supplied.
    * For each program, derive nodes & edges (prefer paragraph/edge indexes; fallback minimal empty doc if none).
    * Compute metrics: node_count, edge_count, max_depth, has_cycles.
    * Collect JSON payload fields: entry_nodes_json, exit_nodes_json, flow_nodes_json, flow_edges_json,
        high_fanout_nodes_json, path_samples_json.
    * Generate Mermaid diagram + a condensed variant (collapsing linear chains) as mermaid_flow & condensed_mermaid.
    * Produce flow_summary (3–6 sentences) capturing size, structure, risk, notable entry/high-fanout paragraphs, sample paths.
    * Embed flow_summary -> flow_vector (3072) unless --no-embed (otherwise set has_vector False for later backfill).
    * Always create a document per program (degenerate doc when zero paragraphs/edges discovered) to satisfy coverage ≥95%.

CLI Examples:
    python build_program_flows.py --src-root cobol_src --all --batch 300 --push
    python build_program_flows.py --programs DAILY,LNQUOT --push
    python build_program_flows.py --all --no-embed --push  # defer embeddings to backfill
"""
from __future__ import annotations
import os, re, json, argparse, pathlib, time, datetime, requests, math
from typing import Dict, List, Set, Tuple
from embedding_utils import batch_embed
from ingest import program_catalog as catalog

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
INDEX='new_cobol_program_flows'
PARA_INDEX='new_cobol_paragraphs'
EDGE_INDEX='new_cobol_flow_edges'
EDGE_INDEX_V2='new_cobol_flow_edges_v2'
VECTOR_FIELD='flow_vector'
HAS_FIELD='has_vector'
FLOW_VECTOR_DIM=int(os.getenv('FLOW_VECTOR_DIM','3072'))

PARA_LABEL_RE=re.compile(r'^(?P<label>[A-Z0-9][A-Z0-9-]{1,63})\.(\s*$)',re.IGNORECASE)
DIVISION_RE=re.compile(r'^(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION',re.IGNORECASE)
SECTION_RE=re.compile(r'^[A-Z0-9][A-Z0-9-]{1,63}\s+SECTION\.',re.IGNORECASE)
PERFORM_RE=re.compile(r'PERFORM\s+([A-Z0-9][A-Z0-9-]{1,63})',re.IGNORECASE)
PERFORM_THRU_RE=re.compile(r'PERFORM\s+([A-Z0-9][A-Z0-9-]{1,63})\s+THRU\s+([A-Z0-9][A-Z0-9-]{1,63})',re.IGNORECASE)

MAX_DEPTH_LIMIT=40
SUMMARY_SENTENCE_LIMIT=6


def load_settings():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing search endpoint/key')
    return ep.rstrip('/'), key

def scan_file(path: pathlib.Path, expand_thru: bool=False) -> Dict[str, any] | None:
    try:
        text=path.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return None
    lines=text.splitlines()
    # Collect paragraphs
    paragraphs: List[str] = []
    para_lines: Dict[str, Tuple[int,int]] = {}  # label -> (start_line, end_line_exclusive placeholder)
    current=None
    for i,line in enumerate(lines):
        if DIVISION_RE.match(line) or SECTION_RE.match(line):
            continue
        m=PARA_LABEL_RE.match(line.strip())
        if m:
            label=m.group('label').upper()
            if current and current not in para_lines:
                para_lines[current]=(para_start,i)
            paragraphs.append(label)
            current=label
            para_start=i
    if current and current not in para_lines:
        para_lines[current]=(para_start,len(lines))
    if not paragraphs:
        return None
    # Build edges & track unexpanded THRU
    edges: List[Tuple[str,str]] = []
    unexpanded_thru=0
    order_index={name:i for i,name in enumerate(paragraphs)}
    for p,label in enumerate(paragraphs):
        start,end=para_lines[label]
        body_lines=lines[start:end]
        for bl in body_lines:
            # detect PERFORM THRU (count & optionally expand)
            for mt in PERFORM_THRU_RE.finditer(bl):
                unexpanded_thru+=1
                if expand_thru:
                    a=mt.group(1).upper(); b=mt.group(2).upper()
                    if a in order_index and b in order_index:
                        ia, ib=order_index[a], order_index[b]
                        if ia<=ib:
                            for tgt_name in paragraphs[ia:ib+1]:
                                if tgt_name!=label:
                                    edges.append((label,tgt_name))
            for m in PERFORM_RE.finditer(bl):
                tgt=m.group(1).upper()
                if tgt!=label:
                    edges.append((label,tgt))
    # Collapse duplicates
    edges=list(dict.fromkeys(edges))
    nodes=paragraphs
    # Depth + cycles
    adj: Dict[str,List[str]]={n:[] for n in nodes}
    for s,d in edges:
        adj.setdefault(s,[]).append(d)
    visited: Set[str]=set()
    stack: Set[str]=set()
    has_cycle=False
    def dfs_cycle(n: str):
        nonlocal has_cycle
        if n in stack:
            has_cycle=True; return
        if n in visited:
            return
        visited.add(n); stack.add(n)
        for nxt in adj.get(n,[]):
            dfs_cycle(nxt)
        stack.remove(n)
    for n in nodes:
        if n not in visited:
            dfs_cycle(n)
    # Max depth DFS with cutoff
    def depth(n: str, depth_so_far: int, seen: Set[str]) -> int:
        if depth_so_far>MAX_DEPTH_LIMIT: return depth_so_far
        best=depth_so_far
        for nxt in adj.get(n,[]):
            if nxt in seen: continue
            best=max(best, depth(nxt, depth_so_far+1, seen|{nxt}))
        return best  # Fix: Added missing return statement
    max_depth=0
    for n in nodes:
        max_depth=max(max_depth, depth(n,1,set()))
    # In/Out degree
    indeg={n:0 for n in nodes}
    outdeg={n:0 for n in nodes}
    for s,d in edges:
        outdeg[s]=outdeg.get(s,0)+1
        indeg[d]=indeg.get(d,0)+1
    entry_nodes=[n for n in nodes if indeg.get(n,0)==0]
    exit_nodes=[n for n in nodes if outdeg.get(n,0)==0]
    # High fanout (top 3)
    high_fanout=sorted(nodes,key=lambda n:outdeg.get(n,0),reverse=True)[:3]
    # Path samples (DFS from each entry) limited
    path_samples=[]
    MAX_PATHS=4
    MAX_PATH_LEN=12
    def dfs_paths(start):
        stack=[(start,[start])]
        local=0
        seen_paths=set()
        while stack and len(path_samples)<MAX_PATHS and local<MAX_PATHS*3:
            node,path_so_far=stack.pop()
            outs=adj.get(node,[])
            if not outs or len(path_so_far)>=MAX_PATH_LEN:
                key='>'.join(path_so_far)
                if key not in seen_paths:
                    path_samples.append(path_so_far[:])
                    seen_paths.add(key)
                continue
            for nxt in outs[:6]:  # limit branching per sample
                if nxt in path_so_far:
                    continue
                stack.append((nxt,path_so_far+[nxt]))
            local+=1
    for e in entry_nodes[:5]:
        if len(path_samples)>=MAX_PATHS: break
        dfs_paths(e)
    # Paragraph roles heuristic
    role_map={}
    def classify(name:str)->str:
        u=name.upper()
        if any(x in u for x in ('INIT','START','SETUP')): return 'INIT'
        if any(x in u for x in ('VAL','CHECK','TEST')): return 'VALIDATE'
        if any(x in u for x in ('CALC','COMP','TOTAL','ACCUM')): return 'CALC'
        if any(x in u for x in ('READ','WRITE','DISPLAY','ACCEPT','OPEN','CLOSE')): return 'IO'
        if any(x in u for x in ('FINAL','END','TERM','WRAP')): return 'FINAL'
        return 'OTHER'
    for n in nodes:
        role_map[n]=classify(n)
    # Condensed Mermaid (collapse linear chains)
    # Identify chain starts: outdeg==1 and indeg!=1 nodes are boundaries
    def condensed_edges():
        consumed=set(); new_edges=[]
        for n in nodes:
            if n in consumed: continue
            if outdeg.get(n,0)==1:
                # try forward chain
                chain=[n]
                cur=n
                while True:
                    nxts=adj.get(cur,[])
                    if len(nxts)!=1: break
                    nxt=nxts[0]
                    if indeg.get(nxt,0)!=1: break
                    if outdeg.get(nxt,0)!=1: # include nxt but stop
                        chain.append(nxt)
                        consumed.update(chain[1:-1])
                        break
                    chain.append(nxt)
                    cur=nxt
                    if len(chain)>15: break
                if len(chain)>2:
                    new_edges.append((chain[0],chain[-1],chain[1:-1]))
        return new_edges
    collapsed=condensed_edges()
    mermaid=['flowchart TD']
    for s,d in edges:
        mermaid.append(f"  {s}--> {d}")
    mermaid_flow='\n'.join(mermaid)[:20000]
    condensed=['flowchart TD']
    collapsed_pairs={(c[0],c[1]):c[2] for c in collapsed}
    used_collapsed=set()
    for s,d in edges:
        key=(s,d)
        if key in collapsed_pairs and key not in used_collapsed:
            mid=collapsed_pairs[key]
            label=f"[{len(mid)} step chain]"
            condensed.append(f"  {s}--> {d}:::chain")
            used_collapsed.add(key)
        else:
            condensed.append(f"  {s}--> {d}")
    condensed_mermaid='\n'.join(condensed)[:20000]
    # Risk score simple blend
    max_fanout=max(outdeg.values()) if outdeg else 0
    norm_depth=min(1.0, max_depth/20 if max_depth else 0)
    norm_fanout=min(1.0, max_fanout/10 if max_fanout else 0)
    risk_score=round(0.3*(1 if has_cycle else 0)+0.3*norm_depth+0.4*norm_fanout,3)
    base_summary=(f"Paragraphs={len(nodes)}; edges={len(edges)}; depth={max_depth}; cycles={'Y' if has_cycle else 'N'}; "
                  f"entries={len(entry_nodes)}; exits={len(exit_nodes)}; fanout_max={max_fanout}; risk={risk_score}.")
    elaboration=""
    if entry_nodes:
        elaboration+=f" Entry nodes: {', '.join(entry_nodes[:3])}."
    if high_fanout and high_fanout[0]:
        elaboration+=f" High fanout: {high_fanout[0]} ({outdeg.get(high_fanout[0],0)} outs)."
    flow_summary=(base_summary+elaboration).strip()[:600]
    return {
        'program_id': path.stem.upper(),
        'node_count': len(nodes),
        'edge_count': len(edges),
        'max_depth': max_depth,
        'has_cycles': has_cycle,
        'flow_nodes_json': json.dumps(nodes)[:16000],
        'flow_edges_json': json.dumps([{'src':s,'dst':d} for s,d in edges])[:32000],
        'entry_nodes_json': json.dumps(entry_nodes)[:4000],
        'exit_nodes_json': json.dumps(exit_nodes)[:4000],
        'high_fanout_nodes_json': json.dumps(high_fanout)[:4000],
        'path_samples_json': json.dumps(path_samples)[:8000],
        'mermaid_flow': mermaid_flow,
        'condensed_mermaid': condensed_mermaid,
        'flow_summary': flow_summary,
        'perform_thru_unexpanded_count': unexpanded_thru,
        'paragraph_roles_json': json.dumps(role_map)[:8000],
        'risk_score': risk_score,
    }


def fetch_existing(ep,key,program_ids:List[str]) -> Set[str]:
    # return subset already present (avoid overwrite unless requested)
    existing=set()
    url=f"{ep}/indexes/{INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    for pid in program_ids:
        body={'search':pid,'filter':f"program_id eq '{pid}'",'top':1,'select':'program_id'}
        r=requests.post(url,headers=headers,json=body)
        if r.status_code==200 and r.json().get('value'):
            existing.add(pid)
    return existing


def upload(ep,key,docs:List[Dict]):
    if not docs: return
    url=f"{ep}/indexes/{INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    ts=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    for d in docs:
        d['updated_at']=ts
        d['@search.action']='mergeOrUpload'
    for i in range(0,len(docs),500):
        r=requests.post(url,headers=headers,json={'value':docs[i:i+500]})
        if r.status_code not in (200,201):
            raise SystemExit(f"Upload failed {r.status_code}: {r.text[:300]}")
    print('Uploaded',len(docs),'flow docs')

def get_index_fields(ep,key,index_name:str) -> Set[str]:
    url=f"{ep}/indexes/{index_name}?api-version={API_VERSION}"
    r=requests.get(url,headers={'api-key':key})
    if r.status_code!=200:
        print(f"[warn] unable to fetch index schema {index_name}: {r.status_code}")
        return set()
    js=r.json()
    fields=set()
    for f in js.get('fields',[]):
        name=f.get('name')
        if name: fields.add(name)
    return fields


# ---------------- Index Mode Helpers -----------------
def search(ep,key,index,body):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json=body)
    if r.status_code!=200:
        raise SystemExit(f"Search {index} failed {r.status_code}: {r.text[:300]}")
    return r.json()

## Removed iter_program_ids: program list now sourced via catalog.discover_program_ids

def fetch_paragraphs_for_program(ep,key, program_id):
    paras=[]; skip=0
    while True:
        body={'search':program_id,'filter':f"program_id eq '{program_id}'",'top':1000,'skip':skip,'select':'paragraph_name'}
        js=search(ep,key,PARA_INDEX,body)
        vals=js.get('value',[])
        for v in vals:
            name=(v.get('paragraph_name') or '').upper()
            if name: paras.append(name)
        if len(vals)<1000: break
        skip+=1000
    return paras

def fetch_edges_for_program(ep,key, program_id, use_v2=False):
    index = EDGE_INDEX_V2 if use_v2 else EDGE_INDEX
    edges=[]; skip=0
    while True:
        body={'search':program_id,'filter':f"program_id eq '{program_id}'",'top':1000,'skip':skip,'select':'caller_para,target_para,resolved_target_para'}
        js=search(ep,key,index,body)
        vals=js.get('value',[])
        for v in vals:
            src=(v.get('caller_para') or '').upper()
            tgt=(v.get('resolved_target_para') or v.get('target_para') or '').upper()
            if src and tgt and src!=tgt:
                edges.append((src,tgt))
        if len(vals)<1000: break
        skip+=1000
    return list(dict.fromkeys(edges))

def build_doc_from_index(program_id, nodes, edges):
    if not nodes:
        return None
    adj={n:[] for n in nodes}
    indeg={n:0 for n in nodes}; outdeg={n:0 for n in nodes}
    for s,d in edges:
        adj.setdefault(s,[]).append(d)
        outdeg[s]=outdeg.get(s,0)+1
        indeg[d]=indeg.get(d,0)+1
    visited=set(); stack=set(); has_cycle=False
    def dfs_cycle(n):
        nonlocal has_cycle
        if n in stack: has_cycle=True; return
        if n in visited: return
        visited.add(n); stack.add(n)
        for nxt in adj.get(n,[]): dfs_cycle(nxt)
        stack.remove(n)
    for n in nodes:
        if n not in visited: dfs_cycle(n)
    def depth(n,d,seen):
        if d>MAX_DEPTH_LIMIT: return d
        best=d
        for nxt in adj.get(n,[]):
            if nxt in seen: continue
            best=max(best, depth(nxt,d+1, seen|{nxt}))
        return best
    max_depth=0
    for n in nodes:
        max_depth=max(max_depth, depth(n,1,set()))
    entry_nodes=[n for n in nodes if indeg.get(n,0)==0]
    exit_nodes=[n for n in nodes if outdeg.get(n,0)==0]
    high_fanout=sorted(nodes,key=lambda x: outdeg.get(x,0), reverse=True)[:3]
    # Build paragraph role map (re-using simple heuristics from earlier version if present)
    role_map={}
    def classify(name:str)->str:
        u=name.upper()
        if any(x in u for x in ('INIT','START','SETUP')): return 'INIT'
        if any(x in u for x in ('VAL','CHECK','TEST')): return 'VALIDATE'
        if any(x in u for x in ('CALC','COMP','TOTAL','ACCUM')): return 'CALC'
        if any(x in u for x in ('READ','WRITE','DISPLAY','ACCEPT','OPEN','CLOSE')): return 'IO'
        if any(x in u for x in ('FINAL','END','TERM','WRAP')): return 'FINAL'
        return 'OTHER'
    for n in nodes:
        role_map[n]=classify(n)
    path_samples=[]; MAX_PATHS=4; MAX_PATH_LEN=12
    unexpanded_thru=0  # future hook for PERFORM THRU expansion metrics
    def dfs_paths(start):
        stack_local=[(start,[start])]; local=0; seen_paths=set()
        while stack_local and len(path_samples)<MAX_PATHS and local<MAX_PATHS*4:
            node,path=stack_local.pop()
            outs=adj.get(node,[])
            if not outs or len(path)>=MAX_PATH_LEN:
                key='>'.join(path)
                if key not in seen_paths:
                    path_samples.append(path[:]); seen_paths.add(key)
                continue
            for nxt in outs[:6]:
                if nxt in path: continue
                stack_local.append((nxt,path+[nxt]))
            local+=1
    for e in entry_nodes[:5]:
        if len(path_samples)>=MAX_PATHS: break
        dfs_paths(e)
    def condensed_edges():
        consumed=set(); new_edges=[]
        for n in nodes:
            if n in consumed: continue
            if outdeg.get(n,0)==1:
                chain=[n]; cur=n
                while True:
                    nxts=adj.get(cur,[])
                    if len(nxts)!=1: break
                    nxt=nxts[0]
                    if indeg.get(nxt,0)!=1: break
                    if outdeg.get(nxt,0)!=1:
                        chain.append(nxt); consumed.update(chain[1:-1]); break
                    chain.append(nxt); cur=nxt
                    if len(chain)>15: break
                if len(chain)>2:
                    new_edges.append((chain[0],chain[-1],chain[1:-1]))
        return new_edges
    collapsed=condensed_edges()
    mermaid=['flowchart TD']
    for s,d in edges: mermaid.append(f"  {s}--> {d}")
    mermaid_flow='\n'.join(mermaid)[:20000]
    condensed=['flowchart TD']
    collapsed_pairs={(c[0],c[1]):c[2] for c in collapsed}; used=set()
    for s,d in edges:
        key=(s,d)
        if key in collapsed_pairs and key not in used:
            condensed.append(f"  {s}--> {d}:::chain"); used.add(key)
        else:
            condensed.append(f"  {s}--> {d}")
    condensed_mermaid='\n'.join(condensed)[:20000]
    max_fanout=max(outdeg.values()) if outdeg else 0
    risk_score=round(0.3*(1 if has_cycle else 0)+0.3*min(1.0,max_depth/20)+0.4*min(1.0,max_fanout/10),3)
    summary=(f"Paragraphs={len(nodes)}; edges={len(edges)}; depth={max_depth}; cycles={'Y' if has_cycle else 'N'}; "
             f"entries={len(entry_nodes)}; exits={len(exit_nodes)}; fanout_max={max_fanout}; risk={risk_score}.")
    if entry_nodes:
        summary+=f" Entry: {entry_nodes[0]}"+("..." if len(entry_nodes)>1 else "")+"."
    if high_fanout:
        summary+=f" High fanout: {high_fanout[0]} ({outdeg.get(high_fanout[0],0)})."
    return {
        'program_id': program_id,
        'node_count': len(nodes),
        'edge_count': len(edges),
        'max_depth': max_depth,
        'has_cycles': has_cycle,
        'flow_nodes_json': json.dumps(nodes)[:16000],
        'flow_edges_json': json.dumps([{'src':s,'dst':d} for s,d in edges])[:32000],
        'entry_nodes_json': json.dumps(entry_nodes)[:4000],
        'exit_nodes_json': json.dumps(exit_nodes)[:4000],
        'high_fanout_nodes_json': json.dumps(high_fanout)[:4000],
        'path_samples_json': json.dumps(path_samples)[:8000],
        'mermaid_flow': mermaid_flow,
        'condensed_mermaid': condensed_mermaid,
        'flow_summary': summary[:600],
        'perform_thru_unexpanded_count': unexpanded_thru,
        'paragraph_roles_json': json.dumps(role_map)[:8000],
        'risk_score': risk_score,
    }

def summarise_flow(doc):
    # Build multi-sentence (3-6) summary.
    def _safe(field):
        raw = doc.get(field, '[]') or '[]'
        try:
            return json.loads(raw)
        except Exception:
            return []
    sentences=[]
    sentences.append(f"Program {doc['program_id']} has {doc['node_count']} paragraphs and {doc['edge_count']} perform edges; max depth {doc['max_depth']} and cycles={'present' if doc['has_cycles'] else 'absent' }.")
    entries=_safe('entry_nodes_json')
    exits=_safe('exit_nodes_json')
    if entries:
        sentences.append(f"Entry paragraphs: {', '.join(entries[:3])}{'...' if len(entries)>3 else ''}.")
    if exits:
        sentences.append(f"Exit paragraphs: {', '.join(exits[:3])}{'...' if len(exits)>3 else ''}.")
    high=_safe('high_fanout_nodes_json')
    if high:
        sentences.append(f"High fan-out paragraph: {high[0]} (top branching).")
    paths=_safe('path_samples_json')
    if paths:
        pretty=['>'.join(p[:5]) + ('...' if len(p)>5 else '') for p in paths[:2]]
        sentences.append('Sample paths: ' + '; '.join(pretty) + '.')
    sentences.append(f"Risk score {doc.get('risk_score',0)} based on depth, fan-out and cycles.")
    if len(sentences)>SUMMARY_SENTENCE_LIMIT:
        sentences=sentences[:SUMMARY_SENTENCE_LIMIT]
    return ' '.join(sentences)[:600]

def _locate_program_file(pid:str, src_root:str) -> pathlib.Path | None:
    """Best-effort resolve program file path by matching base name (without extension)."""
    root=pathlib.Path(src_root)
    if not root.exists():
        return None
    base=pid.upper()
    # Try direct matches first (most repos keep uppercase names)
    for ext in ('.CBL','.COB','.cbl','.cob'):
        cand=root / f"{base}{ext}"
        if cand.exists():
            return cand
    # Fallback: glob expensive but only used when edges missing
    try:
        for p in root.rglob('*'):
            if not p.is_file():
                continue
            if p.suffix.lower() not in ('.cbl','.cob'):
                continue
            if p.stem.upper()==base:
                return p
    except Exception:
        pass
    return None

def _scan_edges_from_file(pid:str, src_root:str, nodes:List[str]) -> List[Tuple[str,str]]:
    path=_locate_program_file(pid,src_root)
    if not path:
        return []
    try:
        text=path.read_text(encoding='utf-8',errors='ignore')
    except Exception:
        return []
    node_set=set(nodes)
    edges=[]
    lines=text.splitlines()
    current=None
    for i,line in enumerate(lines):
        m=PARA_LABEL_RE.match(line.strip())
        if m:
            label=m.group('label').upper()
            if label in node_set:
                current=label
            else:
                current=None
            continue
        if not current:
            continue
        # Within a paragraph body
        for mt in PERFORM_THRU_RE.finditer(line):
            a=mt.group(1).upper(); b=mt.group(2).upper()
            if a in node_set and b in node_set and a!=b:
                edges.append((current,a))
                if b!=a:
                    edges.append((current,b))
        for m in PERFORM_RE.finditer(line):
            tgt=m.group(1).upper()
            if tgt in node_set and tgt!=current:
                edges.append((current,tgt))
    return list(dict.fromkeys(edges))

def ensure_doc(ep,key,pid,use_v2_edges,embed,embed_batch, allowed_fields: Set[str] | None=None, src_root: str='cobol_src', fallback_scan: bool=False):
    nodes=fetch_paragraphs_for_program(ep,key,pid)
    edges=fetch_edges_for_program(ep,key,pid,use_v2=use_v2_edges) if nodes else []
    # Adaptive fallback: if we have nodes but zero edges and fallback_scan requested, attempt lightweight file scan
    if fallback_scan and nodes and not edges:
        alt=_scan_edges_from_file(pid,src_root,nodes)
        if alt:
            edges=alt
            # print minimal debug occasionally
            if len(alt)>0:
                print(f"[fallback-scan] injected {len(alt)} edges for {pid}")
    if not nodes:
        # Degenerate doc
        empty={
            'program_id':pid,
            'node_count':0,'edge_count':0,'max_depth':0,'has_cycles':False,
            'flow_nodes_json':json.dumps([]),'flow_edges_json':json.dumps([]),
            'entry_nodes_json':json.dumps([]),'exit_nodes_json':json.dumps([]),
            'high_fanout_nodes_json':json.dumps([]),'path_samples_json':json.dumps([]),
            'mermaid_flow':'flowchart TD','condensed_mermaid':'flowchart TD','flow_summary':f'Program {pid} has no detected paragraphs (degenerate flow).',
            'perform_thru_unexpanded_count':0,
            'paragraph_roles_json':json.dumps({}),
            'risk_score':0.0
        }
        if embed:
            vec=batch_embed([empty['flow_summary']],batch_size=1,target_dim=FLOW_VECTOR_DIM)[0]
            empty[VECTOR_FIELD]=vec; empty[HAS_FIELD]=True
        else:
            empty[HAS_FIELD]=False
        if allowed_fields:
            for k in list(empty.keys()):
                if k not in allowed_fields and not k.startswith('@'):
                    empty.pop(k,None)
        return empty
    base=build_doc_from_index(pid,nodes,edges)
    base['flow_summary']=summarise_flow(base)
    if embed:
        vec=batch_embed([base['flow_summary']],batch_size=1,target_dim=FLOW_VECTOR_DIM)[0]
        base[VECTOR_FIELD]=vec; base[HAS_FIELD]=True
    else:
        base[HAS_FIELD]=False
    if allowed_fields:
        for k in list(base.keys()):
            if k not in allowed_fields and not k.startswith('@'):
                base.pop(k,None)
    return base

def build_index_mode(ep,key, program_ids, batch, push, overwrite_existing, embed_batch, use_v2_edges=False, embed=True, src_root: str='cobol_src', fallback_scan: bool=False):
    total=len(program_ids); processed=0; uploaded_total=0
    allowed_fields=get_index_fields(ep,key,INDEX)
    if allowed_fields:
        print(f"[schema] index has {len(allowed_fields)} fields; filtering docs dynamically")
    while processed<total:
        chunk_ids=program_ids[processed:processed+batch]
        docs=[ensure_doc(ep,key,pid,use_v2_edges,embed,embed_batch, allowed_fields=allowed_fields, src_root=src_root, fallback_scan=fallback_scan) for pid in chunk_ids]
        if processed==0 and docs:
            af_sorted=sorted(list(allowed_fields)) if allowed_fields else []
            print('[debug] allowed_fields count:', len(af_sorted))
            print('[debug] allowed_fields first 25:', af_sorted[:25])
            print('[debug] contains program_id:', 'program_id' in allowed_fields if allowed_fields else None)
            print('[debug] first doc program_id:', docs[0].get('program_id'))
            print('[debug] first doc keys:', list(docs[0].keys()))
        if push:
            if not overwrite_existing:
                existing=fetch_existing(ep,key,[d['program_id'] for d in docs])
                docs=[d for d in docs if d['program_id'] not in existing]
            upload(ep,key,docs); uploaded_total+=len(docs)
        else:
            print(json.dumps(docs[0],indent=2)[:600])
        processed+=len(chunk_ids)
        pct=processed/total*100
        print(f"[build] processed {processed}/{total} ({pct:.1f}%) uploaded={uploaded_total}")
    return uploaded_total

# --------- Edge Scan Fallback (no paragraph index dependency) ---------
def fetch_edges_program_grouped(ep,key, limit_programs=None, max_edges_per_program=20000):
    """Scan edges index in pages, grouping by program_id.

    Stops when all discovered program groups reach max_edges_per_program or edges exhausted.
    If limit_programs provided (set), only collect those program_ids.
    Returns dict program_id -> list[(caller,target)].
    """
    url=f"{ep}/indexes/{EDGE_INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    groups={}

    # Optimized path: if specific program subset provided, query each directly with a filter
    if limit_programs:
        for pid in limit_programs:
            top=1000; skip=0; collected=[]
            # Pass 1: program_id filter
            while True:
                body={'search':'*','top':top,'skip':skip,'filter':f"program_id eq '{pid}'",
                      'select':'program_id,caller_para,target_para,resolved_target_para'}
                r=requests.post(url,headers=headers,json=body)
                if r.status_code!=200:
                    print(f"[edge-scan] stop on error for {pid}",r.status_code,r.text[:160]); break
                vals=r.json().get('value',[])
                if not vals: break
                for v in vals:
                    src=(v.get('caller_para') or '').upper(); tgt=(v.get('resolved_target_para') or v.get('target_para') or '').upper()
                    if not src or not tgt or src==tgt: continue
                    if len(collected)<max_edges_per_program:
                        collected.append((src,tgt))
                skip+=len(vals)
                if len(vals)<top or len(collected)>=max_edges_per_program: break
                if skip>=99000: # avoid Search 100k skip hard limit
                    print(f"[edge-scan] reached skip limit for {pid}; collected {len(collected)} edges so far (program_id filter)")
                    break
            # If still empty, fallback to file_id filter (program_id may be missing in many docs)
            if not collected:
                top=1000; skip=0
                while True:
                    body={'search':'*','top':top,'skip':skip,'filter':f"file_id eq '{pid}'",
                          'select':'program_id,caller_para,target_para,resolved_target_para'}
                    r=requests.post(url,headers=headers,json=body)
                    if r.status_code!=200:
                        print(f"[edge-scan] fallback error for {pid}",r.status_code,r.text[:160]); break
                    vals=r.json().get('value',[])
                    if not vals: break
                    for v in vals:
                        src=(v.get('caller_para') or '').upper(); tgt=(v.get('resolved_target_para') or v.get('target_para') or '').upper()
                        if not src or not tgt or src==tgt: continue
                        if len(collected)<max_edges_per_program:
                            collected.append((src,tgt))
                    skip+=len(vals)
                    if len(vals)<top or len(collected)>=max_edges_per_program: break
                    if skip>=99000:
                        print(f"[edge-scan] reached skip limit for {pid}; collected {len(collected)} edges so far (file_id fallback)")
                        break
                if collected:
                    print(f"[edge-scan] used file_id fallback for {pid}; edges={len(collected)}")
            # Last resort: lexical search for program token (may surface inter-program call edges)
            if not collected:
                r=requests.post(url,headers=headers,json={'search':pid,'top':2000,'select':'program_id,caller_para,target_para,resolved_target_para'})
                if r.status_code==200:
                    vals=r.json().get('value',[])
                    # Heuristic: keep only edges where caller contains pid token (likely paragraph name inside program)
                    token=pid.upper()
                    for v in vals:
                        src=(v.get('caller_para') or '').upper(); tgt=(v.get('resolved_target_para') or v.get('target_para') or '').upper()
                        if token not in src: continue
                        if not src or not tgt or src==tgt: continue
                        if len(collected)<max_edges_per_program:
                            collected.append((src,tgt))
                if collected:
                    print(f"[edge-scan] lexical search fallback for {pid}; edges={len(collected)}")
            if collected:
                groups[pid]=collected
        return groups

    # General scan path (no limit_programs): may hit skip limit for very large indexes
    top=1000; skip=0
    while True:
        body={'search':'*','top':top,'skip':skip,'select':'program_id,caller_para,target_para,resolved_target_para'}
        r=requests.post(url,headers=headers,json=body)
        if r.status_code!=200:
            print('[edge-scan] stop on error',r.status_code,r.text[:200]); break
        vals=r.json().get('value',[])
        if not vals: break
        for v in vals:
            pid=(v.get('program_id') or '').upper()
            if not pid: continue
            src=(v.get('caller_para') or '').upper(); tgt=(v.get('resolved_target_para') or v.get('target_para') or '').upper()
            if not src or not tgt or src==tgt: continue
            lst=groups.setdefault(pid,[])
            if len(lst)<max_edges_per_program:
                lst.append((src,tgt))
        skip+=len(vals)
        if len(vals)<top: break
        if skip>=99000:
            print('[edge-scan] reached global skip limit (~100k); stopping scan')
            break
    return groups

def build_from_edges(ep,key, program_subset, batch, push, overwrite_existing, embed_batch):
    limit_programs=set(p.upper() for p in program_subset) if program_subset else None
    groups=fetch_edges_program_grouped(ep,key, limit_programs=limit_programs)
    if not groups:
        # Paragraph-only fallback: build docs with zero edges if paragraphs exist
        if limit_programs:
            print('[edge-scan] no edge groups; attempting paragraph-only fallback for requested programs')
            groups={}
            for pid in limit_programs:
                paras=fetch_paragraphs_for_program(ep,key,pid)
                if paras:
                    groups[pid]=[]
            if not groups:
                print('[edge-scan] paragraph-only fallback also empty');
                return 0
        else:
            print('[edge-scan] no groups collected'); return 0
    docs=[]
    for pid, edges in groups.items():
        # derive nodes
        nodes=sorted({n for pair in edges for n in pair}) if edges else fetch_paragraphs_for_program(ep,key,pid)
        doc=build_doc_from_index(pid,nodes,edges)
        if doc: docs.append(doc)
    if not docs:
        print('[edge-scan] no docs built'); return 0
    vecs=batch_embed([d['flow_summary'] for d in docs], batch_size=embed_batch, target_dim=FLOW_VECTOR_DIM)
    for d,v in zip(docs,vecs):
        d[VECTOR_FIELD]=v; d[HAS_FIELD]=True
    if not push:
        print('[edge-scan] dry-run sample:', json.dumps(docs[0],indent=2)[:600])
        print('[edge-scan] prepared docs:',len(docs))
        return len(docs)
    if not overwrite_existing:
        existing=fetch_existing(ep,key,[d['program_id'] for d in docs])
        docs=[d for d in docs if d['program_id'] not in existing]
    upload(ep,key,docs)
    print(f'[edge-scan] uploaded {len(docs)} docs')
    return len(docs)


def main():
    ap=argparse.ArgumentParser(description='Build program flow graphs (paragraph PERFORM graph) into new_cobol_program_flows.')
    ap.add_argument('--src-root',default='cobol_src')
    ap.add_argument('--all',action='store_true',help='Process all discovered programs (discover_program_ids)')
    ap.add_argument('--programs',help='Comma-separated explicit subset')
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--overwrite-existing',action='store_true')
    ap.add_argument('--batch',type=int,default=200)
    ap.add_argument('--embed-batch',type=int,default=64)
    ap.add_argument('--no-embed',action='store_true',help='Skip embedding (sets has_vector False; run backfill later)')
    ap.add_argument('--use-v2-edges',action='store_true')
    ap.add_argument('--fallback-scan',action='store_true',help='If no edges from index, attempt local file PERFORM scan for basic edges')
    args=ap.parse_args(); load_settings(); ep,key=resolve()
    # Determine program list
    if args.programs:
        program_ids=[p.strip().upper() for p in args.programs.split(',') if p.strip()]
    elif args.all:
        program_ids=catalog.discover_program_ids(ep,key,args.src_root,disable_index=False)
    else:
        print('Specify --all or --programs P1,P2'); return
    before=len(program_ids)
    program_ids=[p.strip().upper() for p in program_ids if p and p.strip()]
    removed=before-len(program_ids)
    if removed:
        print(f"[debug] filtered out {removed} blank/empty program ids (from {before} to {len(program_ids)})")
    print('[debug] first 10 program_ids:', program_ids[:10])
    if not program_ids:
        print('No programs to process.'); return
    print(f'Discovered {len(program_ids)} programs; batch={args.batch} embed={not args.no_embed}')
    uploaded=build_index_mode(ep,key, program_ids, args.batch, args.push, args.overwrite_existing, args.embed_batch, use_v2_edges=args.use_v2_edges, embed=not args.no_embed, src_root=args.src_root, fallback_scan=args.fallback_scan)
    if args.push:
        print(f'Completed upload of {uploaded} docs (coverage target >=95%).')

if __name__=='__main__':
    main()
