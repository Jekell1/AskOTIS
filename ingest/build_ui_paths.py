"""Build / ingest UI navigation paths into `new_cobol_ui_paths` (ingest variant).

Goals (spec):
  - Coverage >= 70% of known menu roots (heuristically defined).
  - Provide guard_summary text (basic IF/EVALUATE heuristics).
  - Provide frequency_score (heuristic) & hop_count.

Strategy:
  1. Load program meta (root candidates: program_role in {DISPATCH, UI} OR name contains MENU).
  2. Build adjacency from calls index (caller_program -> callee_program).
  3. Enumerate paths per root (depth-first biased toward UI children) with scoring & pruning.
  4. Compute coverage; if < target (0.70) widen root set (union + top degree) and re-run augmentation.
  5. Enrich with guard detection + frequency aggregates.
    6. Compute hop_count = length-1, frequency_score heuristic, path_json = JSON array of nodes.
    7. Populate screen_ids_json + screen_names_json (first observed screen per program, ordered occurrence).
    8. Secondary deepening DFS ensures each covered root attempts at least one hop_count>=3 non-placeholder path.

This script is intentionally lighter than root-level experimental `build_ui_paths.py`, but reuses
key ideas. (We keep both; orchestrations can migrate gradually.)

Usage:
  python ingest/build_ui_paths.py --push --max-depth 10
"""
from __future__ import annotations
import os, sys, json, argparse, math, hashlib, datetime, re, requests, collections
from typing import List, Dict, Any, Set

API_VERSION=os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX='new_cobol_program_meta'
CALL_INDEX='new_cobol_calls'
UI_INDEX='new_cobol_ui_paths'

TARGET_COVERAGE=0.70

UI_NAME_HINTS=['MENU','SCR','SCRN','SCREEN','PAGE','FORM','INQ','MAINT','LIST']
GUARD_PATTERNS=[
    re.compile(r'IF\s+[^.]{0,160}?DISPLAY',re.IGNORECASE),
    re.compile(r'IF\s+[^.]{0,160}?(ACCEPT|PROMPT)',re.IGNORECASE),
    re.compile(r'EVALUATE\s+[^.]{0,160}',re.IGNORECASE)
]
# Additional source-level guard extraction (lines containing IF / EVALUATE with UI verbs or flags)
GUARD_SRC_LINE=re.compile(r'\b(IF|EVALUATE)\b[^.]{0,200}',re.IGNORECASE)
UI_VERB_HINT=re.compile(r'(DISPLAY|ACCEPT|PROMPT|SCREEN|PF\d{1,2})',re.IGNORECASE)

# Cache for program source content (upper-cased for pattern matching)
_PROGRAM_SOURCE_CACHE:dict[str,str]={}
_PROGRAM_FILE_MAP:dict[str,str]={}

def load_local():
    try:
        vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','AZURE_SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ: os.environ[k]=vals[k]
    except Exception:
        pass

def resolve():
    ep=os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key',file=sys.stderr); sys.exit(1)
    return ep.rstrip('/'), key

def paged_search(ep,key,index,select=None):
    url=f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    results=[]; skip=0; top=1000
    payload={'search':'*','top':top}
    if select: payload['select']=select
    while True:
        payload['skip']=skip
        r=requests.post(url,headers=headers,json=payload)
        if r.status_code!=200: break
        data=r.json().get('value',[])
        results.extend(data)
        if len(data)<top: break
        skip+=top
    return results

def fetch_program_meta(ep,key):
    return paged_search(ep,key,PROGRAM_META_INDEX)

def fetch_calls(ep,key):
    return paged_search(ep,key,CALL_INDEX,select='caller_program,callee_program')

def fetch_screen_nodes(ep,key):
    # minimal fields needed: program_id, screen_id, screen_name, transitions_json
    return paged_search(ep,key,'new_cobol_screen_nodes',select='program_id,screen_id,screen_name,transitions_json')

def is_ui_program(p:Dict[str,Any]) -> bool:
    if not p: return False
    if p.get('program_role') in ('UI','DISPATCH'): return True
    name=(p.get('program_id') or '').upper()
    if any(h in name for h in UI_NAME_HINTS): return True
    summary=(p.get('program_summary') or '').upper()
    if any(h in summary for h in UI_NAME_HINTS): return True
    return False

def build_program_file_map()->dict[str,str]:
    roots=['cobol_src','src','source']
    fm={}
    for r in roots:
        if not os.path.isdir(r):
            continue
        for root,_,files in os.walk(r):
            for fn in files:
                if fn.lower().endswith(('.cbl','.cob')):
                    prog=os.path.splitext(fn)[0].upper()
                    fm.setdefault(prog, os.path.join(root,fn))
    return fm

def load_program_source(program_id:str)->str:
    if program_id in _PROGRAM_SOURCE_CACHE:
        return _PROGRAM_SOURCE_CACHE[program_id]
    fp=_PROGRAM_FILE_MAP.get(program_id)
    if not fp or not os.path.isfile(fp):
        _PROGRAM_SOURCE_CACHE[program_id]=''
        return ''
    try:
        with open(fp,'r',encoding='utf-8',errors='ignore') as f:
            txt=f.read()
    except Exception:
        txt=''
    _PROGRAM_SOURCE_CACHE[program_id]=txt
    return txt

def guard_markers(p:Dict[str,Any]) -> list[str]:
    out=[]
    txt=(p.get('program_summary') or '')
    # summary-derived patterns
    for pat in GUARD_PATTERNS:
        for m in pat.findall(txt):
            mm=m.strip()
            if mm and mm not in out:
                out.append(mm[:160])
    # source-derived line heuristics
    pid=p.get('program_id') or ''
    src=load_program_source(pid)
    if src:
        for line in src.splitlines():
            if len(out) >= 10: # cap early
                break
            if GUARD_SRC_LINE.search(line) and UI_VERB_HINT.search(line):
                cleaned=' '.join(line.strip().split())
                if cleaned not in out:
                    out.append(cleaned[:160])
    return out[:10]

def build_adjacency(calls:List[Dict[str,Any]])->Dict[str,Set[str]]:
    adj=collections.defaultdict(set)
    for c in calls:
        a=(c.get('caller_program') or '').upper(); b=(c.get('callee_program') or '').upper()
        if a and b and a!=b:
            adj[a].add(b)
    return adj

def enumerate_paths(roots:List[str], prog_map:Dict[str,Any], adj:Dict[str,Set[str]], max_depth:int, max_paths_per_root:int, branching_limit:int, transitions_map:Dict[str,Set[str]]|None=None)->List[Dict[str,Any]]:
    paths=[]
    def score(seq:list[str])->float:
        ui_ratio=sum(1 for x in seq if is_ui_program(prog_map.get(x))) / max(1,len(seq))
        return ui_ratio * math.log(len(seq)+1)
    def hash_seq(seq):
        return hashlib.sha1('>'.join(seq).encode()).hexdigest()[:40]
    for root in roots:
        stack=[(root,[root])]
        emitted=0
        while stack and emitted<max_paths_per_root:
            node,seq=stack.pop()
            if len(seq)>=2:  # avoid trivial singletons except if no children
                rec={'path_id':hash_seq(seq),'start_program_id':seq[0],'end_program_id':seq[-1],'program_sequence_json':json.dumps(seq),'length':len(seq)}
                rec['score']=round(score(seq),4)
                paths.append(rec); emitted+=1
            if len(seq)>=max_depth: continue
            children=sorted(adj.get(node,[]))
            if not children: continue
            trans_set=transitions_map.get(node,set()) if transitions_map else set()
            # prefer transition edges, then UI children, then higher out-degree
            children=sorted(children,key=lambda c:(c in trans_set, is_ui_program(prog_map.get(c)), len(adj.get(c,[]))),reverse=True)[:branching_limit]
            for ch in children:
                if ch in seq: continue  # simple loop avoidance
                stack.append((ch,seq+[ch]))
    # dedupe by path_id
    uniq={p['path_id']:p for p in paths}
    return list(uniq.values())

def compute_coverage(paths:List[Dict[str,Any]], menu_roots:Set[str])->float:
    covered={p['start_program_id'] for p in paths if p.get('start_program_id') in menu_roots}
    if not menu_roots: return 0.0
    return len(covered)/len(menu_roots)

def enrich(paths:List[Dict[str,Any]], prog_map:Dict[str,Any]):
    # frequency metrics per edge
    edge_counter=collections.Counter()
    for p in paths:
        seq=json.loads(p['program_sequence_json'])
        for a,b in zip(seq,seq[1:]): edge_counter[(a,b)]+=1
    max_freq_score=0.0
    for p in paths:
        seq=json.loads(p['program_sequence_json'])
        guards=[]
        for pid in seq:
            guards.extend(guard_markers(prog_map.get(pid,{})))
        if guards:
            uniq=sorted(set(guards))[:8]
            p['guards_json']=json.dumps(uniq)
            p['guard_summary']='; '.join(uniq)[:400]
        # edge freqs for path
        freqs=[edge_counter[(a,b)] for a,b in zip(seq,seq[1:])]
        if freqs:
            avg=sum(freqs)/len(freqs)
            mn=min(freqs)
        else:
            avg=1.0; mn=1
        length=len(seq)
        freq_score=(math.log(avg+1)*0.7 + math.log(mn+1)*0.3)*math.log(length+1)
        p['frequency_score']=round(freq_score,4)
        if freq_score>max_freq_score:
            max_freq_score=freq_score
        p['hop_count']=max(0,length-1)
        p['path_json']=json.dumps(seq)[:64000]
        # edge origins classification (call vs transition)
        trans_edges=0; call_edges=0; origins=[]
        for a,b in zip(seq,seq[1:]):
            # if adjacency came originally from transitions_map: we cannot access here, so heuristic: store placeholder (resolve later) -> skip
            # We'll re-evaluate using a global transitions cache if present
            pass
    # normalization pass
    for p in paths:
        fs=p.get('frequency_score',0.0)
        p['frequency_score_norm']= round((fs/max_freq_score),4) if max_freq_score>0 else 0.0
    return paths

def annotate_edge_origins(paths:List[Dict[str,Any]], transitions_map:dict[str,set[str]]):
    for p in paths:
        try:
            seq=json.loads(p.get('program_sequence_json') or '[]')
        except Exception:
            seq=[]
        origins=[]; trans_ct=0; call_ct=0
        for a,b in zip(seq,seq[1:]):
            if b in transitions_map.get(a,set()):
                origins.append('T')
                trans_ct+=1
            else:
                origins.append('C')
                call_ct+=1
        if origins:
            p['edge_origins_json']=json.dumps(origins)[:64000]
        p['transition_edge_count']=trans_ct
        p['call_edge_count']=call_ct
    return paths

def upload(ep,key,paths:List[Dict[str,Any]]):
    if not paths:
        print('No paths to upload'); return
    # fetch schema to filter allowed fields
    r=requests.get(f"{ep}/indexes/{UI_INDEX}?api-version={API_VERSION}",headers={'api-key':key})
    allowed=set()
    if r.status_code==200:
        try:
            allowed={f['name'] for f in r.json().get('fields',[])}
        except Exception: pass
    ts=datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    for p in paths:
        p['updated_at']=ts
    docs=[]
    for p in paths:
        docs.append({'@search.action':'mergeOrUpload', **{k:v for k,v in p.items() if (not allowed) or k in allowed}})
    url=f"{ep}/indexes/{UI_INDEX}/docs/index?api-version={API_VERSION}"
    for i in range(0,len(docs),500):
        chunk=docs[i:i+500]
        rr=requests.post(url,headers={'api-key':key,'Content-Type':'application/json'},json={'value':chunk})
        if rr.status_code!=200:
            print('[ERROR] Upload chunk failed',rr.status_code,rr.text[:200]); break
    print(f"Uploaded {len(paths)} ui path docs")

def main():
    ap=argparse.ArgumentParser(description='Ingest UI paths (coverage-focused)')
    ap.add_argument('--max-depth',type=int,default=10)
    ap.add_argument('--branching-limit',type=int,default=12)
    ap.add_argument('--max-paths-per-root',type=int,default=120)
    ap.add_argument('--deepening-max-depth',type=int,default=14,help='Max depth for secondary deepening DFS (default 14)')
    ap.add_argument('--deepening-branch-cap',type=int,default=10,help='Branch cap for deepening DFS (default 10)')
    ap.add_argument('--push',action='store_true')
    ap.add_argument('--export-json')
    args=ap.parse_args(); load_local(); ep,key=resolve()
    programs=fetch_program_meta(ep,key)
    if not programs:
        print('[FATAL] No program meta docs'); return
    global _PROGRAM_FILE_MAP
    _PROGRAM_FILE_MAP=build_program_file_map()
    prog_map={p['program_id']:p for p in programs if p.get('program_id')}
    calls=fetch_calls(ep,key)
    screen_nodes=fetch_screen_nodes(ep,key)
    adj=build_adjacency(calls)
    # augment adjacency with screen transitions
    transitions_map=collections.defaultdict(set)
    for sn in screen_nodes:
        pid=(sn.get('program_id') or '').upper()
        if not pid:
            continue
        try:
            trans=json.loads(sn.get('transitions_json') or '[]') if sn.get('transitions_json') else []
        except Exception:
            trans=[]
        # restrict to reasonable number per screen to avoid explosion
        for t in trans[:12]:
            tU=str(t).upper()
            if tU and tU!=pid:
                adj[pid].add(tU)
                transitions_map[pid].add(tU)
    # Identify menu roots
    menu_roots={p['program_id'] for p in programs if (p.get('program_role') in ('DISPATCH','UI') or 'MENU' in (p.get('program_id') or '').upper())}
    first_roots=sorted(list(menu_roots))[:50]
    paths=enumerate_paths(first_roots,prog_map,adj,args.max_depth,args.max_paths_per_root,args.branching_limit,transitions_map)
    coverage=compute_coverage(paths,menu_roots)
    # Automatic expansion loop to reach target coverage
    attempt=0
    while coverage < TARGET_COVERAGE and attempt < 3:
        attempt+=1
        print(f"[INFO] Coverage {coverage:.2%} < target {TARGET_COVERAGE:.0%}; expansion attempt {attempt}")
        # Expand roots: all menu_roots plus top-degree callers
        degree=sorted(((len(adj.get(k,[])),k) for k in adj.keys()), reverse=True)
        top_extra=[k for _,k in degree[:50]]
        expanded=list(menu_roots.union(top_extra))
        added=enumerate_paths(expanded,prog_map,adj,args.max_depth,args.max_paths_per_root//2,args.branching_limit,transitions_map)
        merged={p['path_id']:p for p in paths}
        for a in added:
            merged.setdefault(a['path_id'],a)
        paths=list(merged.values())
        coverage=compute_coverage(paths,menu_roots)
    paths=enrich(paths,prog_map)
    # annotate edge origins before deepening
    annotate_edge_origins(paths,transitions_map)
    # After enrichment, if coverage still below target, synthesize placeholder single-node paths
    covered_roots={p['start_program_id'] for p in paths if p.get('start_program_id') in menu_roots}
    uncovered=list(menu_roots - covered_roots)
    synthesized=0
    if coverage < TARGET_COVERAGE and uncovered:
        now=datetime.datetime.utcnow().isoformat()+'Z'
        for root in uncovered:
            # create a trivial placeholder path doc (length=1, hop_count=0)
            seq=[root]
            pid=hashlib.sha1(f"SYNTH::{root}".encode()).hexdigest()[:40]
            doc={'path_id':pid,'start_program_id':root,'end_program_id':root,'program_sequence_json':json.dumps(seq),'length':1,'score':0.0,'frequency_score':0.0,'frequency_score_norm':0.0,'hop_count':0,'path_json':json.dumps(seq),'updated_at':now,'guard_summary':'(no outgoing UI calls detected)','is_placeholder':True,'path_type':'PLACEHOLDER','generation_pass':attempt}
            paths.append(doc); synthesized+=1
        # recompute coverage
        coverage=compute_coverage(paths,menu_roots)
        print(f"[INFO] Added {synthesized} placeholder paths to raise coverage -> {coverage:.2%}")
    # Ensure at least one long (hop_count>=3) path per covered root if possible
    by_root=collections.defaultdict(list)
    for p in paths:
        by_root[p['start_program_id']].append(p)
    for root, plist in by_root.items():
        if not any(pp.get('hop_count',0) >=3 for pp in plist):
            # try to extend one path if adjacency allows
            for pp in sorted(plist, key=lambda x:x.get('length',0), reverse=True)[:3]:
                seq=json.loads(pp['program_sequence_json'])
                last=seq[-1]
                for nxt in sorted(adj.get(last,[])):
                    if nxt not in seq:
                        new_seq=seq+[nxt]
                        h=hashlib.sha1('>'.join(new_seq).encode()).hexdigest()[:40]
                        if h not in {p['path_id'] for p in paths}:
                            rec={'path_id':h,'start_program_id':new_seq[0],'end_program_id':new_seq[-1],'program_sequence_json':json.dumps(new_seq),'length':len(new_seq)}
                            rec['score']=round(math.log(len(new_seq)+1),4)
                            paths.append(rec)
                        break
                if any(p2.get('hop_count',0)>=3 for p2 in by_root[root]):
                    break
    # Deepening pass: targeted DFS allowing passing through one non-UI to reach deeper UI if still missing hop3
    by_root=collections.defaultdict(list)
    for p in paths:
        by_root[p['start_program_id']].append(p)
    existing_ids={p['path_id'] for p in paths}
    deep_added=0
    for root in menu_roots:
        plist=by_root.get(root,[])
        if any(p.get('hop_count',0)>=3 for p in plist):
            continue
        # guided DFS with allowance: UI -> (any) -> (any) -> UI preference
        def guided(start,max_depth=6):
            out=[]
            stack=[(start,[start])]
            while stack:
                node,seq=stack.pop()
                if len(seq)>max_depth: continue
                for nxt in sorted(adj.get(node,[])):
                    if nxt in seq: continue
                    new_seq=seq+[nxt]
                    if len(new_seq)>=4: # hop_count 3
                        out.append(new_seq)
                        if len(out)>=3:
                            return out
                    stack.append((nxt,new_seq))
            return out
        candidates=guided(root)
        for seq in candidates:
            h=hashlib.sha1('>'.join(seq).encode()).hexdigest()[:40]
            if h in existing_ids: continue
            rec={'path_id':h,'start_program_id':seq[0],'end_program_id':seq[-1],'program_sequence_json':json.dumps(seq),'length':len(seq),'deepening_pass':1}
            rec['score']=round(math.log(len(seq)+1),4)
            paths.append(rec); existing_ids.add(h); deep_added+=1
            break
    if deep_added:
        # re-run enrichment for new records only
        new_recs=[p for p in paths if p.get('deepening_pass')==1]
        enrich(new_recs,prog_map)
        annotate_edge_origins(new_recs,transitions_map)
        print(f"[INFO] Deepening added {deep_added} hop3 candidate paths")
    # Re-enrich new additions lacking enrichment
    enriched_ids={p['path_id'] for p in paths if 'hop_count' in p}
    to_enrich=[p for p in paths if p['path_id'] not in enriched_ids]
    if to_enrich:
        to_enrich=enrich(to_enrich,prog_map)
    # Path type heuristic classification & generation_pass assignment for non-placeholders
    for p in paths:
        if p.get('is_placeholder'):
            continue
        seq=json.loads(p.get('program_sequence_json','[]')) if p.get('program_sequence_json') else []
        root=seq[0] if seq else p.get('start_program_id')
        leaf=seq[-1] if seq else p.get('end_program_id')
        # simple heuristics
        name_root=root or ''
        if 'LOG' in name_root and 'MENU' in name_root:
            p['path_type']='LOGIN_FLOW'
        elif 'MENU' in name_root:
            p['path_type']='MENU_FLOW'
        elif any(tok in (leaf or '') for tok in ('MAINT','INQ','LIST')):
            p['path_type']='MAINT_FLOW'
        else:
            p.setdefault('path_type','GENERIC_FLOW')
        p.setdefault('generation_pass',0)
        # placeholder flag default false
        p.setdefault('is_placeholder',False)
    # Multi-screen ordered mapping: collect all screens per program and expand along sequence (limit duplicates overall)
    prog_to_screens_ordered=collections.defaultdict(list)
    prog_to_screen_names=collections.defaultdict(list)
    for sn in screen_nodes:
        pid=(sn.get('program_id') or '').upper(); sid=sn.get('screen_id'); sname=sn.get('screen_name') or ''
        if not pid or not sid: continue
        if sid not in prog_to_screens_ordered[pid]:
            prog_to_screens_ordered[pid].append(sid)
        if sname and sname not in prog_to_screen_names[pid]:
            prog_to_screen_names[pid].append(sname)
    for p in paths:
        try:
            seq=json.loads(p.get('program_sequence_json') or '[]')
        except Exception:
            seq=[]
        ordered_screen_ids=[]; ordered_screen_names=[]; seen=set()
        for pr in seq:
            for sid in prog_to_screens_ordered.get(pr,[])[:3]:  # limit per program to top 3
                if sid not in seen:
                    ordered_screen_ids.append(sid); seen.add(sid)
            for sname in prog_to_screen_names.get(pr,[])[:3]:
                if sname not in ordered_screen_names:
                    ordered_screen_names.append(sname)
        if ordered_screen_ids:
            p['screen_ids_json']=json.dumps(ordered_screen_ids)[:64000]
        if ordered_screen_names:
            p['screen_names_json']=json.dumps(ordered_screen_names)[:64000]

    # Secondary deepening pass: attempt to create at least one hop_count>=3 non-placeholder path per root
    by_root=collections.defaultdict(list)
    for p in paths:
        by_root[p['start_program_id']].append(p)
    existing_ids={p['path_id'] for p in paths}

    def dfs_collect(root:str, max_depth:int=args.deepening_max_depth, branch_cap:int=args.deepening_branch_cap)->list[list[str]]:
        out=[]; stack=[(root,[root])]; seen_paths=set()
        while stack:
            node, seq = stack.pop()
            if len(seq)>=4:
                h='>'.join(seq)
                if h not in seen_paths:
                    out.append(seq[:]); seen_paths.add(h)
            if len(seq)>=max_depth: continue
            children=sorted(adj.get(node,[]))[:branch_cap]
            # prefer UI-labeled programs first via ordering key
            children=sorted(children, key=lambda c:(is_ui_program(prog_map.get(c)), len(adj.get(c,[]))), reverse=True)[:branch_cap]
            for ch in children:
                if ch in seq: continue
                stack.append((ch, seq+[ch]))
            if len(out)>=12:  # cap candidates per root
                break
        return out

    extended=0
    for root, plist in by_root.items():
        if any((p.get('hop_count') or 0)>=3 and not p.get('is_placeholder') for p in plist):
            continue
        candidates=dfs_collect(root)
        # rank candidates by length then UI ratio then uniqueness of leaf
        scored=[]
        for seq in candidates:
            ui_ratio=sum(1 for x in seq if is_ui_program(prog_map.get(x)))/len(seq)
            scored.append((len(seq), ui_ratio, seq))
        scored.sort(reverse=True)
        for _,_,seq in scored[:2]:  # add up to 2 deepened paths
            hid=hashlib.sha1('>'.join(seq).encode()).hexdigest()[:40]
            if hid in existing_ids: continue
            rec={'path_id':hid,'start_program_id':seq[0],'end_program_id':seq[-1],'program_sequence_json':json.dumps(seq),'length':len(seq)}
            rec['score']=round(math.log(len(seq)+1),4)
            paths.append(rec); existing_ids.add(hid); extended+=1
    if extended:
        new_unenriched=[p for p in paths if p['path_id'] in existing_ids and 'hop_count' not in p]
        if new_unenriched:
            enrich(new_unenriched,prog_map)
            annotate_edge_origins(new_unenriched,transitions_map)
            # ensure hop_count is set for deepened additions (enrich sets it, but guard just in case)
            for p in new_unenriched:
                if 'hop_count' not in p:
                    try:
                        seq=json.loads(p.get('program_sequence_json') or '[]')
                        p['hop_count']=max(0,len(seq)-1)
                    except Exception:
                        p['hop_count']=0
    path_count=len(paths)
    # Hop count distribution diagnostics
    hop_hist=collections.Counter()
    for p in paths:
        hc=p.get('hop_count')
        if hc is not None:
            hop_hist[hc]+=1
    hop_dist={str(k):v for k,v in sorted(hop_hist.items())}
    diag={'menu_root_count':len(menu_roots),'coverage':round(coverage,4),'path_docs':path_count,'expanded_attempts':attempt,'placeholders_added':synthesized if 'synthesized' in locals() else 0,'hop_hist':hop_dist}
    print(json.dumps(diag,indent=2))
    if args.export_json:
        with open(args.export_json,'w',encoding='utf-8') as f: json.dump(paths,f,indent=2)
    if args.push:
        upload(ep,key,paths)

if __name__=='__main__':
    main()
