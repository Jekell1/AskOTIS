"""Enumerate heuristic UI navigation paths and ingest into new_cobol_ui_paths.

Enhancements:
  - Added --min-length to filter out trivial single-node paths (default 1 preserves old behavior).
  - Added --root-mode (ui|degree|mixed|all) to control root candidate selection strategy.
  - Added --root-list (comma separated) to explicitly specify root programs (overrides other selection).
  - Fixed adjacency extraction to use 'caller_program' / 'callee_program' from calls index (previously unused field names caused only single-node paths).
  - Added diagnostic summary printing root candidate stats and expansion coverage.
  - Added option --max-roots-after-union to cap root set post-union for mixed/all modes.

Existing scoring preserved.
"""
from __future__ import annotations
import os, sys, json, argparse, hashlib, math, datetime, requests, collections, re
from typing import List, Dict, Any, Set, Tuple

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
PROGRAM_META_INDEX = 'new_cobol_program_meta'
FLOW_EDGES_INDEX = 'new_cobol_flow_edges'
UI_PATHS_INDEX = 'new_cobol_ui_paths'
SCREEN_INDEX = 'new_cobol_screen_nodes'
CALL_INDEX_CANDIDATE = 'new_cobol_calls'

# ------------- environment helpers ------------

def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('[FATAL] Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key

# ------------- search utilities ------------

def search_all(ep: str, key: str, index: str, select: str|None=None) -> List[Dict[str,Any]]:
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    results=[]
    payload={'search':'*','queryType':'simple','top':1000}
    if select:
        payload['select']=select
    skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url, headers=headers, json=payload)
        if r.status_code!=200:
            print(f"[ERROR] search {index} {r.status_code}: {r.text[:200]}")
            break
        data=r.json()
        batch=data.get('value',[])
        results.extend(batch)
        if len(batch)<payload['top']:
            break
        skip+=payload['top']
    return results

def get_index_exists(ep: str, key: str, name: str) -> bool:
    r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key})
    return r.status_code == 200

# ------------- calls adjacency ------------

def fetch_calls(ep: str, key: str) -> List[Dict[str,Any]]:
    if not get_index_exists(ep,key,CALL_INDEX_CANDIDATE):
        return []
    url=f"{ep}/indexes/{CALL_INDEX_CANDIDATE}/docs/search?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    results=[]
    payload={'search':'*','top':1000,'queryType':'simple','select':'caller_program,callee_program'}
    skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url, headers=headers, json=payload)
        if r.status_code!=200:
            break
        data=r.json()
        batch=data.get('value',[])
        results.extend(batch)
        if len(batch)<payload['top']:
            break
        skip+=payload['top']
    return results

def build_outgoing_from_calls(calls: List[Dict[str,Any]]) -> Dict[str,Set[str]]:
    out: Dict[str,Set[str]] = collections.defaultdict(set)
    for c in calls:
        caller = (c.get('caller_program') or c.get('caller_program_id') or c.get('caller') or '').upper()
        callee = (c.get('callee_program') or c.get('callee_program_id') or c.get('callee') or '').upper()
        if caller and callee and caller != callee:
            out[caller].add(callee)
    return out

# ------------- heuristics ------------

_UI_NAME_TOKENS = ['MENU','SCR','SCRN','MAP','PANEL','PAGE','SCREEN','INQ','MAINT','LIST','FORM']

def is_ui_program(p: Dict[str,Any]) -> bool:
    if not p:
        return False
    if p.get('ui_flag') or p.get('ui_path_participant'):
        return True
    role = (p.get('program_role') or '').upper()
    if role in {'UI','SCREEN','PRESENTATION','DISPATCH'}:
        return True
    name = (p.get('program_id') or '').upper()
    for tok in _UI_NAME_TOKENS:
        if tok in name:
            return True
    summary = (p.get('program_summary') or '').upper()
    for tok in _UI_NAME_TOKENS:
        if tok in summary:
            return True
    return False

# ------------- path enumeration (enhanced unified) ------------

def enumerate_paths(programs: List[Dict[str,Any]], outgoing: Dict[str,Set[str]], max_depth:int, branching_limit:int, max_paths:int, min_length:int, root_ids: List[str], non_ui_hop_budget:int, call_counts:Dict[tuple[str,str],int], dedupe_leaf_global:bool, keep_all: bool=False) -> List[Dict[str,Any]]:
    """Enumerate candidate UI navigation paths with frequency & UI bias.

    Parameters:
      programs: program meta documents
      outgoing: adjacency map caller -> set(callee)
      max_depth: maximum path length (#nodes)
      branching_limit: cap on children explored per expansion
      max_paths: cap paths per root (ignored if keep_all True, but still serves as safety)
      min_length: minimum length to emit
      root_ids: starting program ids
      non_ui_hop_budget: allowed non-UI transitions before pruning
      call_counts: edge frequency map
      dedupe_leaf_global: if True keep only highest scoring path per leaf across all roots
      keep_all: if True retain all unique path_id sequences (no best-by-leaf pruning)
    """
    prog_map = {p['program_id']: p for p in programs if p.get('program_id')}
    out_degree_cache = {pid: len(children) for pid, children in outgoing.items()}

    def path_hash(seq: List[str]) -> str:
        return hashlib.sha256('>'.join(seq).encode()).hexdigest()[:40]

    best_by_leaf_per_root: Dict[tuple[str,str], Dict[str,Any]] = {}
    best_by_leaf_global: Dict[str, Dict[str,Any]] = {}
    all_paths: Dict[str, Dict[str,Any]] = {}

    for root_id in root_ids:
        stack: List[tuple[str,List[str],bool,int,int,List[int]]] = []  # (node, seq, loop_flag, depth, rem_budget, edge_freqs)
        stack.append((root_id, [root_id], False, 1, non_ui_hop_budget, []))
        emitted_for_root = 0
        while stack and (emitted_for_root < max_paths or keep_all):
            current, seq, loop_flag, depth, rem_budget, edge_freqs = stack.pop()
            length = len(seq)
            if length >= min_length:
                ui_count = sum(1 for pid in seq if is_ui_program(prog_map.get(pid,{})))
                depth_reach = length / max_depth
                if edge_freqs:
                    avg_freq = sum(edge_freqs)/len(edge_freqs)
                    min_freq = min(edge_freqs)
                else:
                    avg_freq = 1.0
                    min_freq = 1
                score = ((ui_count/length) if length>0 else 0) * math.log(length+1) + 0.1*depth_reach + 0.05*math.log(avg_freq+1) + 0.03*math.log(min_freq+1)
                rec = {
                    'path_id': path_hash(seq),
                    'root_program_id': root_id,
                    'leaf_program_id': current,
                    'program_sequence_json': json.dumps(seq),
                    'screen_sequence_json': json.dumps(seq),
                    'length': length,
                    'ui_program_count': ui_count,
                    'branching_events_json': json.dumps([]),
                    'guards_json': json.dumps([]),
                    'edge_freqs_json': json.dumps(edge_freqs),
                    'avg_edge_freq': float(avg_freq),
                    'min_edge_freq': int(min_freq),
                    'loop_collapsed': loop_flag,
                    'score': round(score,4)
                }
                if keep_all:
                    all_paths[rec['path_id']] = rec
                else:
                    key_leaf = (root_id, current)
                    prev = best_by_leaf_per_root.get(key_leaf)
                    replace = (not prev) or rec['score'] > prev['score']
                    if dedupe_leaf_global:
                        prev_global = best_by_leaf_global.get(current)
                        if (not prev_global) or rec['score'] > prev_global['score']:
                            best_by_leaf_global[current] = rec
                            replace = True
                        else:
                            replace = False
                    if replace:
                        best_by_leaf_per_root[key_leaf] = rec
                emitted_for_root += 1
            if length >= max_depth:
                continue
            children = list(outgoing.get(current, []))
            if not children:
                continue
            ui_children = [c for c in children if is_ui_program(prog_map.get(c,{}))]
            non_ui_children = [c for c in children if c not in ui_children]
            candidate_children: List[str] = []
            candidate_children.extend(ui_children)
            if rem_budget > 0 and non_ui_children:
                remaining_slots = max(0, branching_limit - len(candidate_children))
                non_ui_allow = min(remaining_slots, max(1, rem_budget))
                candidate_children.extend(non_ui_children[:non_ui_allow])
            if not candidate_children and rem_budget > 0:
                candidate_children = non_ui_children[:branching_limit]
            def child_key(c: str):
                freq = call_counts.get((current, c), 0)
                return (is_ui_program(prog_map.get(c,{})), freq, out_degree_cache.get(c,0))
            candidate_children = sorted(set(candidate_children), key=child_key, reverse=True)[:branching_limit]
            for nxt in candidate_children:
                new_seq = seq + [nxt]
                new_edge_freqs = edge_freqs + [call_counts.get((current,nxt),1)]
                new_budget = rem_budget - (0 if is_ui_program(prog_map.get(nxt,{})) else 1)
                if new_budget < 0:
                    continue
                stack.append((nxt, new_seq, (nxt in seq), depth+1, new_budget, new_edge_freqs))
    if keep_all:
        return list(all_paths.values())
    if dedupe_leaf_global:
        # global best per leaf
        global_best = {}
        for rec in best_by_leaf_global.values():
            lp = rec['leaf_program_id']
            if lp not in global_best or rec['score'] > global_best[lp]['score']:
                global_best[lp] = rec
        return list(global_best.values())
    return list(best_by_leaf_per_root.values())

# --------- enrichment passes ---------

def fetch_screen_nodes(ep:str,key:str) -> Dict[str,list[str]]:
    """Map program_id -> list of screen_ids."""
    url=f"{ep}/indexes/{SCREEN_INDEX}/docs/search?api-version={API_VERSION}"
    headers={'api-key':key,'Content-Type':'application/json'}
    payload={'search':'*','top':1000,'select':'program_id,screen_id'}
    prog_to_screens=collections.defaultdict(list)
    skip=0
    while True:
        payload['skip']=skip
        r=requests.post(url,headers=headers,json=payload)
        if r.status_code!=200:
            break
        batch=r.json().get('value',[])
        for b in batch:
            pid=(b.get('program_id') or '').upper()
            sid=b.get('screen_id')
            if pid and sid:
                prog_to_screens[pid].append(sid)
        if len(batch)<payload['top']:
            break
        skip+=payload['top']
    return prog_to_screens

GUARD_PATTERNS=[re.compile(r'IF\s+[^.]{0,120}?DISPLAY',re.IGNORECASE), re.compile(r'EVALUATE\s',re.IGNORECASE)]

def guard_scan(program_id:str, prog_meta:Dict[str,Any]) -> list[str]:
    # placeholder: inspect summary fields for conditional UI hints (lightweight)
    out=[]
    summ=(prog_meta.get('program_summary') or '')
    for pat in GUARD_PATTERNS:
        if pat.search(summ):
            out.append(pat.pattern)
    return out[:5]

def enrich_paths(paths:List[Dict[str,Any]], prog_map:Dict[str,Any], prog_to_screens:Dict[str,list[str]]):
    # Edge frequency: count edges across all paths first
    edge_counter=collections.Counter()
    for p in paths:
        seq=json.loads(p['program_sequence_json'])
        for a,b in zip(seq,seq[1:]):
            edge_counter[(a,b)]+=1
    # annotate each path
    for p in paths:
        seq=json.loads(p['program_sequence_json'])
        freqs=[edge_counter[(a,b)] for a,b in zip(seq,seq[1:])] if len(seq)>1 else []
        if freqs:
            p['edge_freqs_json']=json.dumps(freqs)
            p['avg_edge_freq']=sum(freqs)/len(freqs)
            p['min_edge_freq']=min(freqs)
        # screen ids (union of screens for each program in sequence)
        screen_ids=[]
        for pid in seq:
            screen_ids.extend(prog_to_screens.get(pid,[])[:3])  # cap per program
        p['screen_ids_json']=json.dumps(sorted(set(screen_ids)))
        # guard heuristics: union of guard markers encountered
        guards=[]
        for pid in seq:
            guards.extend(guard_scan(pid, prog_map.get(pid,{})))
        if guards:
            p['guards_json']=json.dumps(sorted(set(guards))[:8])
    return paths

# ------------- ingestion ------------

def get_index_field_names(ep: str, key: str, name: str) -> Set[str]:
    try:
        r = requests.get(f"{ep}/indexes/{name}?api-version={API_VERSION}", headers={'api-key': key})
        if r.status_code==200:
            data = r.json()
            return {f['name'] for f in data.get('fields',[])}
    except Exception as e:
        print(f"[WARN] Unable to fetch index schema: {e}")
    return set()

def upload_docs(ep: str, key: str, docs: List[Dict[str,Any]]):
    if not docs:
        print('No paths to upload.')
        return
    allowed = get_index_field_names(ep,key,UI_PATHS_INDEX)
    # Always preserve keys if schema unknown (empty allowed means skip filtering)
    if allowed:
        filtered=[]
        for d in docs:
            filtered.append({k:v for k,v in d.items() if k in allowed})
        docs = filtered
    url=f"{ep}/indexes/{UI_PATHS_INDEX}/docs/index?api-version={API_VERSION}"
    headers={'api-key': key,'Content-Type':'application/json'}
    ts = datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    actions=[]
    for d in docs:
        d['generated_at']=ts
        actions.append({'@search.action':'mergeOrUpload', **d})
    for i in range(0,len(actions),500):
        r=requests.post(url, headers=headers, json={'value': actions[i:i+500]})
        if r.status_code!=200:
            print(f"[ERROR] Upload chunk {i} status {r.status_code}: {r.text[:300]}")
            break
    print(f"Uploaded {len(docs)} UI path docs (after field filtering)")

# ------------- root selection ------------

def select_roots(programs: List[Dict[str,Any]], mode: str, limit: int, explicit: List[str]|None, outgoing: Dict[str,Set[str]], union_cap: int) -> List[str]:
    prog_map = {p['program_id']: p for p in programs if p.get('program_id')}
    if explicit:
        return [pid for pid in explicit if pid in prog_map]
    def sort_key(p):
        return (p.get('reach_out_size') or 0, p.get('centrality_score') or 0.0)
    if mode == 'ui':
        roots = [p for p in programs if (p.get('program_role') in ('DISPATCH','UI') or p.get('ui_flag') or p.get('ui_path_participant'))]
        roots.sort(key=sort_key, reverse=True)
        return [p['program_id'] for p in roots[:limit]]
    if mode == 'degree':
        roots = sorted(programs, key=lambda p: (p.get('outgoing_count') or 0, p.get('incoming_count') or 0), reverse=True)
        return [p['program_id'] for p in roots[:limit]]
    if mode == 'mixed':
        ui_set = {p['program_id'] for p in programs if (p.get('program_role') in ('DISPATCH','UI') or p.get('ui_flag') or p.get('ui_path_participant'))}
        degree_sorted = [p['program_id'] for p in sorted(programs, key=lambda p: (p.get('outgoing_count') or 0), reverse=True)[:limit]]
        cent_sorted = [p['program_id'] for p in sorted(programs, key=lambda p: (p.get('centrality_score') or 0), reverse=True)[:limit]]
        union = list(dict.fromkeys(list(ui_set) + degree_sorted + cent_sorted))  # preserve order of insertion
        # apply cap
        return union[: min(union_cap, len(union))]
    if mode == 'all':
        # broad ranking by reach_out_size then outgoing_count
        ranked = sorted(programs, key=lambda p: (p.get('reach_out_size') or 0, p.get('outgoing_count') or 0), reverse=True)
        return [p['program_id'] for p in ranked[:limit]]
    # default fallback
    return select_roots(programs, 'ui', limit, None, outgoing, union_cap)

# ------------- main ------------

def compute_call_counts(calls: List[Dict[str,Any]]) -> Dict[tuple[str,str], int]:
    counts: Dict[tuple[str,str], int] = {}
    for c in calls or []:
        src = (c.get('caller_program') or c.get('caller_program_id') or c.get('caller') or '').upper()
        dst = (c.get('callee_program') or c.get('callee_program_id') or c.get('callee') or '').upper()
        if not src or not dst:
            continue
        key = (src,dst)
        counts[key] = counts.get(key,0)+1
    return counts

def main():
    ap = argparse.ArgumentParser(description='Build heuristic UI navigation paths (enhanced unified).')
    ap.add_argument('--max-depth', type=int, default=10)
    ap.add_argument('--branching-limit', type=int, default=12)
    ap.add_argument('--max-paths-per-root', type=int, default=200)
    ap.add_argument('--limit-roots', type=int, default=10)
    ap.add_argument('--max-roots-after-union', type=int, default=40)
    ap.add_argument('--root-mode', choices=['ui','degree','mixed','all'], default='ui')
    ap.add_argument('--root-list', help='Comma separated explicit root program IDs')
    ap.add_argument('--min-length', type=int, default=1)
    ap.add_argument('--non-ui-hop-budget', type=int, default=1)
    ap.add_argument('--dedupe-leaf-global', action='store_true', help='De-duplicate paths globally by leaf program (keep highest score overall).')
    ap.add_argument('--keep-all', action='store_true', help='Retain all enumerated candidate paths (unique path_id) rather than best-by-leaf.')
    ap.add_argument('--push', action='store_true')
    ap.add_argument('--dry-run', action='store_true')
    ap.add_argument('--export-json', help='If set, write all generated path docs (post-dedupe) to this JSON file.')
    args = ap.parse_args()

    load_local_settings()
    ep,key=resolve_endpoint_key()
    programs = search_all(ep,key,PROGRAM_META_INDEX)
    if not programs:
        print('[FATAL] No program meta docs found.')
        return
    calls = fetch_calls(ep,key)
    outgoing = build_outgoing_from_calls(calls) if calls else {}
    call_counts = compute_call_counts(calls)
    explicit_roots = [s.strip().upper() for s in args.root_list.split(',')] if args.root_list else None
    root_ids = select_roots(programs, args.root_mode, args.limit_roots, explicit_roots, outgoing, args.max_roots_after_union)
    if not root_ids:
        print('[WARN] No roots selected; exiting.')
        return
    filtered_programs = programs
    paths = enumerate_paths(filtered_programs, outgoing, args.max_depth, args.branching_limit, args.max_paths_per_root, args.min_length, root_ids, args.non_ui_hop_budget, call_counts, args.dedupe_leaf_global, args.keep_all)
    # enrichment
    prog_map = {p['program_id']: p for p in programs if p.get('program_id')}
    screen_map = fetch_screen_nodes(ep,key)
    paths = enrich_paths(paths, prog_map, screen_map)
    now_ts = datetime.datetime.utcnow().replace(microsecond=0).isoformat()+'Z'
    for p in paths:
        try:
            seq = json.loads(p.get('program_sequence_json','[]'))
        except Exception:
            seq = []
        if seq:
            p['start_program_id']=seq[0]
            p['end_program_id']=seq[-1]
        try:
            screens = json.loads(p.get('screen_ids_json','[]')) if p.get('screen_ids_json') else []
        except Exception:
            screens = []
        try:
            guards = json.loads(p.get('guards_json','[]')) if p.get('guards_json') else []
        except Exception:
            guards = []
        meta_obj = {
            'program_sequence': seq,
            'screen_ids': screens,
            'guards': guards,
            'length': p.get('length'),
            'ui_program_count': p.get('ui_program_count')
        }
        p['path_json'] = json.dumps(meta_obj)[:64000]
        if guards:
            p['guard_summary'] = '; '.join(guards)[:400]
        avgf = p.get('avg_edge_freq') or 0.0
        minf = p.get('min_edge_freq') or 0
        length = p.get('length') or 1
        freq_score = (math.log(avgf+1) * 0.7 + math.log(minf+1) * 0.3) * math.log(length+1)
        p['frequency_score'] = round(freq_score,4)
        p['updated_at'] = now_ts
    root_stats = []
    for rid in root_ids[:50]:
        root_stats.append({
            'program_id': rid,
            'out_deg': len(outgoing.get(rid, [])),
            'role': prog_map.get(rid,{}).get('program_role'),
            'ui_eval': is_ui_program(prog_map.get(rid,{})),
        })
    diag = {
        'root_count': len(root_ids),
        'paths_generated': len(paths),
        'min_length': args.min_length,
        'root_mode': args.root_mode,
        'non_ui_hop_budget': args.non_ui_hop_budget,
        'dedupe_leaf_global': args.dedupe_leaf_global,
        'keep_all': args.keep_all,
        'root_stats_sample': root_stats[:10]
    }
    print(json.dumps(diag, indent=2))
    if args.export_json:
        try:
            with open(args.export_json,'w',encoding='utf-8') as f:
                json.dump(paths, f, ensure_ascii=False, indent=2)
            print(f"Exported {len(paths)} paths to {args.export_json}")
        except Exception as e:
            print(f"[WARN] Failed to export JSON: {e}")
    if args.dry_run:
        for p in paths[: min(5,len(paths))]:
            print(json.dumps(p, indent=2)[:800])
        return
    if args.push:
        upload_docs(ep,key,paths)
        print(f"Uploaded {len(paths)} UI path docs")

if __name__ == '__main__':
    main()
