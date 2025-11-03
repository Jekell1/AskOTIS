"""Aggregate per-program call metadata and upload to new_cobol_program_meta.

Derives:
  - outgoing_count: number of call edges originating in program
  - incoming_count: number of call edges targeting program
  - unique_callees / unique_callers
  - has_cycles: simple cycle detection via DFS on call graph
  - call_depth_score: maximum depth from this program following call edges (bounded)
  - sample_call_lines: small sample of call line numbers

Usage:
  python build_program_meta.py [--endpoint ... --key ...] [--overwrite-existing]

Environment fallback: SEARCH_ENDPOINT / SEARCH_KEY or values from local.settings.json.

Assumptions:
  - Calls index already populated (new_cobol_calls)
  - Programs inferred from caller_program and callee_program fields
"""
from __future__ import annotations
import os, json, argparse, requests, sys, collections, math, time
from typing import Dict, Any, List, Set, Tuple

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
CALLS_INDEX = 'new_cobol_calls'
META_INDEX = 'new_cobol_program_meta'


def load_local_settings():
    try:
        vals = json.load(open('local.settings.json','r')).get('Values',{})
        for k in ['SEARCH_ENDPOINT','SEARCH_KEY','AZURE_SEARCH_ENDPOINT','AZURE_SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve_endpoint_key(args):
    ep = args.endpoint or os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = args.key or os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        print('Missing search endpoint/key (provide --endpoint/--key or set env).', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def fetch_all_calls(ep: str, key: str, batch: int = 1000, top_limit: int | None = None) -> List[Dict[str,Any]]:
    # Use search with * and pagination on $skip to retrieve docs.
    # NOTE: For large corpora consider using indexer/data export; here we assume manageable size.
    out: List[Dict[str,Any]] = []
    skip = 0
    while True:
        remaining = None if top_limit is None else max(0, top_limit - len(out))
        if remaining is not None and remaining == 0:
            break
        top = batch if remaining is None else min(batch, remaining)
        url = f"{ep}/indexes/{CALLS_INDEX}/docs/search.post.search?api-version={API_VERSION}"
        body = {"search": "*", "top": top, "skip": skip, "select": "caller_program,callee_program,line"}
        r = requests.post(url, headers={"api-key": key, "Content-Type":"application/json"}, json=body)
        if r.status_code != 200:
            raise SystemExit(f"Call fetch failed {r.status_code}: {r.text[:200]}")
        js = r.json()
        batch_docs = js.get('value', [])
        out.extend(batch_docs)
        if len(batch_docs) < top:
            break
        skip += top
    return out


def build_graph(calls: List[Dict[str,Any]]):
    outgoing: Dict[str, Set[str]] = collections.defaultdict(set)
    incoming: Dict[str, Set[str]] = collections.defaultdict(set)
    sample_lines: Dict[str, List[int]] = collections.defaultdict(list)
    for c in calls:
        caller = (c.get('caller_program') or '').upper()
        callee = (c.get('callee_program') or '').upper()
        if not caller or not callee:
            continue
        outgoing[caller].add(callee)
        incoming[callee].add(caller)
        if len(sample_lines[caller]) < 10 and isinstance(c.get('line'), int):
            sample_lines[caller].append(c['line'])
    programs = set(outgoing.keys()) | set(incoming.keys())
    return programs, outgoing, incoming, sample_lines


def detect_cycles(programs: Set[str], outgoing: Dict[str, Set[str]], limit: int = 10000) -> Set[str]:
    cycles: Set[str] = set()
    visited: Set[str] = set()
    stack: Set[str] = set()

    def dfs(node: str, depth: int = 0):
        if depth > limit:
            return
        visited.add(node)
        stack.add(node)
        for nxt in outgoing.get(node, []):
            if nxt not in visited:
                dfs(nxt, depth+1)
            elif nxt in stack:
                cycles.add(node)
                cycles.add(nxt)
        stack.discard(node)

    for p in programs:
        if p not in visited:
            dfs(p)
    return cycles


def compute_depth(programs: Set[str], outgoing: Dict[str, Set[str]], max_depth_cap: int = 50) -> Dict[str,int]:
    memo: Dict[str,int] = {}
    visiting: Set[str] = set()

    def depth(node: str, d: int = 0) -> int:
        if node in memo:
            return memo[node]
        if d >= max_depth_cap:
            memo[node] = max_depth_cap
            return max_depth_cap
        if node in visiting:
            memo[node] = 0
            return 0
        visiting.add(node)
        children = outgoing.get(node, [])
        if not children:
            memo[node] = 0
        else:
            memo[node] = 1 + max(depth(ch, d+1) for ch in children)
        visiting.discard(node)
        return memo[node]

    for p in programs:
        depth(p)
    return memo

def bounded_reach(start: str, graph: Dict[str, Set[str]], limit_nodes: int = 5000, max_depth: int = 50) -> int:
    """Return number of distinct reachable nodes (excluding start)."""
    seen: Set[str] = set()
    frontier: collections.deque[tuple[str,int]] = collections.deque([(start,0)])
    while frontier and len(seen) < limit_nodes:
        node, d = frontier.popleft()
        for nxt in graph.get(node, []):
            if nxt not in seen and nxt != start and d+1 <= max_depth:
                seen.add(nxt)
                frontier.append((nxt, d+1))
    return len(seen)

def transpose(graph: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
    rev: Dict[str, Set[str]] = collections.defaultdict(set)
    for a, outs in graph.items():
        for b in outs:
            rev[b].add(a)
    return rev

def compute_centrality(programs: Set[str], out_graph: Dict[str, Set[str]], in_graph: Dict[str, Set[str]]) -> Dict[str,float]:
    cent: Dict[str,float] = {}
    # Simple normalized degree-based centrality combining in/out
    max_out = max((len(out_graph.get(p,())) for p in programs), default=1)
    max_in = max((len(in_graph.get(p,())) for p in programs), default=1)
    for p in programs:
        out_deg = len(out_graph.get(p,()))/max_out if max_out else 0.0
        in_deg = len(in_graph.get(p,()))/max_in if max_in else 0.0
        cent[p] = round((out_deg + in_deg)/2.0, 4)
    return cent


def upload_meta(ep: str, key: str, docs: List[Dict[str,Any]]):
    url = f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API_VERSION}"
    headers = {"api-key": key, "Content-Type":"application/json"}
    for i in range(0, len(docs), 500):
        batch = {"value": [{"@search.action":"mergeOrUpload", **d} for d in docs[i:i+500]]}
        r = requests.post(url, headers=headers, json=batch)
        if r.status_code not in (200,201):
            raise SystemExit(f"Upload meta failed {r.status_code}: {r.text[:200]}")


def load_coverage_map(path: str) -> Dict[str, Dict[str, Any]]:
    if not path:
        return {}
    try:
        data = json.load(open(path,'r',encoding='utf-8'))
        # Expect shape { aggregate: {...}, programs: [ {...fields...} ] }
        progs = data.get('programs') or []
        out = {}
        for p in progs:
            pid = p.get('program_id')
            if pid:
                # augment largest_gap_length for convenience
                largest_gap_len = 0
                try:
                    if p.get('largest_gaps'):
                        largest_gap_len = p['largest_gaps'][0]['length']
                except Exception:
                    largest_gap_len = 0
                p['largest_gap_length'] = largest_gap_len
                out[pid.upper()] = p
        return out
    except Exception as e:
        print(f"[WARN] Failed to load coverage JSON '{path}': {e}", file=sys.stderr)
        return {}

def classify_program(cov: Dict[str, Any] | None) -> str:
    if not cov:
        return 'UNKNOWN'
    para_count = cov.get('paragraph_count',0)
    coverage_pct = cov.get('coverage_pct',0.0)
    total_lines = cov.get('total_lines',0)
    if total_lines > 0 and para_count == 0:
        return 'NO_PARAS'
    if coverage_pct < 10.0:
        return 'LOW_COVERAGE'
    # Additional future heuristics could detect DATA_ONLY via keyword density etc.
    return 'NORMAL'

def main():
    ap = argparse.ArgumentParser(description='Build program meta from call edges (optionally merging coverage metrics).')
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    ap.add_argument('--top', type=int, help='Limit total calls fetched (debug)')
    ap.add_argument('--coverage-json', help='Path to coverage_full.json produced by program_coverage_audit.py')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    print('Fetching calls ...')
    calls = fetch_all_calls(ep, key, top_limit=args.top)
    print(f'Fetched {len(calls)} call edges')

    programs, outgoing, incoming, sample_lines = build_graph(calls)
    cycles = detect_cycles(programs, outgoing)
    depth_map = compute_depth(programs, outgoing)

    coverage_map = load_coverage_map(args.coverage_json) if args.coverage_json else {}
    rev_graph = transpose(outgoing)
    centrality = compute_centrality(programs, outgoing, incoming)
    cov_ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()) if coverage_map else None
    docs: List[Dict[str,Any]] = []
    for prog in sorted(programs):
        out_set = outgoing.get(prog, set())
        in_set = incoming.get(prog, set())
        # --- New enrichment heuristics ---
        # program_role: classify based on name / structural signals
        name_up = prog.upper()
        role = None
        if any(tok in name_up for tok in ('SCRN','SCREEN','MAP','PANEL','PANT','UI','TERM','MENU')):
            role = 'UI'
        elif any(tok in name_up for tok in ('DB2','SQL','DB','DATA')):
            role = 'DATA_ACCESS'
        elif name_up.endswith('API') or name_up.startswith('API'):
            role = 'API'
        elif name_up.startswith('BATCH') or name_up.endswith('BATCH'):
            role = 'BATCH'
        elif name_up.startswith('UTIL') or name_up.endswith('UTIL'):
            role = 'UTILITY'
        else:
            # Fallback to shallow graph signals
            if len(out_set) == 0 and len(in_set) > 5:
                role = 'LEAF'
            elif len(out_set) > 10 and len(in_set) <= 1:
                role = 'DISPATCH'
            else:
                role = 'UNKNOWN'

        # ui_flag: separate boolean signal (true if role UI or name indicates)
        ui_flag = role == 'UI'

        # flow_graph_json: miniature adjacency preview (limit counts)
        flow_preview = {
            'outgoing': sorted(list(out_set))[:25],
            'incoming': sorted(list(in_set))[:25],
            'out_degree': len(out_set),
            'in_degree': len(in_set)
        }
        flow_graph_json = json.dumps(flow_preview, separators=(',',':'))

        # program_summary: concise textual summary (deterministic, no model call)
        summary_parts = [
            f"Program {prog}",
            f"calls {len(out_set)} others" if out_set else "no outgoing calls",
            f"called by {len(in_set)}" if in_set else "not called by others",
            f"depth={depth_map.get(prog,0)}",
        ]
        if role and role != 'UNKNOWN':
            summary_parts.append(f"role={role}")
        if prog in cycles:
            summary_parts.append('in_cycle')
        program_summary = '; '.join(summary_parts)

        # placeholder for input_screen_paths_json (future path tracing)
        input_screen_paths_json = json.dumps([])
        # Reachability (bounded)
        reach_out = bounded_reach(prog, outgoing, limit_nodes=8000, max_depth=40)
        reach_in = bounded_reach(prog, rev_graph, limit_nodes=8000, max_depth=40)
        # External callees (those without meta record placeholder)
        external_callees = [c for c in out_set if c not in programs]
        dynamic_calls = 0  # Placeholder: if calls dataset carried is_dynamic per edge, could sum here
        total_calls = len(out_set)
        dynamic_call_ratio = (dynamic_calls/total_calls) if total_calls else 0.0
        # Centrality & risk heuristic
        cent_score = centrality.get(prog, 0.0)
        cov = coverage_map.get(prog) if coverage_map else None
        coverage_pct = cov.get('coverage_pct') if cov else None
        risk_flag = False
        try:
            if coverage_pct is not None and coverage_pct < 25.0 and cent_score >= 0.4:
                risk_flag = True
        except Exception:
            pass
        # UI path participant heuristic (any UI program reachable or it is UI)
        ui_path_participant = ui_flag
        if not ui_path_participant and out_set:
            if any(any(tok in t for tok in ('SCRN','SCREEN','MAP','PANEL')) for t in out_set):
                ui_path_participant = True

        base_doc = {
            'program_id': prog,
            'outgoing_count': len(out_set),
            'incoming_count': len(in_set),
            'unique_callees': sorted(out_set)[:1000],
            'unique_callers': sorted(in_set)[:1000],
            'has_cycles': prog in cycles,
            'call_depth_score': depth_map.get(prog, 0),
            'sample_call_lines': ','.join(str(x) for x in sorted(sample_lines.get(prog, [])[:10])),
            'program_summary': program_summary,
            'program_role': role,
            'flow_graph_json': flow_graph_json,
            'ui_flag': ui_flag,
            'input_screen_paths_json': input_screen_paths_json,
            'reach_out_size': reach_out,
            'reach_in_size': reach_in,
            'centrality_score': cent_score,
            'risk_flag': risk_flag,
            'dynamic_call_ratio': dynamic_call_ratio,
            'external_callee_count': len(external_callees),
            'external_callees': external_callees[:200],
            'ui_path_participant': ui_path_participant,
            'ingested_at': None
        }
        cov = coverage_map.get(prog) if coverage_map else None
        if cov:
            base_doc.update({
                'total_lines': cov.get('total_lines'),
                'covered_lines': cov.get('covered_lines'),
                'coverage_pct': cov.get('coverage_pct'),
                'paragraph_count': cov.get('paragraph_count'),
                'avg_paragraph_length': cov.get('avg_paragraph_length'),
                'median_paragraph_length': cov.get('median_paragraph_length'),
                'max_paragraph_length': cov.get('max_paragraph_length'),
                'gap_count': cov.get('gap_count'),
                'largest_gap_length': cov.get('largest_gap_length'),
                'classification': classify_program(cov),
                'coverage_ingested_at': cov_ts
            })
        docs.append(base_doc)
    print(f'Uploading {len(docs)} program meta docs ...')
    upload_meta(ep, key, docs)
    print('Program meta aggregation complete.')

if __name__ == '__main__':
    main()
