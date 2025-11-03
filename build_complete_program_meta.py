"""Build complete program meta including ALL programs from filesystem.

Unlike build_program_meta.py which only includes programs with call relationships,
this script includes ALL programs found in the cobol_src directory tree.

Strategy:
  1. Get all program IDs from filesystem (CBL/CPY files)
  2. Get call relationship data for programs that have calls
  3. Create program meta entries for ALL programs:
     - Programs with calls: full call metadata (outgoing_count, incoming_count, etc.)
     - Programs without calls: basic metadata (isolated program markers)
  4. Generate program_summary text for embedding

Usage:
  python build_complete_program_meta.py [--dry-run] [--batch 500]
"""
from __future__ import annotations
import os, json, argparse, requests, sys, collections, math, time, glob
from typing import Dict, Any, List, Set, Tuple
from pathlib import Path

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
CALLS_INDEX = 'new_cobol_calls'
META_INDEX = 'new_cobol_program_meta'
COBOL_SRC_DIR = 'cobol_src'


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


def get_all_program_ids_from_filesystem() -> Set[str]:
    """Get all program IDs from CBL/CPY files in cobol_src directory."""
    program_ids = set()
    
    # Walk through all subdirectories in cobol_src
    for root, dirs, files in os.walk(COBOL_SRC_DIR):
        for file in files:
            if file.upper().endswith(('.CBL', '.CPY')):
                # Extract program ID from filename (without extension)
                program_id = os.path.splitext(file)[0].upper()
                program_ids.add(program_id)
    
    return program_ids


def fetch_all_calls(ep: str, key: str, batch: int = 1000, top_limit: int | None = None) -> List[Dict[str,Any]]:
    """Fetch all call relationships from the calls index."""
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


def build_call_graph(calls: List[Dict[str,Any]]):
    """Build call graph data structures from call relationships."""
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
    
    programs_with_calls = set(outgoing.keys()) | set(incoming.keys())
    return programs_with_calls, outgoing, incoming, sample_lines


def detect_cycles(programs: Set[str], outgoing: Dict[str, Set[str]], limit: int = 10000) -> Set[str]:
    """Detect cycles in the call graph."""
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
    """Compute call depth for each program."""
    memo: Dict[str,int] = {}
    visiting: Set[str] = set()

    def depth(node: str, d: int = 0) -> int:
        if node in memo:
            return memo[node]
        if d >= max_depth_cap:
            memo[node] = max_depth_cap
            return max_depth_cap
        if node in visiting:
            memo[node] = max_depth_cap  # cycle
            return max_depth_cap
        visiting.add(node)
        max_child = 0
        for child in outgoing.get(node, []):
            max_child = max(max_child, depth(child, d+1))
        visiting.discard(node)
        memo[node] = max_child + 1
        return memo[node]

    for p in programs:
        if p not in memo:
            depth(p)
    return memo


def generate_program_summary(program_id: str, 
                           has_calls: bool,
                           outgoing_count: int = 0,
                           incoming_count: int = 0,
                           unique_callees: int = 0,
                           unique_callers: int = 0,
                           has_cycles: bool = False,
                           call_depth_score: int = 0,
                           sample_call_lines: List[int] = None) -> str:
    """Generate descriptive summary text for a program."""
    parts = [f"Program {program_id}"]
    
    if has_calls:
        # Program with call relationships
        if outgoing_count > 0:
            parts.append(f"makes {outgoing_count} calls to {unique_callees} unique programs")
        if incoming_count > 0:
            parts.append(f"called by {unique_callers} programs ({incoming_count} total calls)")
        if has_cycles:
            parts.append("participates in call cycles")
        if call_depth_score > 0:
            parts.append(f"call depth {call_depth_score}")
        if sample_call_lines:
            lines_str = ','.join(map(str, sample_call_lines[:5]))
            parts.append(f"calls at lines {lines_str}")
        
        # Classify program role
        if outgoing_count == 0 and incoming_count > 0:
            parts.append("(leaf program - called but makes no calls)")
        elif outgoing_count > 0 and incoming_count == 0:
            parts.append("(root program - makes calls but not called)")
        elif outgoing_count > 0 and incoming_count > 0:
            parts.append("(intermediate program - both calls and is called)")
    else:
        # Isolated program with no call relationships
        parts.append("isolated program with no detected call relationships")
        file_ext = "CBL" if program_id.endswith(('.CBL', '_CBL')) else "CPY" if program_id.endswith(('.CPY', '_CPY')) else "unknown"
        parts.append(f"likely {file_ext} source file")
    
    return ' | '.join(parts)


def create_program_meta_docs(all_program_ids: Set[str],
                           programs_with_calls: Set[str], 
                           outgoing: Dict[str, Set[str]], 
                           incoming: Dict[str, Set[str]], 
                           sample_lines: Dict[str, List[int]],
                           cycles: Set[str],
                           depths: Dict[str, int]) -> List[Dict[str, Any]]:
    """Create program meta documents for ALL programs."""
    docs = []
    
    for program_id in all_program_ids:
        has_calls = program_id in programs_with_calls
        
        if has_calls:
            # Program with call relationships
            out_calls = outgoing.get(program_id, set())
            in_calls = incoming.get(program_id, set())
            outgoing_count = len(out_calls)
            incoming_count = len(in_calls)
            unique_callees = len(out_calls)
            unique_callers = len(in_calls)
            has_cycles = program_id in cycles
            call_depth_score = depths.get(program_id, 0)
            call_lines = sample_lines.get(program_id, [])
        else:
            # Isolated program
            outgoing_count = 0
            incoming_count = 0
            unique_callees = 0
            unique_callers = 0
            has_cycles = False
            call_depth_score = 0
            call_lines = []
        
        # Generate summary text
        program_summary = generate_program_summary(
            program_id, has_calls, outgoing_count, incoming_count, 
            unique_callees, unique_callers, has_cycles, call_depth_score, call_lines
        )
        
        doc = {
            "program_id": program_id,
            "outgoing_count": outgoing_count,
            "incoming_count": incoming_count,
            "unique_callees": list(sorted(outgoing.get(program_id, set())))[:1000] if has_calls else [],
            "unique_callers": list(sorted(incoming.get(program_id, set())))[:1000] if has_calls else [],
            "has_cycles": has_cycles,
            "call_depth_score": call_depth_score,
            "sample_call_lines": ','.join(str(x) for x in call_lines[:10]) if call_lines else '',
            "program_summary": program_summary,
            "program_role": "isolated" if not has_calls else ("leaf" if outgoing_count == 0 else ("root" if incoming_count == 0 else "intermediate")),
            "flow_graph_json": "{}",
            "ui_flag": False,
            "input_screen_paths_json": "[]",
            "reach_out_size": unique_callees,
            "reach_in_size": unique_callers,
            "centrality_score": 0.0,
            "risk_flag": False,
            "dynamic_call_ratio": 0.0,
            "external_callee_count": 0,
            "external_callees": [],
            "ui_path_participant": False,
            "ingested_at": None,
            "total_lines": None,
            "covered_lines": None,
            "coverage_pct": None,
            "paragraph_count": None,
            "avg_paragraph_length": None,
            "median_paragraph_length": None,
            "max_paragraph_length": None,
            "gap_count": None,
            "largest_gap_length": None,
            "classification": None,
            "coverage_ingested_at": None,
            "has_vector": False
        }
        docs.append(doc)
    
    return docs


def upload_docs(ep: str, key: str, docs: List[Dict[str, Any]], batch_size: int = 500):
    """Upload documents to the program meta index."""
    if not docs:
        return
    
    url = f"{ep}/indexes/{META_INDEX}/docs/index?api-version={API_VERSION}"
    total_uploaded = 0
    
    for i in range(0, len(docs), batch_size):
        batch = docs[i:i+batch_size]
        
        # Add search action
        for doc in batch:
            doc['@search.action'] = 'upload'
        
        payload = {"value": batch}
        r = requests.post(url, headers={"api-key": key, "Content-Type": "application/json"}, 
                         json=payload, timeout=120)
        
        if r.status_code not in (200, 201):
            raise SystemExit(f"Upload failed {r.status_code}: {r.text[:200]}")
        
        total_uploaded += len(batch)
        print(f"Uploaded {total_uploaded}/{len(docs)} docs...")
    
    return total_uploaded


def main():
    ap = argparse.ArgumentParser(description='Build complete program meta including all filesystem programs')
    ap.add_argument('--endpoint', help='Search service endpoint')
    ap.add_argument('--key', help='Search service key')
    ap.add_argument('--dry-run', action='store_true', help='Show what would be done without uploading')
    ap.add_argument('--batch', type=int, default=500, help='Upload batch size')
    args = ap.parse_args()
    
    load_local_settings()
    ep, key = resolve_endpoint_key(args)
    
    print("1. Discovering all program IDs from filesystem...")
    all_program_ids = get_all_program_ids_from_filesystem()
    print(f"   Found {len(all_program_ids)} unique program IDs")
    
    print("2. Fetching call relationships...")
    calls = fetch_all_calls(ep, key)
    print(f"   Found {len(calls)} call edges")
    
    print("3. Building call graph...")
    programs_with_calls, outgoing, incoming, sample_lines = build_call_graph(calls)
    print(f"   {len(programs_with_calls)} programs have call relationships")
    print(f"   {len(all_program_ids) - len(programs_with_calls)} programs are isolated")
    
    print("4. Detecting cycles...")
    cycles = detect_cycles(programs_with_calls, outgoing)
    print(f"   {len(cycles)} programs participate in cycles")
    
    print("5. Computing call depths...")
    depths = compute_depth(programs_with_calls, outgoing)
    
    print("6. Creating program meta documents...")
    docs = create_program_meta_docs(all_program_ids, programs_with_calls, outgoing, 
                                  incoming, sample_lines, cycles, depths)
    print(f"   Created {len(docs)} program meta documents")
    
    if args.dry_run:
        # Show sample of what would be uploaded
        print("\nSample documents (dry run):")
        for i, doc in enumerate(docs[:5]):
            print(f"  {i+1}. {doc['program_id']}: {doc['program_summary'][:100]}...")
        print(f"\nWould upload {len(docs)} total documents")
        return
    
    print("7. Uploading to program meta index...")
    uploaded = upload_docs(ep, key, docs, args.batch)
    print(f"   Successfully uploaded {uploaded} documents")
    
    print(f"\nComplete program meta build finished!")
    print(f"  Total programs: {len(docs)}")
    print(f"  Programs with calls: {len(programs_with_calls)}")
    print(f"  Isolated programs: {len(all_program_ids) - len(programs_with_calls)}")


if __name__ == '__main__':
    main()