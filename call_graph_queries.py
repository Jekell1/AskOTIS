"""Interactive/CLI helper to query COBOL call graph indexes.

Supports:
  who-calls <PROGRAM>
  callees <PROGRAM>
  chain <PROGRAM> [--depth N]
  rchain <PROGRAM> [--depth N]
  search-line <PROGRAM>:<LINE>

Uses:
  - new_cobol_program_meta for aggregate sets when available
  - falls back to new_cobol_calls scanning when needed (with pagination)

Usage examples:
    python call_graph_queries.py who-calls ORDERS
    python call_graph_queries.py callees PAYROLL
    python call_graph_queries.py chain BILLING --depth 5
    python call_graph_queries.py rchain INVENTORY --depth 5
    python call_graph_queries.py search-line ACCOUNT:1200
"""
from __future__ import annotations
import os, json, argparse, requests, sys, collections, functools
from typing import List, Dict, Any, Set

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
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if args.endpoint: ep = args.endpoint
    if args.key: key = args.key
    if not ep or not key:
        print('Missing endpoint/key', file=sys.stderr)
        sys.exit(1)
    return ep.rstrip('/'), key


def search_program_meta(ep: str, key: str, prog: str) -> Dict[str,Any] | None:
    url = f"{ep}/indexes/{META_INDEX}/docs/search.post.search?api-version={API_VERSION}"
    body = {"search": prog, "filter": f"program_id eq '{prog}'", "top": 1}
    r = requests.post(url, headers={"api-key": key, "Content-Type":"application/json"}, json=body)
    if r.status_code != 200:
        raise SystemExit(f"Meta search error {r.status_code}: {r.text[:200]}")
    vals = r.json().get('value', [])
    return vals[0] if vals else None


def fetch_calls_for_program(ep: str, key: str, prog: str, direction: str) -> List[Dict[str,Any]]:
    # direction: 'incoming' (callee == prog) or 'outgoing' (caller == prog)
    filt = "callee_program" if direction == 'incoming' else 'caller_program'
    url = f"{ep}/indexes/{CALLS_INDEX}/docs/search.post.search?api-version={API_VERSION}"
    out: List[Dict[str,Any]] = []
    skip = 0
    while True:
        body = {"search": prog, "filter": f"{filt} eq '{prog}'", "top": 1000, "skip": skip, "select": "caller_program,callee_program,line"}
        r = requests.post(url, headers={"api-key": key, "Content-Type":"application/json"}, json=body)
        if r.status_code != 200:
            raise SystemExit(f"Calls fetch error {r.status_code}: {r.text[:200]}")
        vals = r.json().get('value', [])
        out.extend(vals)
        if len(vals) < 1000:
            break
        skip += 1000
    return out


def who_calls(ep: str, key: str, prog: str):
    meta = search_program_meta(ep, key, prog)
    if meta:
        callers = meta.get('unique_callers', [])
    else:
        callers = sorted({c['caller_program'] for c in fetch_calls_for_program(ep, key, prog, 'incoming')})
    print(f"Programs calling {prog}: {len(callers)}")
    for c in callers:
        print('  ', c)


def callees(ep: str, key: str, prog: str):
    meta = search_program_meta(ep, key, prog)
    if meta:
        callees = meta.get('unique_callees', [])
    else:
        callees = sorted({c['callee_program'] for c in fetch_calls_for_program(ep, key, prog, 'outgoing')})
    print(f"Programs called by {prog}: {len(callees)}")
    for c in callees:
        print('  ', c)


def traverse_chain(ep: str, key: str, root: str, depth: int, reverse: bool = False):
    visited: Set[str] = set()
    edge_cache: Dict[str, Set[str]] = {}
    direction = 'incoming' if reverse else 'outgoing'

    def neighbors(p: str) -> Set[str]:
        if p in edge_cache:
            return edge_cache[p]
        meta = search_program_meta(ep, key, p)
        if meta:
            neigh = set(meta.get('unique_callers' if reverse else 'unique_callees', []))
        else:
            calls = fetch_calls_for_program(ep, key, p, 'incoming' if reverse else 'outgoing')
            keyfield = 'caller_program' if reverse else 'callee_program'
            neigh = {c[keyfield] for c in calls}
        edge_cache[p] = neigh
        return neigh

    def dfs(p: str, d: int):
        if d > depth or p in visited:
            return
        visited.add(p)
        print('  ' * d + p)
        for nxt in sorted(neighbors(p)):
            dfs(nxt, d+1)

    dfs(root, 0)


def search_line(ep: str, key: str, spec: str):
    if ':' not in spec:
        print("Format PROGRAM:LINE required", file=sys.stderr)
        return
    prog, line = spec.split(':',1)
    try:
        line_int = int(line)
    except ValueError:
        print('Line must be int', file=sys.stderr)
        return
    url = f"{ep}/indexes/{CALLS_INDEX}/docs/search.post.search?api-version={API_VERSION}"
    body = {"search": prog, "filter": f"caller_program eq '{prog}' and line eq {line_int}", "top": 50, "select": "caller_program,callee_program,line,snippet"}
    r = requests.post(url, headers={"api-key": key, "Content-Type":"application/json"}, json=body)
    if r.status_code != 200:
        raise SystemExit(f"Search line error {r.status_code}: {r.text[:160]}")
    vals = r.json().get('value', [])
    for v in vals:
        print(f"{v['caller_program']}:{v['line']} -> {v['callee_program']} | {v['snippet']}")
    if not vals:
        print('No matches.')


def main():
    ap = argparse.ArgumentParser(description='COBOL call graph query helper.')
    ap.add_argument('command', choices=['who-calls','callees','chain','rchain','search-line'])
    ap.add_argument('target')
    ap.add_argument('--depth', type=int, default=5)
    ap.add_argument('--endpoint')
    ap.add_argument('--key')
    args = ap.parse_args()
    load_local_settings()
    ep, key = resolve_endpoint_key(args)

    cmd = args.command
    if cmd == 'who-calls':
        who_calls(ep, key, args.target.upper())
    elif cmd == 'callees':
        callees(ep, key, args.target.upper())
    elif cmd == 'chain':
        traverse_chain(ep, key, args.target.upper(), args.depth, reverse=False)
    elif cmd == 'rchain':
        traverse_chain(ep, key, args.target.upper(), args.depth, reverse=True)
    elif cmd == 'search-line':
        search_line(ep, key, args.target.upper())

if __name__ == '__main__':
    main()
