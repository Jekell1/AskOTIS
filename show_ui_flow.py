"""Pretty-print UI navigation paths from new_cobol_ui_paths.

Usage examples:
  python show_ui_flow.py --root MAINMENU01 --top 3
  python show_ui_flow.py --auto-root --top 5 --min-length 2

If --auto-root is used, we query new_cobol_program_meta for the highest reach_out_size UI/dispatch program.

Environment: AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY or SEARCH_ENDPOINT / SEARCH_KEY
Fallback: local.settings.json Values.
"""
from __future__ import annotations
import os, sys, json, argparse, requests
from typing import Any, Dict, List

API_VERSION = os.getenv('AZURE_SEARCH_API_VERSION','2025-08-01-preview')
UI_PATHS_INDEX = 'new_cobol_ui_paths'
PROGRAM_META_INDEX = 'new_cobol_program_meta'


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

def search(index: str, body: Dict[str,Any], ep: str, key: str) -> Dict[str,Any]:
    url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key,'Content-Type':'application/json'}, json=body)
    if r.status_code!=200:
        raise SystemExit(f"Search error {r.status_code}: {r.text[:300]}")
    return r.json()

def auto_root(ep: str, key: str) -> str|None:
    # Corrected filter: Azure Search OData filters use single quotes for string literals
    filter_expr = "program_role eq 'DISPATCH' or program_role eq 'UI' or ui_flag eq true"
    body = {
        'search':'*',
        'filter': filter_expr,
        'orderby':'reach_out_size desc, centrality_score desc',
        'top':1,
        'select':'program_id,reach_out_size,centrality_score'
    }
    try:
        js = search(PROGRAM_META_INDEX, body, ep, key)
        vals = js.get('value',[])
        if vals:
            return vals[0]['program_id']
    except SystemExit as e:
        # Fallback: retry without filter (broadest high-reach candidate) if filter caused an error
        if "Invalid expression" in str(e):
            try:
                fb_body = {
                    'search':'*',
                    'orderby':'reach_out_size desc, centrality_score desc',
                    'top':1,
                    'select':'program_id'
                }
                js = search(PROGRAM_META_INDEX, fb_body, ep, key)
                vals = js.get('value',[])
                if vals:
                    return vals[0]['program_id']
            except Exception:
                pass
        raise
    return None

def get_paths(ep: str, key: str, root: str, top: int) -> List[Dict[str,Any]]:
    body = {
        'search': root,
        'searchFields': 'root_program_id',
        'orderby': 'score desc',
        'top': top*5,  # over-fetch to allow local min-length filtering
        'select': 'path_id,program_sequence_json,score,length,loop_collapsed,root_program_id,leaf_program_id,ui_program_count'
    }
    js = search(UI_PATHS_INDEX, body, ep, key)
    return js.get('value',[])

def format_path(rec: Dict[str,Any], idx: int) -> str:
    seq = json.loads(rec.get('program_sequence_json') or '[]')
    parts = []
    for i,p in enumerate(seq, start=1):
        parts.append(f"  {i:02d}. {p}")
    meta = f"(score={rec.get('score')}, length={rec.get('length')}, ui={rec.get('ui_program_count')}, loop={rec.get('loop_collapsed')})"
    return f"Path {idx} {meta}\n" + '\n'.join(parts)

def main():
    ap = argparse.ArgumentParser(description='Show heuristic UI navigation flows.')
    ap.add_argument('--root', help='Root program id (override).')
    ap.add_argument('--auto-root', action='store_true', help='Select top-ranked UI/dispatch root automatically.')
    ap.add_argument('--top', type=int, default=5)
    ap.add_argument('--min-length', type=int, default=1, help='Minimum path length to display.')
    args = ap.parse_args()
    load_local_settings()
    ep,key=resolve_endpoint_key()
    root = args.root
    if not root and args.auto_root:
        try:
            root = auto_root(ep,key)
        except SystemExit as e:
            print(f"Auto-root selection failed: {e}", file=sys.stderr)
            root = None
        if not root:
            print('No root candidate found.')
            return
    if not root:
        print('Root not specified. Use --root PROGRAM or --auto-root')
        return
    paths = get_paths(ep,key,root,args.top)
    if not paths:
        print(f'No paths found for root {root}')
        return
    # client-side filter
    filtered = [p for p in paths if (p.get('length') or 0) >= args.min_length]
    filtered.sort(key=lambda r: (r.get('score') or 0), reverse=True)
    shown = filtered[:args.top]
    print(f"Root: {root}  (showing {len(shown)} of {len(filtered)} filtered paths; raw fetched={len(paths)})\n")
    if not shown:
        print('No paths meet min-length constraint.')
        return
    for i,rec in enumerate(shown, start=1):
        print(format_path(rec,i))
        print('-'*60)

if __name__=='__main__':
    main()
