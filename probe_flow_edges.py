#!/usr/bin/env python3
"""Utility to inspect raw cobol-flow-edges index contents.

Examples:
  python probe_flow_edges.py --search TIMER --top 30
  python probe_flow_edges.py --search TIM360 --top 50 --show-missing

If --search is omitted, defaults to '*'.
Outputs a compact table: edge_id caller_para -> target_para kind line
Use --json to dump raw JSON.
"""
import os, json, argparse, sys, requests

API_VERSION = "2024-07-01"


def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    cfg = {
        'search_endpoint': first('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT'),
        'search_key': first('AZURE_SEARCH_KEY','SEARCH_KEY'),
    }
    missing = [k for k,v in cfg.items() if not v]
    if missing:
        print(f"Missing config values: {missing}", file=sys.stderr)
        sys.exit(1)
    cfg['search_endpoint'] = cfg['search_endpoint'].rstrip('/')
    return cfg


def search_edges(cfg, term: str, top: int):
    url = f"{cfg['search_endpoint']}/indexes/cobol-flow-edges/docs/search?api-version={API_VERSION}"
    body = {"search": term, "top": top, "select": "edge_id,caller_para,target_para,line,kind,file_id"}
    r = requests.post(url, headers={'api-key':cfg['search_key'],'Content-Type':'application/json'}, json=body, timeout=30)
    if r.status_code != 200:
        raise RuntimeError(f"Search failed {r.status_code}: {r.text[:200]}")
    return r.json().get('value', [])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--search', default='*', help='Lexical search term (defaults to * for broad)')
    ap.add_argument('--top', type=int, default=40, help='Top N to fetch')
    ap.add_argument('--json', action='store_true', help='Output raw JSON')
    ap.add_argument('--show-missing', action='store_true', help='Show message if zero results with hints')
    args = ap.parse_args()

    cfg = load_settings()
    try:
        docs = search_edges(cfg, args.search, args.top)
    except Exception as ex:
        print(f"ERROR: {ex}", file=sys.stderr)
        sys.exit(2)

    if args.json:
        print(json.dumps(docs, ensure_ascii=False, indent=2))
        return

    if not docs:
        print("No edge docs returned.")
        if args.show_mising:
            print("  Hints: try --search TIMER, or confirm ingestion pipeline produced flow edges.")
        return

    print(f"Returned {len(docs)} edge docs (showing up to {args.top}):")
    for d in docs:
        print(f"{d.get('edge_id')}  {d.get('caller_para')} -> {d.get('target_para')}  kind={d.get('kind')} line={d.get('line')} file={d.get('file_id')}")

if __name__ == '__main__':
    main()
