#!/usr/bin/env python3
"""Locate edges involving a target token (program_id, caller_para, target_para, or blob_name substring).

Usage:
  python find_program_edges.py --token TIM360 --limit 100 --json
  python find_program_edges.py --token ACUMEM --kinds perform,call

Outputs matching edges (truncated by limit) with minimal columns.
"""
import json, argparse, sys
from pathlib import Path

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--jsonl', default='JSONL/flow_edges.jsonl')
    ap.add_argument('--token', required=True, help='Case-insensitive substring to match')
    ap.add_argument('--limit', type=int, default=50, help='Max rows to output')
    ap.add_argument('--kinds', default=None, help='Comma list of kinds to include (e.g. perform,call,goto) supports :unresolved suffix match automatically')
    ap.add_argument('--json', action='store_true', help='Emit raw JSON lines instead of table')
    args = ap.parse_args()

    path = Path(args.jsonl)
    if not path.exists():
        print(f"Missing file: {path}", file=sys.stderr)
        sys.exit(2)
    tok = args.token.lower()
    allowed = None
    if args.kinds:
        allowed = {k.strip() for k in args.kinds.split(',') if k.strip()}

    matches = []
    with path.open('r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                doc = json.loads(line)
            except Exception:
                continue
            k = doc.get('kind','')
            base_kind = k.split(':',1)[0]
            if allowed and base_kind not in allowed:
                continue
            hay = ' '.join(str(doc.get(x,'')) for x in ('program_id','caller_para','target_para','blob_name'))
            if tok in hay.lower():
                matches.append(doc)
                if len(matches)>=args.limit:
                    break

    if args.json:
        for m in matches:
            print(json.dumps(m, ensure_ascii=False))
        return

    if not matches:
        print('No matches found.')
        return
    print(f"Found {len(matches)} matches (showing up to {args.limit}):")
    for m in matches:
        print(f"{m.get('edge_id')[:28]:28s} {m.get('program_id','?')[:10]:10s} {m.get('caller_para')[:18]:18s} -> {m.get('target_para')[:20]:20s} {m.get('kind'):16s} line={m.get('line')} file={m.get('blob_name')}")

if __name__=='__main__':
    main()
