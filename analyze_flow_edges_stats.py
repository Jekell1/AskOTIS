#!/usr/bin/env python3
"""Aggregate statistics over flow_edges.jsonl.

Outputs:
  - Total docs
  - Kind distribution
  - Unresolved proportion per kind
  - Top N programs by edge count
  - Programs with highest unresolved ratios (thresholded)

Usage:
  python analyze_flow_edges_stats.py --jsonl JSONL/flow_edges.jsonl --top 20
"""
import json, argparse, collections, math, sys
from pathlib import Path

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--jsonl', default='JSONL/flow_edges.jsonl')
    ap.add_argument('--top', type=int, default=15, help='Show top N programs by edge count')
    ap.add_argument('--min-program-edges', type=int, default=50, help='Minimum edges to consider when listing high unresolved ratios')
    args = ap.parse_args()

    path = Path(args.jsonl)
    if not path.exists():
        print(f"Missing file: {path}", file=sys.stderr)
        sys.exit(2)

    total=0
    kind_counter = collections.Counter()
    prog_counter = collections.Counter()
    prog_unresolved = collections.Counter()
    caller_missing = 0

    with path.open('r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                doc = json.loads(line)
            except Exception:
                continue
            total += 1
            k = doc.get('kind','?')
            kind_counter[k]+=1
            prog = doc.get('program_id') or doc.get('file_id') or 'UNKNOWN'
            prog_counter[prog]+=1
            if k.endswith(':unresolved'):
                prog_unresolved[prog]+=1
            if doc.get('caller_para')=='<<NONE>>':
                caller_missing += 1

    print(f"Total edges: {total}")
    print("Kind distribution:")
    for k,v in kind_counter.most_common():
        print(f"  {k:18s} {v:8d} {(v/total*100):5.1f}%")
    print(f"Caller_para missing: {caller_missing} ({caller_missing/total*100:.2f}%)")

    print(f"\nTop {args.top} programs by edge count:")
    for prog, cnt in prog_counter.most_common(args.top):
        un = prog_unresolved.get(prog,0)
        ratio = (un/cnt*100) if cnt else 0
        print(f"  {prog:20s} edges={cnt:6d} unresolved={un:6d} ({ratio:5.1f}%)")

    print("\nHigh unresolved ratio programs (>=50 edges & ratio>=70%):")
    for prog,cnt in prog_counter.most_common():
        if cnt < args.min_program_edges:
            continue
        un = prog_unresolved.get(prog,0)
        ratio = (un/cnt*100) if cnt else 0
        if ratio >= 70:
            print(f"  {prog:20s} edges={cnt:6d} unresolved={un:6d} ({ratio:5.1f}%)")

if __name__=='__main__':
    main()
