#!/usr/bin/env python3
"""Analyze v2 enriched flow edges + alias coverage.

Reads:
  JSONL/flow_edges_enriched.jsonl  (from resolve_flow_edges.py)
  JSONL/aliases_full.jsonl or JSONL/aliases.jsonl (alias docs) if present

Outputs (stdout):
  - Total edges
  - Resolution strategy counts & percentages
  - Resolved rate
  - Top unresolved raw targets (normalized) with counts
  - Family key distribution stats (p50, p90, max alias family size) from alias file
  - Alias coverage: percent of unique normalized raw_target tokens that appear as alias
  - Top raw targets missing alias

Optionally write JSON summary via --out stats.json

Usage:
  python analyze_flow_edges_v2_stats.py --edges JSONL/flow_edges_enriched.jsonl --aliases JSONL/aliases_full.jsonl --top 25
"""
from __future__ import annotations
import json, os, sys, argparse, re, statistics
from collections import Counter, defaultdict
from typing import Dict, List, Set

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    t = re.sub(r"[^A-Z0-9]+","-", t)
    return t.strip('-')

def load_jsonl(path: str):
    if not os.path.exists(path):
        return []
    out=[]
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            if not line.strip():
                continue
            try:
                out.append(json.loads(line))
            except Exception:
                continue
    return out

def analyze(edges_path: str, aliases_path: str|None, top: int):
    edges = load_jsonl(edges_path)
    if not edges:
        raise SystemExit(f"No edges loaded from {edges_path}")
    alias_docs = load_jsonl(aliases_path) if aliases_path else []

    strat_counter = Counter()
    unresolved_raw_tokens = Counter()
    raw_token_set: Set[str] = set()
    resolved_count = 0

    for e in edges:
        strat = e.get('resolution_strategy','(none)')
        strat_counter[strat]+=1
        raw = e.get('raw_target') or ''
        norm = normalize_token(raw)
        if norm:
            raw_token_set.add(norm)
            if strat in ('none','ambiguous') or not e.get('resolved'):
                unresolved_raw_tokens[norm]+=1
        if e.get('resolved'):
            resolved_count += 1

    total_edges = len(edges)
    summary: Dict[str,object] = {}
    summary['total_edges'] = total_edges
    summary['resolution_strategies'] = {
        k:{'count':v,'pct': round(v/total_edges*100,3)} for k,v in strat_counter.most_common()
    }
    summary['resolved_edges'] = resolved_count
    summary['resolved_pct'] = round(resolved_count/total_edges*100,3)
    summary['unique_raw_targets'] = len(raw_token_set)

    top_unresolved = unresolved_raw_tokens.most_common(top)
    summary['top_unresolved'] = top_unresolved

    alias_norms = {d.get('alias') for d in alias_docs if d.get('alias')}
    alias_coverage = 0.0
    missing_alias = []
    if alias_norms:
        covered = len([t for t in raw_token_set if t in alias_norms])
        alias_coverage = covered / len(raw_token_set) * 100 if raw_token_set else 0.0
        missing_alias = [t for t in raw_token_set if t not in alias_norms]
    summary['alias_docs'] = len(alias_docs)
    summary['alias_coverage_pct'] = round(alias_coverage,2)

    if missing_alias:
        # rank missing by how often they appear unresolved
        missing_ranked = [(t, unresolved_raw_tokens.get(t,0)) for t in missing_alias]
        missing_ranked.sort(key=lambda x: x[1], reverse=True)
        summary['top_missing_alias'] = missing_ranked[:top]
    else:
        summary['top_missing_alias'] = []

    # Family alias stats
    if alias_docs:
        fam_counter = defaultdict(int)
        for d in alias_docs:
            fam_counter[d.get('family_key','')] +=1
        fam_sizes = [v for v in fam_counter.values() if v>0]
        fam_sizes.sort()
        if fam_sizes:
            summary['family_alias_stats'] = {
                'families': len(fam_sizes),
                'p50': statistics.median(fam_sizes),
                'p90': fam_sizes[int(0.9*len(fam_sizes))-1],
                'max': max(fam_sizes)
            }
    return summary


def print_report(summary: Dict[str,object], top: int):
    print("=== Flow Edge v2 Stats ===")
    print(f"Total edges: {summary['total_edges']}")
    print("Resolution strategies:")
    for k,v in summary['resolution_strategies'].items():
        print(f"  {k:15s} {v['count']:10d} ({v['pct']:5.2f}%)")
    print(f"Resolved edges: {summary['resolved_edges']} ({summary['resolved_pct']}%)")
    print(f"Unique normalized raw targets: {summary['unique_raw_targets']}")
    print(f"Alias docs: {summary['alias_docs']}  coverage={summary['alias_coverage_pct']}%")
    if summary.get('family_alias_stats'):
        fas = summary['family_alias_stats']
        print(f"Family alias size stats: families={fas['families']} p50={fas['p50']} p90={fas['p90']} max={fas['max']}")
    print(f"Top unresolved raw targets (limit {top}):")
    for t,c in summary['top_unresolved']:
        print(f"  {t:40s} {c}")
    if summary['top_missing_alias']:
        print(f"Top missing alias candidates (limit {top}):")
        for t,c in summary['top_missing_alias']:
            print(f"  {t:40s} {c}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', default='JSONL/flow_edges_enriched.jsonl')
    ap.add_argument('--aliases', default='JSONL/aliases_full.jsonl')
    ap.add_argument('--top', type=int, default=25)
    ap.add_argument('--out', help='Optional path to JSON summary')
    args = ap.parse_args()

    summary = analyze(args.edges, args.aliases, args.top)
    print_report(summary, args.top)
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump(summary,f,indent=2)
        print(f"Wrote summary JSON to {args.out}")

if __name__ == '__main__':
    main()
