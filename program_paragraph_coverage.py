#!/usr/bin/env python3
"""Compute separate coverage stats for program vs paragraph targets.

Inputs:
  --edges JSONL/flow_edges_enriched_v2_kind.jsonl (must include target_kind + resolution_strategy)
  --out JSONL/program_paragraph_coverage.json (optional)

Metrics:
  - Overall edges
  - Paragraph edges: count, resolved %, unresolved %, strategy breakdown
  - Program edges: same breakdown
  - Unknown edges (neither classified) breakdown
  - Synthetic vs genuine split inside each category

Assumptions:
  resolution_strategy in edge, synthetic strategies = {'caller-derived','synthetic-paragraph','synthetic-program-ref','synthetic-paragraph-anchor','synthetic-paragraph-ref'}
  genuine strategies = others except 'none'
"""
from __future__ import annotations
import json, argparse
from collections import Counter
from typing import Dict

SYNTHETIC = {
    'caller-derived',
    'synthetic-paragraph',
    'synthetic-program-ref',
    'synthetic-paragraph-anchor',
    'synthetic-paragraph-ref'
}

def iter_jsonl(path: str):
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                yield json.loads(line)
            except Exception:
                continue

def classify(edge: dict) -> str:
    return edge.get('target_kind') or 'unknown'

def is_resolved(edge: dict) -> bool:
    strat = edge.get('resolution_strategy') or 'none'
    return strat != 'none'

def is_synthetic(edge: dict) -> bool:
    return (edge.get('resolution_strategy') or '') in SYNTHETIC

def aggregate(edges_path: str) -> Dict:
    cats = {'program':[], 'paragraph':[], 'unknown':[]}
    total=0
    for e in iter_jsonl(edges_path):
        cats[classify(e)].append(e)
        total+=1
    def stats(lst):
        if not lst:
            return {
                'count':0,'resolved':0,'resolved_pct':0.0,'synthetic_resolved':0,'genuine_resolved':0,
                'synthetic_share':0.0,'strategies':{} }
        count=len(lst)
        resolved=[e for e in lst if is_resolved(e)]
        syn=[e for e in resolved if is_synthetic(e)]
        strat_counter=Counter(e.get('resolution_strategy') or 'none' for e in lst)
        return {
            'count':count,
            'resolved':len(resolved),
            'resolved_pct': (len(resolved)/count*100.0),
            'synthetic_resolved': len(syn),
            'genuine_resolved': len(resolved)-len(syn),
            'synthetic_share': (len(syn)/len(resolved)*100.0) if resolved else 0.0,
            'strategies': dict(sorted(strat_counter.items(), key=lambda x: (-x[1], x[0])))
        }
    summary={
        'total_edges': total,
        'program': stats(cats['program']),
        'paragraph': stats(cats['paragraph']),
        'unknown': stats(cats['unknown'])
    }
    return summary

def print_report(summary: Dict):
    print("=== Program vs Paragraph Coverage ===")
    print(f"Total edges: {summary['total_edges']}")
    for cat in ['program','paragraph','unknown']:
        s=summary[cat]
        print(f"{cat.capitalize()} edges: {s['count']} resolved={s['resolved']} ({s['resolved_pct']:.2f}%) synthetic_share={s['synthetic_share']:.1f}%")
        print("  Strategies:")
        for k,v in s['strategies'].items():
            print(f"    {k:18s} {v}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', required=True)
    ap.add_argument('--out')
    args = ap.parse_args()
    summary = aggregate(args.edges)
    print_report(summary)
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump(summary,f,indent=2)
            print(f"Wrote {args.out}")

if __name__=='__main__':
    main()
