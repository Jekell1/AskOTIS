#!/usr/bin/env python3
"""Augment flow edges with target_kind classification (program|paragraph|unknown).

Inputs:
  --edges JSONL/flow_edges_enriched_v2_real.jsonl (or similar)
  --program-ids JSONL/program_ids.jsonl produced by index_program_ids.py
  --out JSONL/flow_edges_enriched_v2_kind.jsonl

Logic:
  - Build a set of PROGRAM-ID and ENTRY names (UPPER) -> classify as 'program'
  - If edge raw_target (or normalized_target if present) matches program set => target_kind=program
  - Else if existing resolution strategy is one of (normalized-equals, file-local-equals) => target_kind=paragraph
  - Else if caller-derived (synthetic) and not in program set => unknown (still ambiguous)
  - Preserve original fields; add target_kind

This allows separating paragraph coverage from genuine program call external references.
"""
from __future__ import annotations
import json, argparse
from typing import Set

def load_set(path: str) -> Set[str]:
    s=set()
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                obj=json.loads(line)
            except Exception:
                continue
            name=obj.get('name')
            if not name: continue
            s.add(str(name).upper())
    return s

def iter_jsonl(path: str):
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                yield json.loads(line)
            except Exception:
                continue

def classify(edge: dict, programs: Set[str]) -> str:
    raw = edge.get('raw_target') or ''
    norm = edge.get('normalized_target') or ''
    upper_raw = raw.upper()
    upper_norm = norm.upper()
    if upper_raw in programs or upper_norm in programs:
        return 'program'
    strat = edge.get('resolution_strategy') or ''
    if strat in ('normalized-equals','file-local-equals'):
        return 'paragraph'
    return 'unknown'

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', required=True)
    ap.add_argument('--program-ids', required=True)
    ap.add_argument('--out', required=True)
    args = ap.parse_args()

    programs = load_set(args.program_ids)
    total=0
    kind_counts={'program':0,'paragraph':0,'unknown':0}
    with open(args.out,'w',encoding='utf-8') as fout:
        for edge in iter_jsonl(args.edges):
            k = classify(edge, programs)
            edge['target_kind']=k
            fout.write(json.dumps(edge, ensure_ascii=False)+'\n')
            total+=1
            kind_counts[k]+=1
    print(f"Augmented {total} edges -> {args.out}")
    for k,v in kind_counts.items():
        print(f"  {k}: {v}")

if __name__=='__main__':
    main()
