"""Refine target_kind (program/paragraph/unknown) using provenance heuristics.

Inputs:
  JSONL/flow_edges_enriched_v2_prov.jsonl (edges with provenance.heuristic_class + program_call_likelihood)
Env overrides:
  EDGE_FILE: path to provenance-augmented edges

Outputs:
  JSONL/flow_edges_enriched_v2_kind2.jsonl (adds / updates target_kind)
  Prints summary counts.

Logic:
  - If edge already has target_kind in {program, paragraph} keep unless provenance contradicts with strong likelihood (>0.85 different class).
  - For unresolved edges only:
      provenance.heuristic_class mapping:
         likely_program_call -> target_kind = program
         likely_paragraph    -> target_kind = paragraph
         ambiguous -> remain unknown except if program_call_likelihood >=0.75 => program, <=0.25 => paragraph
  - We do NOT mark resolved program edges as paragraph and vice versa unless strong contradiction.
  - Adds field kind_refinement = { "source": "provenance-v1", "changed": bool }

Later scripts can leverage 'program' vs 'paragraph'.
"""
from __future__ import annotations
import os, json
from pathlib import Path
from collections import Counter

EDGE_FILE = Path(os.environ.get('EDGE_FILE','JSONL/flow_edges_enriched_v2_prov.jsonl'))
OUTPUT_FILE = Path('JSONL/flow_edges_enriched_v2_kind2.jsonl')

PROGRAM_TAG = 'program'
PARA_TAG = 'paragraph'


def decide(edge: dict):
    existing = edge.get('target_kind')
    prov = (edge.get('provenance') or {}).get('heuristic_class')
    likelihood = edge.get('program_call_likelihood')
    resolved = edge.get('resolved', False)

    new_kind = existing or 'unknown'
    changed = False

    # Skip changing resolved edges unless strong contradiction.
    if resolved and existing in {PROGRAM_TAG, PARA_TAG}:
        if prov == 'likely_program_call' and existing != PROGRAM_TAG and likelihood and likelihood > 0.85:
            new_kind = PROGRAM_TAG
            changed = True
        elif prov == 'likely_paragraph' and existing != PARA_TAG and likelihood and likelihood < 0.15:
            new_kind = PARA_TAG
            changed = True
        return new_kind, changed

    if not resolved:
        if prov == 'likely_program_call':
            if new_kind != PROGRAM_TAG:
                new_kind = PROGRAM_TAG
                changed = True
        elif prov == 'likely_paragraph':
            if new_kind != PARA_TAG:
                new_kind = PARA_TAG
                changed = True
        elif prov == 'ambiguous':
            if likelihood is not None:
                if likelihood >= 0.75 and new_kind != PROGRAM_TAG:
                    new_kind = PROGRAM_TAG
                    changed = True
                elif likelihood <= 0.25 and new_kind != PARA_TAG:
                    new_kind = PARA_TAG
                    changed = True
    return new_kind, changed


def main():
    if not EDGE_FILE.exists():
        print(f"Edge file not found: {EDGE_FILE}")
        return
    counts = Counter()
    changes = 0
    total = 0
    with EDGE_FILE.open('r', encoding='utf-8') as fin, OUTPUT_FILE.open('w', encoding='utf-8') as fout:
        for line in fin:
            line=line.rstrip('\n')
            if not line: continue
            try:
                edge=json.loads(line)
            except json.JSONDecodeError:
                fout.write(line+'\n')
                continue
            total += 1
            new_kind, changed = decide(edge)
            edge['target_kind'] = new_kind
            if changed:
                edge['kind_refinement'] = { 'source':'provenance-v1','changed':True }
                changes += 1
            else:
                if 'kind_refinement' not in edge:
                    edge['kind_refinement'] = { 'source':'provenance-v1','changed':False }
            counts[new_kind]+=1
            fout.write(json.dumps(edge)+'\n')
    print(f"Refinement complete -> {OUTPUT_FILE}")
    print(f"Total edges: {total}")
    for k,v in counts.most_common():
        print(f"  {k}: {v}")
    print(f"Changed kinds: {changes}")

if __name__=='__main__':
    main()
