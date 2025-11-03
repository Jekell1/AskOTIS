"""Synthesize paragraph anchors for unresolved paragraph-likely edges.

Purpose:
  After provenance-based kind refinement, many edges tagged paragraph remain unresolved.
  We create synthetic anchor records so downstream retrieval / RAG has stable targets.

Approach:
  Stream refined edges file (default: JSONL/flow_edges_enriched_v2_kind2.jsonl)
  For each edge:
    - If resolved -> passthrough.
    - Else if target_kind == 'paragraph':
         Create synthetic resolution:
            resolved = True
            resolution_strategy = 'synthetic-paragraph'
            synthetic_target = True
            confidence = max(existing confidence, 0.55)
            resolved_target_para = raw_target (if absent)
            Add synthetic_meta = { 'source': 'paragraph-anchor-v1' }
    - Else if target_kind == 'program' but unresolved and program_call_likelihood >= 0.9:
         Provide minimal synthetic program placeholder (strategy 'synthetic-program-ref') with confidence 0.5.

  Additionally we will emit a side-car JSONL file with unique synthetic paragraphs:
      JSONL/synthetic_paragraph_anchors.jsonl containing one record per unique (file_id, resolved_target_para)
      { 'anchor_kind':'paragraph','file_id':..., 'paragraph':..., 'source':'synthetic', 'edges': <count> }

Outputs:
  JSONL/flow_edges_enriched_v2_synth.jsonl
  JSONL/synthetic_paragraph_anchors.jsonl

Env overrides:
  EDGE_FILE -> input refined edges file
"""
from __future__ import annotations
import os, json
from pathlib import Path
from collections import defaultdict

INPUT_FILE = Path(os.environ.get('EDGE_FILE','JSONL/flow_edges_enriched_v2_kind2.jsonl'))
OUTPUT_FILE = Path('JSONL/flow_edges_enriched_v2_synth.jsonl')
ANCHORS_FILE = Path('JSONL/synthetic_paragraph_anchors.jsonl')

PARA_STRATEGY = 'synthetic-paragraph'
PROG_REF_STRATEGY = 'synthetic-program-ref'


def main():
    if not INPUT_FILE.exists():
        print(f"Input edges not found: {INPUT_FILE}")
        return
    os.makedirs('JSONL', exist_ok=True)
    paragraph_anchor_counts = defaultdict(int)
    paragraph_anchor_files = defaultdict(set)
    total=0
    para_synth=0
    prog_synth=0
    with INPUT_FILE.open('r', encoding='utf-8') as fin, OUTPUT_FILE.open('w', encoding='utf-8') as fout:
        for line in fin:
            line=line.rstrip('\n')
            if not line: continue
            try:
                edge=json.loads(line)
            except json.JSONDecodeError:
                fout.write(line+'\n')
                continue
            total+=1
            if not edge.get('resolved'):
                kind=edge.get('target_kind')
                if kind=='paragraph':
                    rt = edge.get('raw_target') or edge.get('resolved_target_para')
                    if rt:
                        edge['resolved']=True
                        edge['resolution_strategy']=PARA_STRATEGY
                        edge['synthetic_target']=True
                        edge['confidence']=max(edge.get('confidence',0.0),0.55)
                        edge['resolved_target_para']=rt
                        edge.setdefault('synthetic_meta',{})['source']='paragraph-anchor-v1'
                        para_synth+=1
                        paragraph_anchor_counts[(edge.get('file_id'), rt.upper())]+=1
                elif kind=='program':
                    pll = edge.get('program_call_likelihood') or 0.0
                    if pll >= 0.9:
                        edge['resolved']=True
                        edge['resolution_strategy']=PROG_REF_STRATEGY
                        edge['synthetic_target']=True
                        edge['confidence']=max(edge.get('confidence',0.0),0.5)
                        edge.setdefault('synthetic_meta',{})['source']='program-ref-v1'
                        prog_synth+=1
            fout.write(json.dumps(edge)+'\n')
    # Write anchors sidecar
    with ANCHORS_FILE.open('w', encoding='utf-8') as af:
        for (file_id, para), cnt in paragraph_anchor_counts.items():
            af.write(json.dumps({
                'anchor_kind':'paragraph',
                'file_id': file_id,
                'paragraph': para,
                'source':'synthetic',
                'edges': cnt
            })+'\n')
    print(f"Synthetic paragraph synthesis complete -> {OUTPUT_FILE}")
    print(f"Total edges: {total} synthetic_paragraph_resolved={para_synth} synthetic_program_refs={prog_synth}")
    print(f"Unique paragraph anchors: {len(paragraph_anchor_counts)} -> {ANCHORS_FILE}")

if __name__=='__main__':
    main()
