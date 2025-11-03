"""Upgrade synthetic paragraph resolutions by attaching genuine paragraph definition metadata.

Inputs:
  Edges file: JSONL/flow_edges_enriched_v2_synth.jsonl (or EDGE_FILE env)
  Paragraph definitions: JSONL/paragraph_definitions.jsonl

Outputs:
  JSONL/flow_edges_enriched_v2_realdefs.jsonl

Process:
  1. Load paragraph definitions building map: (file_id, paragraph) -> {start_line,end_line,hash,...}
  2. Stream edges:
       - If edge.resolved and edge.resolution_strategy == 'synthetic-paragraph' and has resolved_target_para (or raw_target) as name N
         and file_id present.
         -> Lookup (file_id, N.upper()) in definitions.
         -> If found, set:
              resolution_strategy = 'paragraph-definition'
              synthetic_target = False
              confidence = max(existing, 0.85)
              paragraph_definition = {...subset fields...}
              upgraded = True
       - Else pass through unchanged.
  3. Track stats: total synthetic paragraphs encountered, matched, unmatched.
  4. Write upgraded edges file.

Subset fields stored:
  start_line, end_line, body_line_count, section, hash, source

"""
from __future__ import annotations
import os, json
from pathlib import Path
from collections import defaultdict

EDGE_FILE = Path(os.environ.get('EDGE_FILE','JSONL/flow_edges_enriched_v2_synth.jsonl'))
PARA_FILE = Path('JSONL/paragraph_definitions.jsonl')
OUTPUT_FILE = Path('JSONL/flow_edges_enriched_v2_realdefs.jsonl')


def load_paragraphs():
    mapping = {}
    if not PARA_FILE.exists():
        print(f"Paragraph definitions not found: {PARA_FILE}")
        return mapping
    with PARA_FILE.open('r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                obj=json.loads(line)
            except json.JSONDecodeError:
                continue
            para = obj.get('paragraph')
            file_id = obj.get('file_id')
            if not para or not file_id:
                continue
            mapping[(file_id.upper(), para.upper())]=obj
    return mapping


def main():
    if not EDGE_FILE.exists():
        print(f"Edge file missing: {EDGE_FILE}")
        return
    para_map = load_paragraphs()
    print(f"Loaded {len(para_map)} paragraph definitions into map")
    total_edges=0
    synth_para_edges=0
    upgraded=0
    unmatched=0
    with EDGE_FILE.open('r',encoding='utf-8') as fin, OUTPUT_FILE.open('w',encoding='utf-8') as fout:
        for line in fin:
            line=line.rstrip('\n')
            if not line: continue
            try:
                edge=json.loads(line)
            except json.JSONDecodeError:
                fout.write(line+'\n')
                continue
            total_edges+=1
            if edge.get('resolution_strategy')=='synthetic-paragraph':
                synth_para_edges+=1
                name = (edge.get('resolved_target_para') or edge.get('raw_target') or '').upper()
                file_id = (edge.get('file_id') or '').upper()
                if name and file_id:
                    pd = para_map.get((file_id, name))
                    if pd:
                        # upgrade
                        edge['resolution_strategy'] = 'paragraph-definition'
                        edge['synthetic_target'] = False
                        edge['confidence'] = max(edge.get('confidence',0.0), 0.85)
                        edge['paragraph_definition'] = {
                            'start_line': pd.get('start_line'),
                            'end_line': pd.get('end_line'),
                            'body_line_count': pd.get('body_line_count'),
                            'section': pd.get('section'),
                            'hash': pd.get('hash'),
                            'source': pd.get('source')
                        }
                        edge.setdefault('upgrade_meta',{})['paragraph_upgraded']=True
                        upgraded+=1
                    else:
                        unmatched+=1
            fout.write(json.dumps(edge)+'\n')
    print(f"Upgrade complete -> {OUTPUT_FILE}")
    print(f"Total edges: {total_edges} synthetic_paragraph_edges={synth_para_edges} upgraded={upgraded} unmatched={unmatched}")

if __name__=='__main__':
    main()
