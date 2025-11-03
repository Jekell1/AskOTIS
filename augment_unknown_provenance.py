"""Augment unresolved edges with provenance & heuristic classification.

Goals:
 - For each unresolved edge, classify raw_target as likely_program_call, likely_paragraph, or ambiguous.
 - Add fields:
      provenance: { "heuristic_class": str, "signals": {..}, "version": "v1" }
      program_call_likelihood: float (0-1)
 - Optionally create a synthetic external program placeholder suggestion when likely_program_call.

Heuristics (scored signals):
 1. Contains hyphen(s) and starts with verb-like token (OPEN, READ, WRITE, UPDATE, DELETE, CREATE, CLOSE) => paragraph-ish (operation on resource) unless ends with -PROGRAM.
 2. Ends with -EXIT or -INIT => paragraph (common pattern).
 3. ALLCAPS short (<=8 chars) with no hyphen and appears multiple times as call/goto target across files => likely program.
 4. Appears as raw_target in a 'call' edge_subkind => strong program signal.
 5. Appears as raw_target in perform/goto with calling file_id matching raw_target prefix => paragraph.
 6. Target matches known program_id list (from program_ids.jsonl) => program.

Scoring:
  program_score accumulative; paragraph_score accumulative.
  likelihood = program_score / (program_score + paragraph_score + 1e-6).

Outputs new JSONL file: JSONL/flow_edges_enriched_v2_prov.jsonl
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from collections import Counter, defaultdict

EDGE_FILE = Path(os.environ.get('EDGE_FILE', 'JSONL/flow_edges_enriched_v2_copybook.jsonl'))
PROGRAM_IDS_FILE = Path('JSONL/program_ids.jsonl')
OUTPUT_FILE = Path('JSONL/flow_edges_enriched_v2_prov.jsonl')

VERB_PREFIXES = {'OPEN','READ','WRITE','UPDATE','DELETE','CREATE','CLOSE','ADD','REMOVE','INIT','BUILD','LOAD','SAVE','CHECK','SET','GET'}

def load_known_program_ids():
    known = set()
    if PROGRAM_IDS_FILE.exists():
        with PROGRAM_IDS_FILE.open('r', encoding='utf-8') as f:
            for line in f:
                line=line.strip()
                if not line: continue
                try:
                    obj=json.loads(line)
                except json.JSONDecodeError:
                    continue
                pid = obj.get('program_id')
                if pid:
                    known.add(pid.upper())
                entry = obj.get('entry_id')
                if entry:
                    known.add(entry.upper())
    return known

def first_pass_collect():
    target_counts = Counter()
    edge_kind_counts = defaultdict(Counter)
    if not EDGE_FILE.exists():
        return target_counts, edge_kind_counts
    with EDGE_FILE.open('r', encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                e=json.loads(line)
            except json.JSONDecodeError:
                continue
            if e.get('resolved'):  # only unresolved needed? We gather all for context.
                continue
            rt = (e.get('raw_target') or '').upper()
            if not rt: continue
            target_counts[rt]+=1
            edge_kind_counts[rt][e.get('edge_subkind','')] += 1
    return target_counts, edge_kind_counts

def classify(rt: str, edge: dict, counts: Counter, kind_counts: dict, known_programs: set[str]):
    signals = {}
    program_score = 0.0
    paragraph_score = 0.0

    # Signal 1: hyphen verb start
    parts = rt.split('-')
    if len(parts) > 1 and parts[0] in VERB_PREFIXES:
        paragraph_score += 1.5
        signals['verb_paragraph_pattern'] = True

    # Signal 2: suffix -EXIT / -INIT
    if rt.endswith('-EXIT') or rt.endswith('-INIT'):
        paragraph_score += 1.0
        signals['exit_or_init_suffix'] = True

    # Signal 3: short allcaps no hyphen
    if '-' not in rt and len(rt) <= 8:
        program_score += 1.2
        signals['short_allcaps_no_hyphen'] = True

    # Signal 4: appears in call edges
    ekc = kind_counts.get(rt, {})
    call_count = 0
    for k,v in ekc.items():
        if 'call' in k:
            call_count += v
    if call_count > 0:
        program_score += 2.0
        signals['appears_in_call_edges'] = call_count

    # Signal 5: perform/goto with prefix relation
    file_id = (edge.get('file_id') or '').upper()
    if file_id and rt.startswith(file_id):
        paragraph_score += 1.0
        signals['target_starts_with_file_id'] = True

    # Signal 6: known program id
    if rt in known_programs:
        program_score += 3.0
        signals['known_program_id'] = True

    # Count based weighting
    freq = counts.get(rt, 0)
    if freq >= 10:
        program_score += 0.5  # frequent unresolved target -> maybe program reused
        signals['high_frequency'] = freq

    total = program_score + paragraph_score + 1e-6
    program_likelihood = program_score / total
    if program_likelihood >= 0.67:
        heuristic_class = 'likely_program_call'
    elif program_likelihood <= 0.33:
        heuristic_class = 'likely_paragraph'
    else:
        heuristic_class = 'ambiguous'

    return heuristic_class, program_likelihood, signals

def main():
    known_programs = load_known_program_ids()
    counts, kind_counts = first_pass_collect()
    print(f"Collected unresolved target stats: {len(counts)} unique unresolved targets")

    with EDGE_FILE.open('r', encoding='utf-8') as fin, OUTPUT_FILE.open('w', encoding='utf-8') as fout:
        for line in fin:
            line=line.rstrip('\n')
            if not line:
                continue
            try:
                edge=json.loads(line)
            except json.JSONDecodeError:
                fout.write(line+'\n')
                continue
            if not edge.get('resolved'):
                rt = (edge.get('raw_target') or '').upper()
                if rt:
                    hclass, likelihood, signals = classify(rt, edge, counts, kind_counts, known_programs)
                    edge['program_call_likelihood'] = round(likelihood,4)
                    edge['provenance'] = {
                        'heuristic_class': hclass,
                        'signals': signals,
                        'version': 'v1'
                    }
            fout.write(json.dumps(edge)+'\n')
    print(f"Provenance augmentation complete -> {OUTPUT_FILE}")

if __name__ == '__main__':
    main()
