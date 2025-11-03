"""Augment unresolved edges by leveraging copybook labels.

Inputs:
 - JSONL/flow_edges_enriched_v2_real.jsonl (large) OR environment variable EDGE_FILE override
 - JSONL/copybook_labels.jsonl (from index_copybook_labels.py)
 - JSONL/copy_inclusions.jsonl (from build_copy_inclusion_map.py)

Approach:
 1. Build map: program_file -> set of normalized copybook names it COPYs.
 2. Build map: copybook name -> set of labels (uppercased) available.
 3. Stream edges; for each unresolved edge where target_program_id is null and kind contains 'perform' or 'goto' or 'call':
       - If raw_target upper() matches a copybook label reachable via included copybooks for that file, resolve.
       - Record resolution_strategy 'copybook-label'.
       - confidence = 0.75 (lower than direct paragraph recovery but higher than synthetic 0.5 default).
       - Mark resolved_target_para = raw_target.
       - Add field copybook_resolution = {"copybooks": [...matching copybooks...]}
 4. Write augmented edges JSONL/flow_edges_enriched_v2_copybook.jsonl preserving order.

Performance: copybook label set is large; use hash sets. We only load labels for copybooks that appear in inclusions.
"""

from __future__ import annotations

import os
import json
from pathlib import Path
from collections import defaultdict

EDGE_FILE = Path(os.environ.get("EDGE_FILE", "JSONL/flow_edges_enriched_v2_real.jsonl"))
COPYBOOK_LABELS = Path("JSONL/copybook_labels.jsonl")
COPY_INCLUSIONS = Path("JSONL/copy_inclusions.jsonl")
OUTPUT_FILE = Path("JSONL/flow_edges_enriched_v2_copybook.jsonl")

def load_copy_inclusions():
    prog_to_copy = defaultdict(set)
    if not COPY_INCLUSIONS.exists():
        print("WARN: copy inclusions file missing")
        return prog_to_copy
    with COPY_INCLUSIONS.open('r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            prog_file = obj.get('program_file')
            norm = obj.get('normalized_copybook')
            if prog_file and norm:
                prog_to_copy[prog_file.upper()].add(norm.upper())
    return prog_to_copy

def load_relevant_copybook_labels(needed_copybooks: set[str]):
    copybook_to_labels = defaultdict(set)
    if not COPYBOOK_LABELS.exists():
        print("WARN: copybook labels file missing")
        return copybook_to_labels
    with COPYBOOK_LABELS.open('r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            cb_file = obj.get('copybook_file')
            label = obj.get('label')
            if not cb_file or not label:
                continue
            norm_cb = cb_file.upper().removesuffix('.CPY')
            if norm_cb not in needed_copybooks:
                continue
            copybook_to_labels[norm_cb].add(label.upper())
    return copybook_to_labels

def main():
    if not EDGE_FILE.exists():
        print(f"Edge file not found: {EDGE_FILE}")
        return
    prog_to_copy = load_copy_inclusions()
    all_needed = set()
    for s in prog_to_copy.values():
        all_needed.update(s)
    copybook_to_labels = load_relevant_copybook_labels(all_needed)

    print(f"Programs with COPYs: {len(prog_to_copy)}; distinct copybooks needed: {len(all_needed)}")
    print(f"Loaded labels for {len(copybook_to_labels)} copybooks")

    resolved_count = 0
    total_unresolved_seen = 0
    with EDGE_FILE.open('r', encoding='utf-8') as fin, OUTPUT_FILE.open('w', encoding='utf-8') as fout:
        for line in fin:
            line = line.rstrip('\n')
            if not line:
                continue
            try:
                edge = json.loads(line)
            except json.JSONDecodeError:
                fout.write(line + '\n')
                continue
            if not edge.get('resolved'):
                # candidate
                total_unresolved_seen += 1
                raw_target = (edge.get('raw_target') or '').upper()
                file_id = edge.get('file_id')  # program file base name? earlier pipeline used file_id as program id.
                program_file = f"{file_id}.CBL" if file_id else None
                copy_sets = prog_to_copy.get((program_file or '').upper())
                if copy_sets and raw_target:
                    matching_copybooks = [cb for cb in copy_sets if raw_target in copybook_to_labels.get(cb, set())]
                    if matching_copybooks:
                        edge['resolved'] = True
                        edge['resolved_target_para'] = edge.get('resolved_target_para') or edge.get('raw_target')
                        edge['confidence'] = max(edge.get('confidence', 0), 0.75)
                        edge['resolution_strategy'] = 'copybook-label'
                        edge['synthetic_target'] = False
                        edge['copybook_resolution'] = {'copybooks': matching_copybooks}
                        resolved_count += 1
            fout.write(json.dumps(edge) + '\n')

    print(f"Augmentation complete. Resolved {resolved_count} previously unresolved edges (out of {total_unresolved_seen} candidates encountered). Output: {OUTPUT_FILE}")

if __name__ == '__main__':
    main()
