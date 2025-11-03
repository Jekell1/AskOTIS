"""Generate placeholder documents for likely external program calls.

Input: JSONL/flow_edges_enriched_v2_prov.jsonl (with provenance classification)
Output: JSONL/external_program_placeholders.jsonl with synthesized docs:
{
  "program_id": str,                 # inferred external program id
  "occurrences": int,                # number of edges pointing to it
  "calling_files": [str],            # list of file_ids invoking it
  "example_edges": [edge_id,...],    # sample up to N edge ids
  "heuristic": str,                  # classification source
  "generated_strategy": "heuristic-v1",
  "created_ts": iso8601
}

We include only unresolved edges where provenance.heuristic_class == 'likely_program_call'.
Short allcaps tokens or tokens without hyphen treated as program names as-is.

These placeholder docs can be indexed to allow chatbot retrieval explaining
that the program is referenced but implementation not present in corpus.
"""

from __future__ import annotations

import json
from pathlib import Path
from collections import defaultdict
from datetime import datetime, timezone

EDGE_FILE = Path('JSONL/flow_edges_enriched_v2_prov.jsonl')
OUTPUT_FILE = Path('JSONL/external_program_placeholders.jsonl')
OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)

MAX_EXAMPLES = 5

def main():
    agg = {}
    if not EDGE_FILE.exists():
        print(f"Edge file missing: {EDGE_FILE}")
        return
    with EDGE_FILE.open('r', encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                e=json.loads(line)
            except json.JSONDecodeError:
                continue
            if e.get('resolved'):
                continue
            prov = e.get('provenance') or {}
            if prov.get('heuristic_class') != 'likely_program_call':
                continue
            target = (e.get('raw_target') or '').upper()
            if not target:
                continue
            entry = agg.setdefault(target, {
                'program_id': target,
                'occurrences': 0,
                'calling_files': set(),
                'example_edges': [],
                'heuristic': 'likely_program_call',
            })
            entry['occurrences'] += 1
            file_id = e.get('file_id')
            if file_id:
                entry['calling_files'].add(file_id)
            if len(entry['example_edges']) < MAX_EXAMPLES:
                edge_id = e.get('edge_id')
                if edge_id:
                    entry['example_edges'].append(edge_id)
    if not agg:
        print("No likely external program calls detected.")
        return
    ts = datetime.now(timezone.utc).isoformat()
    count = 0
    with OUTPUT_FILE.open('w', encoding='utf-8') as out:
        for program_id, data in sorted(agg.items(), key=lambda kv: kv[1]['occurrences'], reverse=True):
            doc = {
                'program_id': program_id,
                'occurrences': data['occurrences'],
                'calling_files': sorted(data['calling_files']),
                'example_edges': data['example_edges'],
                'heuristic': data['heuristic'],
                'generated_strategy': 'heuristic-v1',
                'created_ts': ts,
            }
            out.write(json.dumps(doc)+'\n')
            count += 1
    print(f"Wrote {count} placeholder program docs -> {OUTPUT_FILE}")

if __name__ == '__main__':
    main()
