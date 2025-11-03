#!/usr/bin/env python3
"""Generate paragraph stub JSONL for high-priority unresolved tokens.

Inputs:
  - coverage audit JSON (from coverage_audit.py with synthetic metrics)
  - enriched edges JSONL (to derive file concentration and caller samples)

Output:
  JSONL paragraph stubs with fields:
    stub_id, name, normalized, reason, unresolved_edges, distinct_callers,
    top_files, sample_callers, synthetic_candidate, created_ts, version

Use these stubs for downstream manual validation or automated extraction refinement.

Usage:
  python generate_paragraph_stubs.py --coverage JSONL/coverage_audit_syn.json --edges JSONL/flow_edges_enriched_v2_heu_syn.jsonl --out JSONL/paragraph_stubs.jsonl --limit 200
"""
from __future__ import annotations
import json, os, argparse, time, re
from collections import Counter, defaultdict
from typing import Dict, List

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
TOKEN_RE = re.compile(r"[^A-Z0-9]+")
VERSION = "v1"

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    return TOKEN_RE.sub('-', t).strip('-')

def load_json(path: str):
    return json.load(open(path,'r',encoding='utf-8'))

def iter_jsonl(path: str):
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                yield json.loads(line)
            except Exception:
                continue

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--coverage', required=True)
    ap.add_argument('--edges', required=True)
    ap.add_argument('--out', required=True)
    ap.add_argument('--limit', type=int, default=200)
    ap.add_argument('--min-unresolved', type=int, default=50)
    args = ap.parse_args()

    coverage = load_json(args.coverage)
    priority_list = coverage.get('top_priority_unresolved', [])

    # Pre-index edges for caller & file stats per token
    callers: Dict[str, Counter] = defaultdict(Counter)
    files: Dict[str, Counter] = defaultdict(Counter)
    unresolved_counts: Dict[str,int] = defaultdict(int)

    for e in iter_jsonl(args.edges):
        raw = e.get('raw_target') or ''
        if not raw: continue
        norm = normalize_token(raw)
        if not e.get('resolved'):
            unresolved_counts[norm]+=1
            c = e.get('caller_para')
            if c:
                callers[norm][c]+=1
            fid = e.get('file_id')
            if fid:
                files[norm][fid]+=1

    created_ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    written = 0
    with open(args.out,'w',encoding='utf-8') as fout:
        for entry in priority_list:
            if written >= args.limit:
                break
            tok = entry.get('token')
            if not tok:
                continue
            if unresolved_counts.get(tok,0) < args.min_unresolved:
                continue
            c_calls = callers.get(tok,Counter())
            c_files = files.get(tok,Counter())
            stub = {
                'stub_id': f"stub-{tok}",
                'name': tok,
                'normalized': tok,
                'reason': 'high-priority-unresolved',
                'unresolved_edges': unresolved_counts.get(tok,0),
                'distinct_callers': len(c_calls),
                'top_files': c_files.most_common(8),
                'sample_callers': [k for k,_ in c_calls.most_common(10)],
                'synthetic_candidate': True,
                'created_ts': created_ts,
                'version': VERSION,
            }
            fout.write(json.dumps(stub, ensure_ascii=False) + '\n')
            written +=1
    print(f"Wrote {written} paragraph stubs to {args.out}")

if __name__ == '__main__':
    main()
