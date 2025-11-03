#!/usr/bin/env python3
"""Generate routine summary documents from enriched flow edges + optional paragraphs + aliases.

Outputs one JSONL line per routine token (normalized raw_target or alias canonical).
This is scaffolding for the future cobol-summaries index.

Heuristics:
  - Routine key = normalized token.
  - Aggregate counts: total_edges, resolved_edges, synthetic_edges, unresolved_edges.
  - Distinct callers and (if resolved) distinct targets.
  - Distinct files referencing token.
  - Synthetic dominance ratio used to compute priority (prefer high unresolved + high fan-in + low genuine anchors).
  - Priority score = (distinct_callers + distinct_files + log1p(total_edges)) * (1 + unresolved_edges/(total_edges+1)) * (1 + 0.5*(1 - genuine_ratio)).

Fields written:
  summary_id, routine_key, family_key, has_alias, total_edges, resolved_edges, synthetic_edges,
  unresolved_edges, distinct_callers, distinct_targets, distinct_files, genuine_ratio,
  synthetic_ratio, priority_score, top_callers (list), sample_targets (list), created_ts, version.

Usage:
  python build_routine_summaries.py --edges JSONL/flow_edges_enriched_v2_heu_syn.jsonl --aliases JSONL/aliases_full.jsonl --out JSONL/routine_summaries.jsonl --top-callers 10 --sample-targets 10
"""
from __future__ import annotations
import json, os, argparse, math, time, re
from collections import defaultdict, Counter
from typing import Dict, List, Optional

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
TOKEN_RE = re.compile(r"[^A-Z0-9]+")
VERSION = "v1"

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    return TOKEN_RE.sub('-', t).strip('-')

def load_jsonl(path: str):
    if not path or not os.path.exists(path):
        return []
    out=[]
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line:
                continue
            try:
                out.append(json.loads(line))
            except Exception:
                continue
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', required=True)
    ap.add_argument('--aliases')
    ap.add_argument('--out', required=True)
    ap.add_argument('--min-occurs', type=int, default=5, help='Minimum total edges referencing token to include')
    ap.add_argument('--top-callers', type=int, default=10)
    ap.add_argument('--sample-targets', type=int, default=10)
    args = ap.parse_args()

    edges = load_jsonl(args.edges)
    aliases = load_jsonl(args.aliases) if args.aliases else []

    alias_map = {normalize_token(a.get('alias')): a for a in aliases if a.get('alias')}

    # Aggregations keyed by normalized raw_target token (and also by resolved_target when available)
    agg: Dict[str, Dict] = defaultdict(lambda: {
        'total':0,'resolved':0,'synthetic':0,'unresolved':0,
        'callers':Counter(),'targets':Counter(),'files':Counter(), 'family_key':None,'has_alias':False
    })

    for e in edges:
        raw = e.get('raw_target') or ''
        if not raw:
            continue
        norm = normalize_token(raw)
        a = agg[norm]
        a['total'] +=1
        caller = e.get('caller_para')
        if caller:
            a['callers'][caller]+=1
        fid = e.get('file_id')
        if fid:
            a['files'][fid]+=1
        if e.get('resolved'):
            a['resolved'] +=1
            if e.get('synthetic_target'):
                a['synthetic']+=1
            tgt = e.get('resolved_target_para')
            if tgt:
                a['targets'][tgt]+=1
        else:
            a['unresolved'] +=1
        # alias / family info
        if norm in alias_map:
            a['has_alias'] = True
            if not a['family_key']:
                a['family_key'] = alias_map[norm].get('family_key')

    created_ts = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    written = 0
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out,'w',encoding='utf-8') as fout:
        for norm, data in agg.items():
            if data['total'] < args.min_occurs:
                continue
            resolved = data['resolved']
            synthetic = data['synthetic']
            genuine = resolved - synthetic
            total = data['total']
            unresolved = data['unresolved']
            genuine_ratio = genuine / resolved if resolved else 0.0
            synthetic_ratio = synthetic / resolved if resolved else 0.0
            # Priority score components
            distinct_callers = len(data['callers'])
            distinct_targets = len(data['targets'])
            distinct_files = len(data['files'])
            unresolved_factor = 1 + (unresolved / (total+1))
            synthetic_penalty = 1 + 0.5*(synthetic_ratio)  # more synthetic increases weight toward extraction attention
            base = (distinct_callers + distinct_files + math.log1p(total))
            priority = base * unresolved_factor * synthetic_penalty

            doc = {
                'summary_id': f"{norm}",
                'routine_key': norm,
                'family_key': data['family_key'],
                'has_alias': data['has_alias'],
                'total_edges': total,
                'resolved_edges': resolved,
                'synthetic_edges': synthetic,
                'unresolved_edges': unresolved,
                'distinct_callers': distinct_callers,
                'distinct_targets': distinct_targets,
                'distinct_files': distinct_files,
                'genuine_ratio': round(genuine_ratio,4),
                'synthetic_ratio': round(synthetic_ratio,4),
                'priority_score': round(priority,2),
                'top_callers': [k for k,_ in data['callers'].most_common(args.top_callers)],
                'sample_targets': [k for k,_ in data['targets'].most_common(args.sample_targets)],
                'created_ts': created_ts,
                'version': VERSION,
            }
            fout.write(json.dumps(doc, ensure_ascii=False) + '\n')
            written +=1
    print(f"Wrote {written} summary docs to {args.out}")

if __name__ == '__main__':
    main()
