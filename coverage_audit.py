#!/usr/bin/env python3
"""Coverage audit for COBOL paragraph & flow edge data.

Goals:
 1. Paragraph definition presence vs. referenced raw targets in flow edges.
 2. Resolution gap categorization (why unresolved: no-candidate, multi-ambiguous, low-strategy, token anomalies).
 3. Token anomaly detection (very short tokens, mostly digits, single-letter, high fan-in raw targets).
 4. File / program concentration: which files contain majority of unresolved references to the same raw target.
 5. Suggested remediation priority list (score raw targets by unresolved frequency * rarity / (1+paragraph_defs)).

Inputs (defaults):
  JSONL/flow_edges_enriched.jsonl
  JSONL/paragraphs.jsonl
  JSONL/aliases_full.jsonl (optional for family linkage)

Outputs:
  - Stdout textual report
  - Optional JSON (--out) with structured metrics

Usage:
  python coverage_audit.py --edges JSONL/flow_edges_enriched.jsonl --paragraphs JSONL/paragraphs.jsonl --out JSONL/coverage_audit.json
"""
from __future__ import annotations
import json, os, re, argparse, statistics
from collections import Counter, defaultdict
from typing import Dict, List, Tuple, Optional

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
TOKEN_RE = re.compile(r"[^A-Z0-9]+")

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    t = TOKEN_RE.sub('-', t).strip('-')
    return t

def load_jsonl(path: str):
    items = []
    if not os.path.exists(path):
        return items
    with open(path,'r',encoding='utf-8') as f:
        for line in f:
            line=line.strip()
            if not line: continue
            try:
                items.append(json.loads(line))
            except Exception:
                continue
    return items

def audit(edges_path: str, paragraphs_path: str, aliases_path: Optional[str], top: int = 40):
    edges = load_jsonl(edges_path)
    paras = load_jsonl(paragraphs_path)
    aliases = load_jsonl(aliases_path) if aliases_path else []

    para_by_norm = defaultdict(list)
    for p in paras:
        name = p.get('name') or p.get('para_name')
        if not name: continue
        n = normalize_token(name)
        para_by_norm[n].append(p)

    alias_norms = {a.get('alias'): a for a in aliases if a.get('alias')}

    unresolved_counter = Counter()
    resolved_counter = Counter()
    synthetic_resolved = 0
    genuine_resolved = 0
    strategy_counter = Counter()
    raw_target_occurs = Counter()
    file_occurs = defaultdict(lambda: Counter())  # raw_token -> file_id -> count

    token_flags = defaultdict(set)  # raw_token -> set(flags)

    for e in edges:
        raw = e.get('raw_target') or ''
        if not raw: continue
        norm = normalize_token(raw)
        raw_target_occurs[norm]+=1
        file_occurs[norm][e.get('file_id')] +=1
        strat = e.get('resolution_strategy') or 'none'
        strategy_counter[strat]+=1
        if e.get('resolved'):
            resolved_counter[norm]+=1
            if e.get('synthetic_target'):
                synthetic_resolved += 1
            else:
                genuine_resolved += 1
        else:
            unresolved_counter[norm]+=1

    # Characterize tokens
    for tok,count in raw_target_occurs.items():
        if len(tok) <= 2:
            token_flags[tok].add('very-short')
        if tok.isdigit():
            token_flags[tok].add('all-digits')
        if sum(c.isdigit() for c in tok) / max(1,len(tok)) > 0.6:
            token_flags[tok].add('mostly-digits')
        if tok.count('-') >= 4:
            token_flags[tok].add('hyphen-heavy')
        if tok in alias_norms:
            token_flags[tok].add('has-alias')
        if tok not in para_by_norm:
            token_flags[tok].add('no-para-def')
        else:
            if len(para_by_norm[tok]) > 3:
                token_flags[tok].add('multi-para')

    total_edges = len(edges)
    total_unresolved_edges = sum(unresolved_counter.values())
    total_resolved_edges = sum(resolved_counter.values())
    unresolved_rate = (total_unresolved_edges / total_edges * 100) if total_edges else 0.0

    # Priority scoring: unresolved_freq * (1 + (flags that indicate anomaly)) / (1 + para_defs)
    priority_scores = []
    for tok,u_count in unresolved_counter.items():
        para_defs = len(para_by_norm.get(tok,[]))
        flag_bonus = len(token_flags[tok] - {'has-alias'})  # exclude has-alias from penalty
        score = u_count * (1 + 0.15*flag_bonus) / (1 + para_defs)
        priority_scores.append((tok, score, u_count, para_defs, sorted(token_flags[tok])))
    priority_scores.sort(key=lambda x: x[1], reverse=True)

    top_priority = priority_scores[:top]

    # File concentration: compute Herfindahl index per top token
    file_concentration = {}
    for tok, *_ in top_priority:
        dist = file_occurs[tok]
        total = sum(dist.values())
        if not total:
            continue
        hi = sum((c/total)**2 for c in dist.values())  # higher means concentrated
        top_files = dist.most_common(5)
        file_concentration[tok] = {
            'herfindahl': round(hi,4),
            'top_files': top_files
        }

    summary = {
        'total_edges': total_edges,
        'resolved_edges': total_resolved_edges,
        'unresolved_edges': total_unresolved_edges,
        'unresolved_rate_pct': round(unresolved_rate,3),
        'unique_raw_tokens': len(raw_target_occurs),
        'unique_unresolved_raw_tokens': len(unresolved_counter),
        'tokens_missing_para_defs': len([t for t in raw_target_occurs if 'no-para-def' in token_flags[t]]),
        'tokens_with_alias': len([t for t in raw_target_occurs if 'has-alias' in token_flags[t]]),
        'strategy_counts': dict(strategy_counter.most_common()),
        'synthetic_resolved_edges': synthetic_resolved,
        'genuine_resolved_edges': genuine_resolved,
        'synthetic_share_pct_of_resolved': round((synthetic_resolved / total_resolved_edges * 100),2) if total_resolved_edges else 0.0,
        'top_priority_unresolved': [
            {
                'token': tok,
                'score': round(score,2),
                'unresolved_occurs': u_count,
                'para_defs': para_defs,
                'flags': flags,
                'file_concentration': file_concentration.get(tok)
            }
            for tok,score,u_count,para_defs,flags in top_priority
        ],
    }
    return summary


def print_report(summary: dict):
    print("=== Coverage Audit Report ===")
    print(f"Total edges: {summary['total_edges']}  Resolved={summary['resolved_edges']}  Unresolved={summary['unresolved_edges']} ({summary['unresolved_rate_pct']}%)")
    print(f"Unique raw tokens: {summary['unique_raw_tokens']}  Unresolved tokens: {summary['unique_unresolved_raw_tokens']}")
    print(f"Tokens missing paragraph defs: {summary['tokens_missing_para_defs']}  Tokens with alias: {summary['tokens_with_alias']}")
    if 'synthetic_resolved_edges' in summary:
        print(f"Resolved breakdown: synthetic={summary['synthetic_resolved_edges']} genuine={summary['genuine_resolved_edges']} synthetic_share={summary['synthetic_share_pct_of_resolved']}%")
    print("Strategy counts:")
    for k,v in summary['strategy_counts'].items():
        print(f"  {k:15s} {v}")
    print("\nTop priority unresolved tokens:")
    for item in summary['top_priority_unresolved']:
        fc = item.get('file_concentration') or {}
        top_files = ', '.join(f"{f}:{c}" for f,c in (fc.get('top_files') or []))
        print(f"  {item['token']:35s} score={item['score']:8.2f} unresolved={item['unresolved_occurs']:7d} para_defs={item['para_defs']:2d} flags={','.join(item['flags'])} HI={fc.get('herfindahl')} files=[{top_files}]")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--edges', default='JSONL/flow_edges_enriched.jsonl')
    ap.add_argument('--paragraphs', default='JSONL/paragraphs.jsonl')
    ap.add_argument('--aliases', default='JSONL/aliases_full.jsonl')
    ap.add_argument('--top', type=int, default=40)
    ap.add_argument('--out')
    args = ap.parse_args()
    summary = audit(args.edges, args.paragraphs, args.aliases, top=args.top)
    print_report(summary)
    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump(summary,f,indent=2)
        print(f"Wrote {args.out}")

if __name__ == '__main__':
    main()
