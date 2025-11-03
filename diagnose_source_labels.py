#!/usr/bin/env python3
"""Diagnose availability of real COBOL paragraph labels vs unresolved tokens.

Outputs:
  - Text report (stdout)
  - Optional JSON with:
      total_cobol_files, files_scanned, label_count, unique_labels,
      sample_labels, unresolved_tokens_tested, direct_hits, fuzzy_hits,
      per_token: { token: {direct: bool, fuzzy: [matches] } }

Fuzzy match: Levenshtein distance <= 2 (fast implementation) over label set.

Usage:
  python diagnose_source_labels.py --coverage JSONL/coverage_audit_syn.json --edges JSONL/flow_edges_enriched_v2_heu_syn.jsonl --out JSONL/source_diagnostics.json --limit-tokens 40 --max-files 300
"""
from __future__ import annotations
import os, re, json, argparse, math
from collections import Counter
from typing import List, Dict, Tuple

LABEL_RE = re.compile(r"^\s*([A-Z0-9][A-Z0-9-]{0,80})(?:\s+SECTION)?[\.:](?:\s|$)")
NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
TOKEN_RE = re.compile(r"[^A-Z0-9]+")

def normalize(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    return TOKEN_RE.sub('-', t).strip('-')

def load_json(path: str):
    return json.load(open(path,'r',encoding='utf-8'))

def levenshtein(a: str, b: str) -> int:
    if a == b: return 0
    if not a: return len(b)
    if not b: return len(a)
    prev = list(range(len(b)+1))
    for i,ca in enumerate(a,1):
        cur=[i]
        for j,cb in enumerate(b,1):
            cur.append(prev[j-1] if ca==cb else 1+min(prev[j-1], prev[j], cur[-1]))
        prev=cur
    return prev[-1]

def collect_cobol_files(root: str, max_files: int) -> List[str]:
    exts={'.cbl','.cob','.cobol'}
    files=[]
    for base,dirs,fs in os.walk(root):
        for fn in fs:
            if len(files) >= max_files: break
            if os.path.splitext(fn)[1].lower() in exts:
                files.append(os.path.join(base,fn))
        if len(files) >= max_files: break
    return files

def extract_labels(path: str) -> List[str]:
    out=[]
    try:
        with open(path,'r',encoding='utf-8',errors='ignore') as f:
            for line in f:
                m = LABEL_RE.match(line)
                if m:
                    out.append(m.group(1).upper())
    except Exception:
        pass
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--coverage', required=True)
    ap.add_argument('--edges', required=True)
    ap.add_argument('--root', default='.')
    ap.add_argument('--out')
    ap.add_argument('--limit-tokens', type=int, default=40)
    ap.add_argument('--max-files', type=int, default=300)
    args = ap.parse_args()

    coverage = load_json(args.coverage)
    unresolved_list = coverage.get('top_priority_unresolved', [])
    tokens = [e['token'] for e in unresolved_list][:args.limit_tokens]

    cobol_files = collect_cobol_files(args.root, args.max_files)
    label_counter = Counter()
    for p in cobol_files:
        labels = extract_labels(p)
        for lb in labels:
            label_counter[lb]+=1

    unique_labels = set(label_counter.keys())

    token_results = {}
    direct_hits = 0
    fuzzy_hits = 0
    label_list = list(unique_labels)

    for tok in tokens:
        res = {'direct': False, 'fuzzy': []}
        if tok in unique_labels:
            res['direct'] = True
            direct_hits += 1
        else:
            # fuzzy pass (limit to maybe 2000 labels for speed if huge)
            candidates = label_list if len(label_list) <= 2000 else label_list[:2000]
            fuzzy = []
            for lb in candidates:
                if abs(len(lb)-len(tok)) > 2:
                    continue
                if levenshtein(lb, tok) <= 2:
                    fuzzy.append(lb)
                if len(fuzzy) >= 10:
                    break
            if fuzzy:
                res['fuzzy'] = fuzzy
                fuzzy_hits += 1
        token_results[tok] = res

    report = {
        'total_cobol_files': len(cobol_files),
        'files_scanned': len(cobol_files),
        'label_count': sum(label_counter.values()),
        'unique_labels': len(unique_labels),
        'sample_labels': list(label_counter.keys())[:25],
        'unresolved_tokens_tested': len(tokens),
        'direct_hits': direct_hits,
        'fuzzy_hit_tokens': fuzzy_hits,
        'tokens': token_results,
    }

    # Text report
    print("=== Source Label Diagnostics ===")
    print(f"COBOL files scanned: {report['files_scanned']}")
    print(f"Unique labels detected: {report['unique_labels']} (total occurrences: {report['label_count']})")
    print(f"Tested unresolved tokens: {report['unresolved_tokens_tested']} | Direct hits: {direct_hits} | Fuzzy-hit tokens: {fuzzy_hits}")
    if direct_hits == 0:
        print("No direct matches found for top unresolved tokens — suggests tokens are absent from raw sources (copy-level, generated, or different naming).")
    elif direct_hits < 5:
        print("Few direct matches — partial coverage; consider expanding search radius or preprocessing copybooks.")
    else:
        print("Some direct matches found — proceed to integrate recovered labels into paragraph set.")

    if args.out:
        with open(args.out,'w',encoding='utf-8') as f:
            json.dump(report,f,indent=2)
        print(f"Wrote diagnostics JSON to {args.out}")

if __name__ == '__main__':
    main()
