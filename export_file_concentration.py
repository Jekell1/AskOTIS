#!/usr/bin/env python3
"""Export unresolved token file concentration to CSV.

Reads an enriched edges JSONL (with resolution strategies) and produces:
  raw_token, unresolved_count, total_count, herfindahl_index, top_files (semicolon list file:count)
Optionally limit to top N unresolved raw tokens by unresolved_count.

Usage:
  python export_file_concentration.py --edges JSONL/flow_edges_enriched_v2_heu_syn.jsonl --out JSONL/file_concentration.csv --top 500
"""
from __future__ import annotations
import json, argparse, os, math, csv, re
from collections import Counter, defaultdict
from typing import Dict

NORMALIZE_PREFIXES = ["C-","P-","A-","B-","X-"]
TOKEN_RE = re.compile(r"[^A-Z0-9]+")

def normalize_token(tok: str) -> str:
    t = (tok or '').upper().strip()
    for p in NORMALIZE_PREFIXES:
        if t.startswith(p):
            t = t[len(p):]
    return TOKEN_RE.sub('-', t).strip('-')

def load_edges(path: str):
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
    ap.add_argument('--edges', required=True)
    ap.add_argument('--out', required=True)
    ap.add_argument('--top', type=int, default=1000)
    args = ap.parse_args()

    unresolved = Counter()
    total = Counter()
    file_counts = defaultdict(lambda: Counter())

    for e in load_edges(args.edges):
        raw = e.get('raw_target') or ''
        if not raw: continue
        norm = normalize_token(raw)
        total[norm]+=1
        if not e.get('resolved'):
            unresolved[norm]+=1
            fid = e.get('file_id') or ''
            if fid:
                file_counts[norm][fid]+=1

    rows = []
    for tok, ucount in unresolved.most_common(args.top):
        dist = file_counts[tok]
        tot = sum(dist.values()) or 1
        hi = sum((c/tot)**2 for c in dist.values())
        top_files = ';'.join(f"{f}:{c}" for f,c in dist.most_common(8))
        rows.append({
            'raw_token': tok,
            'unresolved_count': ucount,
            'total_count': total.get(tok, ucount),
            'herfindahl_index': round(hi,5),
            'top_files': top_files
        })

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out,'w',newline='',encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=['raw_token','unresolved_count','total_count','herfindahl_index','top_files'])
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"Wrote {len(rows)} rows to {args.out}")

if __name__ == '__main__':
    main()
