"""Analyze COBOL call extraction for duplicate call_ids and export JSONL.

Usage:
  python analyze_calls_extraction.py [--root cobol_src] [--limit N] [--jsonl calls_raw.jsonl]

Produces:
  - Summary of total extracted calls, unique call_ids, duplicate count, top duplicate examples.
  - Optional JSONL with raw extracted call records (including original call_id generation).

This helps diagnose reduced doc counts in the cobol-calls index caused by call_id collisions
(e.g., multiple CALL statements on the same line producing identical snippet + called_program).
"""
from __future__ import annotations
import os, json, argparse, collections
from ingestion_common import SourceWalker, extract_calls, stable_hash
from pathlib import Path


def extract_all_calls(root: str, limit: int | None):
    walker = SourceWalker(root)
    records = []
    for fp in walker.iter_files():
        if fp.suffix.lower() not in {'.cbl', '.cob'}:
            continue
        try:
            lines = fp.read_text(encoding='utf-8', errors='ignore').splitlines(True)
        except Exception:
            continue
        for c in extract_calls(lines, str(fp)):
            rec = {
                'call_id': c['call_id'],
                'file_path': str(fp),
                'line': c['line'],
                'col': c.get('col'),
                'occurrence': c.get('occurrence'),
                'called_program': c['called_program'],
                'snippet': c['snippet']
            }
            records.append(rec)
            if limit and len(records) >= limit:
                return records
    return records


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', default=os.environ.get('COBOL_SOURCE_ROOT') or 'cobol_src')
    ap.add_argument('--limit', type=int)
    ap.add_argument('--jsonl', default='calls_extraction_raw.jsonl')
    ap.add_argument('--no-export', action='store_true')
    args = ap.parse_args()

    if not Path(args.root).exists():
        print(f"Source root '{args.root}' not found.")
        return

    records = extract_all_calls(args.root, args.limit)
    total = len(records)
    ctr = collections.Counter(r['call_id'] for r in records)
    unique = sum(1 for k,v in ctr.items() if v >= 1)
    dup_keys = [k for k,v in ctr.items() if v > 1]
    duplicate_instances = sum(v for v in ctr.values() if v > 1)

    print(f"Total extracted calls: {total}")
    print(f"Unique call_ids:       {unique}")
    print(f"Duplicate call_ids:    {len(dup_keys)} (collapsed instances: {duplicate_instances - len(dup_keys)})")
    if dup_keys:
        print("Sample duplicates:")
        for k in dup_keys[:10]:
            subset = [r for r in records if r['call_id']==k][:3]
            print(f"  ID {k} occurrences={ctr[k]} example: {json.dumps(subset[0], ensure_ascii=False)[:140]}...")

    if not args.no_export:
        with open(args.jsonl, 'w', encoding='utf-8') as f:
            for r in records:
                f.write(json.dumps(r, ensure_ascii=False) + '\n')
        print(f"Wrote {total} records to {args.jsonl}")

    # Provide recommendation if duplicates exist
    if dup_keys:
        print("Recommendation: augment call_id with match position (column) or per-line occurrence index to remove collisions.")

if __name__ == '__main__':
    main()
