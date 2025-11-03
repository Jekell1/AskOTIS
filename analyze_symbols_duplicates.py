#!/usr/bin/env python3
"""Analyze the symbols JSONL source blob (downloaded locally) for duplicate item_id keys.

Usage:
  python analyze_symbols_duplicates.py symbols_data_items.jsonl

Outputs:
  * Total non-empty JSONL lines
  * Unique item_id count
  * Number of duplicate item_id values (ids with count > 1)
  * Total surplus rows from duplicates (sum(count-1))
  * Top 15 duplicate item_ids with their counts
  * Simple heuristic explaining expected Azure Search final doc count (unique keys)

This helps reconcile: uploaded_docs vs final_index_count where duplicates overwrite prior docs (same key).
"""
import sys, json, collections, os, math
from typing import Tuple

def analyze(path: str) -> Tuple[int,int,int,int,collections.Counter]:
    if not os.path.exists(path):
        raise SystemExit(f"File not found: {path}")
    line_count = 0
    ids = collections.Counter()
    bad_json = 0
    with open(path,'r',encoding='utf-8') as f:
        for raw in f:
            raw = raw.strip()
            if not raw:
                continue
            line_count += 1
            try:
                obj = json.loads(raw)
            except Exception:
                bad_json += 1
                continue
            iid = obj.get('item_id')
            if iid is not None:
                ids[iid] += 1
    unique = len(ids)
    dup_id_count = sum(1 for k,v in ids.items() if v>1)
    surplus_rows = sum(v-1 for v in ids.values() if v>1)
    return line_count, unique, dup_id_count, surplus_rows, ids

def main():
    if len(sys.argv) < 2:
        print("Usage: python analyze_symbols_duplicates.py <symbols_data_items.jsonl>")
        return
    path = sys.argv[1]
    line_count, unique, dup_id_ct, surplus_rows, ids = analyze(path)
    print(f"File: {path}")
    print(f"Lines (non-empty JSON parsed rows): {line_count}")
    print(f"Unique item_id keys:              {unique}")
    print(f"Item_ids with duplicates:         {dup_id_ct}")
    print(f"Surplus rows from duplicates:     {surplus_rows}")
    if line_count:
        pct = surplus_rows/line_count*100
        print(f"Duplicate row percentage:         {pct:.2f}%")
    print()
    # Top duplicates
    dup_samples = [ (k,v) for k,v in ids.items() if v>1 ]
    dup_samples.sort(key=lambda kv: kv[1], reverse=True)
    print("Top 15 duplicate item_ids (key -> count):")
    for k,v in dup_samples[:15]:
        print(f"  {k} -> {v}")
    print()
    expected_final = unique
    print("Reconciliation:")
    print("- Documents uploaded (lines):     ", line_count)
    print("- Final expected index docs:      ", expected_final)
    print("- Difference (duplicate overwrites):", line_count - expected_final)
    print("If the Azure Search index count matches ~unique keys, the discrepancy is fully explained by overwrites.")

if __name__ == '__main__':
    main()
