#!/usr/bin/env python3
"""Compare flat vs nested blob copies for chunks & symbols to detect missing ingestion.

Outputs counts of lines and unique IDs for:
  chunks.jsonl (flat) vs chunks/chunks.jsonl (nested)
  data_items.jsonl (flat) vs symbols/data_items.jsonl (nested)

Usage:
  python inspect_chunk_symbol_sources.py --container aisearch --prefix S35-Source/
"""
import argparse, json, sys
from collections import Counter
from azure.storage.blob import BlobServiceClient

DATASETS = [
    ('chunks','chunks.jsonl','chunks/chunks.jsonl','chunk_id'),
    ('symbols','data_items.jsonl','symbols/data_items.jsonl','item_id'),
]

def load_settings():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    return vals['AzureWebJobsStorage']

def read_blob_text(cc, path):
    try:
        return cc.get_blob_client(path).download_blob().readall().decode('utf-8','replace')
    except Exception:
        return None

def analyze(text, key_field):
    if text is None: return None
    total=0; uniq=Counter(); missing_key=0; invalid=0
    for ln in text.splitlines():
        if not ln.strip(): continue
        total+=1
        try:
            obj=json.loads(ln)
        except Exception:
            invalid+=1; continue
        kid=obj.get(key_field)
        if kid is None:
            missing_key+=1; continue
        uniq[kid]+=1
    return {
        'lines': total,
        'unique': len(uniq),
        'dupeLines': sum(c-1 for c in uniq.values() if c>1),
        'missingKey': missing_key,
        'invalid': invalid
    }

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--container',required=True)
    ap.add_argument('--prefix',required=True)
    args=ap.parse_args()
    conn=load_settings()
    bsc=BlobServiceClient.from_connection_string(conn)
    cc=bsc.get_container_client(args.container)
    base=f"{args.prefix.rstrip('/')}/JSONL"
    print(f"Comparing flat vs nested under {base}\n")
    print(f"{'dataset':<8} {'variant':<7} {'lines':>10} {'unique':>10} {'dupeLines':>10} {'missingKey':>11} {'invalid':>8}")
    for ds, flat_rel, nested_rel, key_field in DATASETS:
        flat_path=f"{base}/{flat_rel}"
        nested_path=f"{base}/{nested_rel}"
        flat_stats=analyze(read_blob_text(cc,flat_path), key_field)
        nested_stats=analyze(read_blob_text(cc,nested_path), key_field)
        for variant, stats in [('flat',flat_stats),('nested',nested_stats)]:
            if stats is None:
                print(f"{ds:<8} {variant:<7} {'MISSING':>10}")
            else:
                print(f"{ds:<8} {variant:<7} {stats['lines']:10} {stats['unique']:10} {stats['dupeLines']:10} {stats['missingKey']:11} {stats['invalid']:8}")
        # quick diff commentary
        if flat_stats and nested_stats:
            if flat_stats['unique'] != nested_stats['unique']:
                print(f"  -> Unique key mismatch (flat {flat_stats['unique']} vs nested {nested_stats['unique']})")
            if flat_stats['lines'] != nested_stats['lines']:
                print(f"  -> Line count mismatch (flat {flat_stats['lines']} vs nested {nested_stats['lines']})")
    print('\nInterpretation: If nested unique << flat unique, current data source using nested lost entities. Copy flat to nested to align.')

if __name__=='__main__':
    import json
    main()
