#!/usr/bin/env python3
"""Analyze uniqueness of key fields in source JSONL blobs vs index counts.

Focuses on datasets with large line->index deltas: chunks, symbols, paragraphs, xrefs.
For each dataset folder under <prefix>/JSONL/<dataset>/ :
  - Iterate all *.jsonl blobs (skips manifest)
  - Parse each non-empty line as JSON
  - Extract key field (per dataset mapping)
  - Count total lines, unique keys, duplicates, and keys with >1 occurrences
  - Optionally show top N most duplicated keys

Usage:
  python analyze_source_uniqueness.py --container aisearch --prefix S35-Source/ --top 10
"""
import argparse, json, sys
from collections import Counter, defaultdict
from azure.storage.blob import BlobServiceClient
import requests

API_VERSION = "2024-07-01"
TARGET_DATASETS = {
    'chunks': ('chunks', 'chunk_id'),
    'symbols': ('symbols', 'item_id'),
    'paragraphs': ('paragraphs', 'para_id'),
    'xrefs': ('xrefs', 'xref_id'),
}
INDEX_NAMES = {
    'chunks': 'code-chunks',
    'symbols': 'cobol-symbols',
    'paragraphs': 'cobol-paragraphs',
    'xrefs': 'cobol-xrefs',
}


def load_settings():
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    ep = vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')
    key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    storage = vals['AzureWebJobsStorage']
    if not (ep and key and storage):
        print('Missing settings values', file=sys.stderr); sys.exit(2)
    return ep.rstrip('/'), key, storage


def get_index_count(ep, key, index):
    url = f"{ep}/indexes/{index}/docs?api-version={API_VERSION}&$count=true&$top=0"
    r = requests.get(url, headers={'api-key': key}, timeout=60)
    try:
        return r.json().get('@odata.count')
    except Exception:
        return None


def analyze_dataset(cc, base_prefix, folder, key_field, topn):
    prefix = f"{base_prefix}/{folder}/"
    blobs = list(cc.list_blobs(name_starts_with=prefix))
    total_lines = 0
    key_counter = Counter()
    invalid = 0
    for b in blobs:
        name = b.name
        if not name.endswith('.jsonl'): continue
        if 'manifest' in name:  # skip any manifests
            continue
        data = cc.get_blob_client(name).download_blob().readall().decode('utf-8', errors='replace')
        for ln in data.splitlines():
            if not ln.strip():
                continue
            total_lines += 1
            try:
                obj = json.loads(ln)
            except Exception:
                invalid += 1
                continue
            kid = obj.get(key_field)
            if kid is None:
                invalid += 1
                continue
            key_counter[kid] += 1
    unique = len(key_counter)
    dups = sum(c-1 for c in key_counter.values() if c > 1)
    multi_keys = sum(1 for c in key_counter.values() if c > 1)
    top = key_counter.most_common(topn)
    return {
        'total_lines': total_lines,
        'unique_keys': unique,
        'duplicate_lines': dups,
        'keys_with_duplicates': multi_keys,
        'invalid_lines': invalid,
        'top_duplicates': [(k,c) for k,c in top if c>1]
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--container', required=True)
    ap.add_argument('--prefix', required=True)
    ap.add_argument('--top', type=int, default=5, help='Show top N duplicated keys')
    args = ap.parse_args()

    ep, skey, storage = load_settings()
    bsc = BlobServiceClient.from_connection_string(storage)
    cc = bsc.get_container_client(args.container)
    base = f"{args.prefix.rstrip('/')}/JSONL"

    print(f"Analyzing uniqueness under {base}")
    print(f"{'dataset':<12} {'srcLines':>10} {'unique':>10} {'dupeLines':>10} {'dupeKeys':>9} {'invalid':>8} {'indexCount':>11} {'lossExplained%':>14}")
    for ds,(folder,key_field) in TARGET_DATASETS.items():
        stats = analyze_dataset(cc, base, folder, key_field, args.top)
        idx_count = get_index_count(ep,skey,INDEX_NAMES[ds])
        src = stats['total_lines']
        uniq = stats['unique_keys']
        dupe_lines = stats['duplicate_lines']
        invalid = stats['invalid_lines']
        loss_explained = ''
        if src and idx_count is not None:
            # If duplicates + invalid account for the gap to index count, show percentage
            gap = src - idx_count
            explained = dupe_lines + invalid - max(0, uniq - idx_count)
            if gap>0:
                pct = (min(gap, dupe_lines+invalid)/gap)*100
                loss_explained = f"{pct:5.1f}"
        print(f"{ds:<12} {src:10} {uniq:10} {dupe_lines:10} {stats['keys_with_duplicates']:9} {invalid:8} {str(idx_count):>11} {loss_explained:>14}")
        if stats['top_duplicates']:
            print(f"  topDupKeys: {stats['top_duplicates'][:args.top]}")
    print('\nInterpretation: dupeLines are extra lines sharing an existing key (index keeps only one). Invalid lines lack usable JSON or key field. If gap roughly equals dupeLines+invalid, disparity is explained by key collisions and invalids.')

if __name__ == '__main__':
    main()
