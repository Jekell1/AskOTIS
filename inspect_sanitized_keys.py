#!/usr/bin/env python3
"""
inspect_sanitized_keys.py
-------------------------
Download first N lines of each sanitized JSONL blob in subfolders (chunks, symbols, xrefs, calls)
and print the key field values to verify that disallowed characters (e.g., ':', '$') were replaced.

Usage:
  python inspect_sanitized_keys.py --container aisearch --prefix S35-Source/ --lines 3
"""
import argparse, json
from azure.storage.blob import BlobServiceClient

MAPPING = {
    'chunks': ('chunks.jsonl','chunk_id'),
    'symbols': ('data_items.jsonl','item_id'),
    'xrefs': ('xrefs.jsonl','xref_id'),
    'calls': ('calls.jsonl','call_id'),
}

def head_lines(text, n):
    out = []
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        out.append(line)
        if len(out) >= n:
            break
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--container', required=True)
    ap.add_argument('--prefix', required=True)
    ap.add_argument('--lines', type=int, default=3)
    args = ap.parse_args()

    base_prefix = f"{args.prefix.rstrip('/')}/JSONL"
    bsc = BlobServiceClient.from_connection_string(json.load(open('local.settings.json'))['Values']['AzureWebJobsStorage'])
    cc = bsc.get_container_client(args.container)

    for folder,(fname,key_field) in MAPPING.items():
        path = f"{base_prefix}/{folder}/{fname}"
        try:
            blob = cc.get_blob_client(path)
            data = blob.download_blob().readall().decode('utf-8','replace')
        except Exception as e:
            print(f"❌ {path} -> {e}")
            continue
        lines = head_lines(data, args.lines)
        print(f"\n=== {path} (showing {len(lines)} lines) ===")
        for i,l in enumerate(lines,1):
            try:
                obj = json.loads(l)
                k = obj.get(key_field)
                print(f"{i}. {key_field}={k}")
            except Exception as e:
                print(f"{i}. (parse error) {e}\n   raw: {l[:120]}")

if __name__=='__main__':
    main()
