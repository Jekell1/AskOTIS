#!/usr/bin/env python3
"""Ensure flat JSONL dataset files are copied into per-dataset subfolders.
Currently needed for datasets: chunks -> chunks/chunks.jsonl, symbols -> symbols/data_items.jsonl
Usage:
  python ensure_nested_files.py --container aisearch --prefix S35-Source/
"""
import argparse, json, sys
from azure.storage.blob import BlobServiceClient

def load_settings():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    storage=vals['AzureWebJobsStorage']
    return storage

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--container',required=True)
    ap.add_argument('--prefix',required=True)
    ap.add_argument('--dry',action='store_true')
    args=ap.parse_args()
    conn=load_settings()
    bsc=BlobServiceClient.from_connection_string(conn)
    cc=bsc.get_container_client(args.container)
    base=f"{args.prefix.rstrip('/')}/JSONL"
    datasets={
        'chunks': ('chunks.jsonl','chunks/chunks.jsonl'),
        'symbols': ('data_items.jsonl','symbols/data_items.jsonl'),
    }
    existing={b.name for b in cc.list_blobs(name_starts_with=base+'/')}
    actions=0
    for name,(flat_rel,nested_rel) in datasets.items():
        flat=f"{base}/{flat_rel}"
        nested=f"{base}/{nested_rel}"
        if nested in existing:
            print(f"{name}: nested exists")
            continue
        if flat not in existing:
            print(f"{name}: flat source missing ({flat_rel})")
            continue
        print(f"{name}: copying {flat_rel} -> {nested_rel}")
        if not args.dry:
            data=cc.get_blob_client(flat).download_blob().readall()
            cc.get_blob_client(nested).upload_blob(data, overwrite=True)
            actions+=1
    if actions==0:
        print('No copy actions performed.')
    else:
        print(f'{actions} file(s) copied.')

if __name__=='__main__':
    main()
