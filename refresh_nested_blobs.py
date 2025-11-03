#!/usr/bin/env python3
"""Force-refresh (touch) nested dataset blobs so Azure Search re-ingests them.

Strategy:
 - Download flat file (e.g. chunks.jsonl) and re-upload into nested path (overwrite) to update LastModified.
 - Also write a version-stamped copy (chunks/_refresh_<timestamp>.marker) to ensure folder change.
 - Repeat for symbols (data_items.jsonl).

Usage:
  python refresh_nested_blobs.py --container aisearch --prefix S35-Source/
"""
import argparse, json, time
from datetime import datetime
from azure.storage.blob import BlobServiceClient

def load_conn():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    return vals['AzureWebJobsStorage']

def refresh(cc, base, flat_rel, nested_rel):
    flat=f"{base}/{flat_rel}"
    nested=f"{base}/{nested_rel}"
    flat_client=cc.get_blob_client(flat)
    if not flat_client.exists():
        print(f"❌ Missing flat blob {flat_rel}; skip")
        return False
    data=flat_client.download_blob().readall()
    cc.get_blob_client(nested).upload_blob(data, overwrite=True)
    marker_path=f"{base}/{nested_rel.rsplit('/',1)[0]}/_refresh_{int(time.time())}.marker"
    cc.get_blob_client(marker_path).upload_blob(b"touch", overwrite=True)
    print(f"✅ Refreshed {nested_rel} and wrote marker")
    return True

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--container',required=True)
    ap.add_argument('--prefix',required=True)
    args=ap.parse_args()
    conn=load_conn()
    bsc=BlobServiceClient.from_connection_string(conn)
    cc=bsc.get_container_client(args.container)
    base=f"{args.prefix.rstrip('/')}/JSONL"
    ops=0
    ops+=refresh(cc, base, 'chunks.jsonl','chunks/chunks.jsonl')
    ops+=refresh(cc, base, 'data_items.jsonl','symbols/data_items.jsonl')
    if not ops:
        print('No blobs refreshed (nothing done).')

if __name__=='__main__':
    main()
