#!/usr/bin/env python3
"""List first N COBOL source blobs to validate prefix and naming.
Reads connection info from local.settings.json / env (DEPLOYMENT_STORAGE_CONNECTION_STRING or AzureWebJobsStorage).
Config keys used:
  SOURCE_STORAGE_ACCOUNT (optional informational)
  SOURCE_BLOB_CONTAINER (required)
  SOURCE_CODE_PREFIX (optional; if provided, restrict listing)

Usage:
  python list_cobol_blobs.py --limit 60
  python list_cobol_blobs.py --no-prefix
"""
import os, json, argparse, sys
from typing import Iterable

try:
    from azure.storage.blob import BlobServiceClient
except ImportError:
    print("Missing dependency azure-storage-blob. Install with: pip install azure-storage-blob", file=sys.stderr)
    sys.exit(1)

COBOL_EXTS = ('.cbl','.cob','.cobol')

def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
        except Exception:
            pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v: return v
        return None
    conn = first('DEPLOYMENT_STORAGE_CONNECTION_STRING','AzureWebJobsStorage')
    container = first('SOURCE_BLOB_CONTAINER')
    prefix = first('SOURCE_CODE_PREFIX') or ''
    if not conn or not container:
        print('Missing connection string or container name', file=sys.stderr)
        sys.exit(2)
    return conn, container, prefix

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--limit', type=int, default=40, help='Max COBOL blobs to list')
    ap.add_argument('--no-prefix', action='store_true', help='Ignore SOURCE_CODE_PREFIX (list entire container)')
    args = ap.parse_args()

    conn, container, prefix = load_settings()
    bsc = BlobServiceClient.from_connection_string(conn)
    cc = bsc.get_container_client(container)

    count=0
    used_prefix = '' if args.no_prefix else prefix
    print(f"Listing COBOL blobs (limit={args.limit}) container={container} prefix='{used_prefix}'...")
    for b in cc.list_blobs(name_starts_with=used_prefix):
        name=b.name
        if name.lower().endswith(COBOL_EXTS):
            print(name)
            count+=1
            if count>=args.limit:
                break
    print(f"Total COBOL blobs listed: {count}")
    if count==0:
        print("No COBOL blobs found. Consider --no-prefix to verify prefix correctness or check file extensions.")

if __name__=='__main__':
    main()
