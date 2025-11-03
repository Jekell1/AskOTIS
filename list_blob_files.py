import os, json, sys
from typing import List

"""Enumerate blobs and line counts for the 'files' JSONL source used by the cobol-files index.

Usage:
  python list_blob_files.py --prefix S35-Source/JSONL/files --limit 0

Behavior:
 - Connects using AzureWebJobsStorage / DEPLOYMENT_STORAGE_CONNECTION_STRING / explicit SOURCE_* values.
 - Lists all blobs whose name starts with the provided --prefix.
 - For each .jsonl blob, can optionally stream line counts (controlled by --count-lines flag).
 - Prints summary: total blobs, total jsonl blobs, total lines (if counted).

Notes:
 - If there is a single aggregated file (e.g. files.jsonl), its line count should match expected doc total (~9951).
 - If indexer only ingested 1740, we will compare which portion is being missed (maybe partial file upload earlier?).
 - Azure Search blob indexer with parsingMode jsonLines will treat each line as a document; ensure newline endings are consistent (LF or CRLF) but typically fine.
"""

import argparse

try:
    from azure.storage.blob import BlobServiceClient
except ImportError:
    print("Missing azure-storage-blob. Install with: pip install azure-storage-blob", file=sys.stderr)
    sys.exit(2)

def get_connection_string():
    # Try multiple keys from local.settings.json
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            data = json.load(open('local.settings.json','r',encoding='utf-8'))
            vals = data.get('Values', {})
        except Exception:
            pass
    keys = [
        os.environ.get('AzureWebJobsStorage'),
        os.environ.get('DEPLOYMENT_STORAGE_CONNECTION_STRING'),
        vals.get('AzureWebJobsStorage'),
        vals.get('DEPLOYMENT_STORAGE_CONNECTION_STRING')
    ]
    for k in keys:
        if k:
            return k
    print('No storage connection string found in env or local.settings.json (AzureWebJobsStorage / DEPLOYMENT_STORAGE_CONNECTION_STRING).', file=sys.stderr)
    sys.exit(2)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--container', default=None, help='Blob container name (default from SOURCE_BLOB_CONTAINER or inferred)')
    ap.add_argument('--prefix', required=True, help='Blob name prefix, e.g. S35-Source/JSONL/files')
    ap.add_argument('--limit', type=int, default=0, help='Max blobs to list (0 = no limit)')
    ap.add_argument('--count-lines', action='store_true', help='Stream and count lines for each .jsonl blob')
    args = ap.parse_args()

    # Load values for default container
    container = args.container
    if not container and os.path.exists('local.settings.json'):
        try:
            vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values', {})
            container = vals.get('SOURCE_BLOB_CONTAINER') or vals.get('BLOB_CONTAINER')
        except Exception:
            pass
    if not container:
        print('Container not specified and not found in config.', file=sys.stderr)
        sys.exit(2)

    conn_str = get_connection_string()
    bsc = BlobServiceClient.from_connection_string(conn_str)
    bc = bsc.get_container_client(container)

    print(f"Listing blobs in container '{container}' with prefix '{args.prefix}'")
    total = 0
    jsonl_blobs: List[str] = []
    for blob in bc.list_blobs(name_starts_with=args.prefix):
        total += 1
        if blob.name.endswith('.jsonl'):
            jsonl_blobs.append(blob.name)
        if args.limit and total >= args.limit:
            break
    print(f"Total blobs (prefix match): {total}")
    print(f"JSONL blobs: {len(jsonl_blobs)}")
    for name in jsonl_blobs[:10]:
        print(f"  sample: {name}")
    if len(jsonl_blobs) > 10:
        print("  ...")

    total_lines = 0
    if args.count_lines:
        for name in jsonl_blobs:
            blob_client = bc.get_blob_client(name)
            # Stream download
            stream = blob_client.download_blob()
            lines = 0
            for chunk in stream.chunks():
                # decode chunk and count newlines; robust line counting by splitting
                text = chunk.decode('utf-8', errors='ignore')
                # handle last partial line by simple splitlines keepends
                lines += text.count('\n')
            # If file does not end with newline, add 1
            if not text.endswith('\n'):
                lines += 1
            print(f"Lines in {name}: {lines}")
            total_lines += lines
        print(f"Total aggregated JSONL lines: {total_lines}")
    else:
        print("(Line counting skipped; use --count-lines to enable)")

    if args.count_lines:
        print("\nIf aggregated lines (~9951) >> current index docs (1740), the indexer failed to ingest full file content; consider re-upload or splitting large JSONL into smaller shards and forcing indexer reset.")

if __name__ == '__main__':
    main()
