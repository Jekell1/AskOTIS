import os, json, sys, argparse, math, uuid
from typing import Dict, List

"""Dedupe and shard the large files.jsonl aggregates into smaller shard blobs to allow full re-index.

Process:
 1. Download (or read locally if already present) the two source blobs:
      S35-Source/JSONL/files.jsonl
      S35-Source/JSONL/files/files.jsonl
 2. Parse all lines, keeping first occurrence per file_id.
 3. Write shard JSONL files locally (default shard size 1000 lines) under output directory.
 4. Optionally upload shards to container at prefix S35-Source/JSONL/files/shards/part-XXXX.jsonl
 5. (User) Remove or rename original aggregate blobs OR leave them but change indexer data source query to shards prefix only.
 6. Reset + run indexer.

Usage examples:
  python split_dedupe_files_jsonl.py --container aisearch --do-upload
  python split_dedupe_files_jsonl.py --shard-size 800 --out-dir shard_output

Requires azure-storage-blob if uploading.
"""

try:
    from azure.storage.blob import BlobServiceClient
    HAS_BLOB = True
except ImportError:
    HAS_BLOB = False

API_PREFIX = 'S35-Source/JSONL'
PRIMARY_FILE = 'files.jsonl'
NESTED_FILE = 'files/files.jsonl'
SHARD_FOLDER = 'files/shards'

def load_settings():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            data=json.load(open('local.settings.json','r',encoding='utf-8'))
            vals=data.get('Values',{})
        except Exception:
            pass
    return vals

def conn_string(vals):
    for k in ['AzureWebJobsStorage','DEPLOYMENT_STORAGE_CONNECTION_STRING']:
        if vals.get(k):
            return vals[k]
    return os.environ.get('AzureWebJobsStorage') or os.environ.get('DEPLOYMENT_STORAGE_CONNECTION_STRING')

def stream_blob_lines(container_client, name):
    blob = container_client.get_blob_client(name)
    stream = blob.download_blob()
    for chunk in stream.chunks():
        text = chunk.decode('utf-8',errors='ignore')
        for line in text.splitlines():
            if line.strip():
                yield line

def read_local_or_blob(container_client, blob_name, local_cache_dir):
    local_path = os.path.join(local_cache_dir, blob_name.replace('/','_'))
    if os.path.exists(local_path):
        with open(local_path,'r',encoding='utf-8') as f:
            for line in f:
                line=line.rstrip('\n')
                if line: yield line
        return
    # fetch from blob
    os.makedirs(os.path.dirname(local_path), exist_ok=True)
    with open(local_path,'w',encoding='utf-8') as out:
        for line in stream_blob_lines(container_client, f"{API_PREFIX}/{blob_name}"):
            out.write(line+'\n')
            yield line

def write_shards(out_dir, docs: List[dict], shard_size: int):
    os.makedirs(out_dir, exist_ok=True)
    total = len(docs)
    shards = math.ceil(total / shard_size)
    paths = []
    for i in range(shards):
        part = docs[i*shard_size:(i+1)*shard_size]
        name = f"part-{i:04d}.jsonl"
        path = os.path.join(out_dir, name)
        with open(path,'w',encoding='utf-8') as f:
            for d in part:
                f.write(json.dumps(d, ensure_ascii=False)+'\n')
        paths.append(path)
    return paths

def upload_shards(container_client, shard_paths, overwrite=False, remote_prefix=SHARD_FOLDER):
    uploaded=[]
    for p in shard_paths:
        blob_name = f"{API_PREFIX}/{remote_prefix}/{os.path.basename(p)}"
        with open(p,'rb') as f:
            container_client.upload_blob(name=blob_name, data=f, overwrite=overwrite)
        uploaded.append(blob_name)
    return uploaded

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--container', default='aisearch')
    ap.add_argument('--shard-size', type=int, default=1000)
    ap.add_argument('--out-dir', default='files_shards_output')
    ap.add_argument('--local-cache', default='.cache_files_blobs')
    ap.add_argument('--no-upload', action='store_true', help='Do not upload shards (just produce locally)')
    ap.add_argument('--force', action='store_true', help='Rebuild even if shards already exist')
    args=ap.parse_args()

    vals=load_settings()
    conn=conn_string(vals)
    if not conn:
        print('No storage connection string found.', file=sys.stderr)
        sys.exit(2)
    if not HAS_BLOB and not args.no_upload:
        print('azure-storage-blob not installed; install or use --no-upload', file=sys.stderr)
        sys.exit(2)

    # Setup container client
    if HAS_BLOB:
        from azure.storage.blob import BlobServiceClient
        bsc=BlobServiceClient.from_connection_string(conn)
        cc=bsc.get_container_client(args.container)
    else:
        cc=None

    # Load lines from both blobs
    seen: Dict[str, dict] = {}
    total_lines=0
    for source_blob in [PRIMARY_FILE, NESTED_FILE]:
        print(f"Reading {source_blob}...")
        for line in read_local_or_blob(cc, source_blob, args.local_cache):
            total_lines+=1
            try:
                obj=json.loads(line)
            except Exception:
                continue
            fid=obj.get('file_id')
            if not fid: # skip if missing key
                continue
            if fid not in seen: # keep first occurrence
                seen[fid]=obj
    print(f"Total raw lines processed: {total_lines}")
    print(f"Unique file_id docs: {len(seen)}")

    # Sort docs for deterministic shard grouping
    docs=sorted(seen.values(), key=lambda d: d.get('file_id'))
    shard_paths=write_shards(args.out_dir, docs, args.shard_size)
    print(f"Wrote {len(shard_paths)} shard files to {args.out_dir}")

    if args.no_upload:
        print('Skipping upload (--no-upload set).')
        return
    uploaded=upload_shards(cc, shard_paths, overwrite=True)
    print('Uploaded shards:')
    for u in uploaded[:8]:
        print(' ',u)
    if len(uploaded)>8:
        print('  ...')
    print('\nNext steps:')
    print('  1. (Optional) Rename or delete original large blobs to prevent double ingestion:')
    print(f"     - {API_PREFIX}/{PRIMARY_FILE}")
    print(f"     - {API_PREFIX}/{NESTED_FILE}")
    print(f"  2. Reset indexer: POST /indexers/idx-files/reset")
    print('  3. Run indexer: python run_indexers.py idx-files')
    print('  4. Verify doc count ~ unique file_id total above.')

if __name__=='__main__':
    main()
