#!/usr/bin/env python3
"""Count JSONL logical document lines in blob storage for each dataset and compare to index counts.

Usage: python count_dataset_lines.py --container aisearch --prefix S35-Source/

Outputs a table: dataset, sourceLines, indexExplicitCount, diff
"""
import argparse, json, os, sys, io
from azure.storage.blob import BlobServiceClient
import requests
API_VERSION="2024-07-01"
DATASETS = {
    'chunks': 'chunks',
    'symbols': 'symbols',
    'xrefs': 'xrefs',
    'calls': 'calls',
    'files': 'files',
    'paragraphs': 'paragraphs',
    'facts': 'procedure_facts',  # sharded
    'flow_edges': 'flow_edges',
    'copybooks': 'copybooks'
}
INDEX_NAMES = {
    'chunks': 'code-chunks',
    'symbols': 'cobol-symbols',
    'xrefs': 'cobol-xrefs',
    'calls': 'cobol-calls',
    'files': 'cobol-files',
    'paragraphs': 'cobol-paragraphs',
    'facts': 'cobol-facts',
    'flow_edges': 'cobol-flow-edges-v2',
    'copybooks': 'cobol-copybooks'
}

def load_settings():
    vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    return vals

def get_index_explicit_count(ep,key,index):
    url=f"{ep}/indexes/{index}/docs?api-version={API_VERSION}&$count=true&$top=0"
    r=requests.get(url,headers={'api-key':key},timeout=60)
    try:
        return r.json().get('@odata.count')
    except Exception:
        return None

def count_lines_in_blob(blob_client):
    data = blob_client.download_blob().readall()
    # Count non-empty lines
    return sum(1 for ln in data.splitlines() if ln.strip())

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--container', required=True)
    ap.add_argument('--prefix', required=True)
    args=ap.parse_args()
    vals=load_settings()
    conn=vals['AzureWebJobsStorage']
    ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
    key = (vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')).strip()
    bsc = BlobServiceClient.from_connection_string(conn)
    cc = bsc.get_container_client(args.container)
    base=f"{args.prefix.rstrip('/')}/JSONL"
    results=[]
    for ds, folder in DATASETS.items():
        prefix=f"{base}/{folder}/"
        blobs=list(cc.list_blobs(name_starts_with=prefix))
        total=0
        counted=False
        for b in blobs:
            # Only JSONL
            if not b.name.endswith('.jsonl'): continue
            # For facts skip manifest if present
            if ds=='facts' and 'manifest' in b.name: continue
            try:
                bc=cc.get_blob_client(b)
                lines=count_lines_in_blob(bc)
                total+=lines
                counted=True
            except Exception as e:
                print(f"⚠️ Error reading {b.name}: {e}")
        idx_name = INDEX_NAMES[ds]
        idx_count = get_index_explicit_count(ep,key,idx_name)
        results.append((ds,total if counted else None, idx_count))
    width=12
    print(f"{'dataset':<{width}} {'sourceLines':>12} {'indexCount':>12} {'diff':>10}")
    for ds,src,idx in results:
        diff = (idx - src) if (src is not None and idx is not None) else ''
        print(f"{ds:<{width}} {str(src):>12} {str(idx):>12} {str(diff):>10}")

if __name__=='__main__':
    main()
