#!/usr/bin/env python3
import json, os, sys, collections, re
from azure.storage.blob import BlobServiceClient
import requests

TARGET_BLOBS = [
    ("code-chunks","chunks.jsonl","chunk_id"),
    ("cobol-symbols","data_items.jsonl","item_id"),
    ("cobol-xrefs","xrefs.jsonl","xref_id"),
    ("cobol-calls","calls.jsonl","call_id"),
]
PREFIX = 'S35-Source/JSONL'
API_VERSION='2024-07-01'
SAMPLE_LIMIT = 20000            # bytes to read from start
LINE_ANALYZE_MAX = 5000         # max lines to parse per blob for stats

def load_cfg():
    with open('local.settings.json','r',encoding='utf-8') as f:
        vals=json.load(f)['Values']
    return {
        'blob_conn': vals['AzureWebJobsStorage'],
        'search_ep': vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT'),
        'search_key': vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
    }

def analyze_blob(bc, logical_index, blob_name, key_field):
    props = bc.get_blob_properties()
    size = props.size
    data = bc.download_blob(offset=0, length=min(size, SAMPLE_LIMIT)).readall().decode('utf-8','replace')
    raw_lines = data.splitlines()
    lines = [ln for ln in raw_lines if ln.strip()]
    parsed = 0
    key_missing = 0
    key_empty = 0
    key_values = []
    parse_errors = 0
    first_keys = []
    for ln in lines[:LINE_ANALYZE_MAX]:
        s = ln.strip()
        if not s: continue
        try:
            obj = json.loads(s)
            parsed += 1
            if not first_keys:
                first_keys = sorted(list(obj.keys()))
            if key_field not in obj:
                key_missing += 1
            else:
                val = obj.get(key_field)
                if val in (None, ""):
                    key_empty += 1
                else:
                    key_values.append(val)
        except Exception:
            parse_errors += 1
    dup_count = len(key_values) - len(set(key_values))
    result = {
        'index': logical_index,
        'blob': blob_name,
        'size_bytes': size,
        'sampled_lines_nonempty': len(lines),
        'parsed_lines': parsed,
        'parse_errors': parse_errors,
        'first_object_keys': first_keys[:50],
        'expected_key': key_field,
        'key_missing_lines': key_missing,
        'key_empty_lines': key_empty,
        'unique_keys': len(set(key_values)),
        'duplicate_keys': dup_count,
    }
    return result

def fetch_indexer_status(ep, key, name):
    url=f"{ep}/indexers('{name}')/status?api-version={API_VERSION}"
    r=requests.get(url, headers={'api-key':key}, timeout=30)
    if r.status_code!=200:
        return {'name': name, 'error': f"HTTP {r.status_code}"}
    js=r.json()
    last=js.get('lastResult') or {}
    return {
        'name': name,
        'status': last.get('status'),
        'itemsProcessed': last.get('itemsProcessed'),
        'itemsFailed': last.get('itemsFailed'),
        'warnings': last.get('warningCount'),
        'errorMessage': last.get('errorMessage'),
    }

def main():
    cfg = load_cfg()
    bsc = BlobServiceClient.from_connection_string(cfg['blob_conn'])
    cc = bsc.get_container_client('aisearch')

    print('=== BLOB STRUCTURE ANALYSIS ===')
    for logical_index, blob, keyf in TARGET_BLOBS:
        bc = cc.get_blob_client(f"{PREFIX}/{blob}")
        try:
            res = analyze_blob(bc, logical_index, blob, keyf)
        except Exception as e:
            print(f"{blob}: ERROR {e}")
            continue
        print(f"{logical_index}: size={res['size_bytes']} parsed={res['parsed_lines']} sample_nonempty={res['sampled_lines_nonempty']} parse_errors={res['parse_errors']} key_missing={res['key_missing_lines']} key_empty={res['key_empty_lines']} dup_keys={res['duplicate_keys']}")
        print(f"   first_keys={res['first_object_keys']}")
        if res['parsed_lines'] and res['key_missing_lines']==res['parsed_lines']:
            print("   !! All parsed lines missing expected key field -> schema mismatch")
        elif res['parse_errors']==res['sampled_lines_nonempty'] and res['sampled_lines_nonempty']>0:
            print("   !! All sample lines failed to parse -> invalid JSONL format")

    print('\n=== INDEXER STATUS ===')
    for name in ['idx-chunks','idx-symbols','idx-xrefs','idx-calls']:
        st = fetch_indexer_status(cfg['search_ep'], cfg['search_key'], name)
        if 'error' in st:
            print(f"{name}: {st['error']}")
        else:
            print(f"{name}: status={st['status']} processed={st['itemsProcessed']} failed={st['itemsFailed']} warnings={st['warnings']} err={st['errorMessage']}")
            if st.get('itemsProcessed')==0 and st.get('itemsFailed')==0 and not st.get('errorMessage'):
                print("   note: zero processed; likely key mismatch or empty/malformed blob content in source")

if __name__=='__main__':
    main()
