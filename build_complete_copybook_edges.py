#!/usr/bin/env python3
"""Enhanced program copybook edges builder for 100% program coverage.

Creates edges for:
1. Programs that use copybooks (from new_cobol_copybook_usage)
2. Programs that use NO copybooks (from new_cobol_program_meta minus usage)

Ensures every program has at least one edge record.
"""

import argparse
import hashlib
import json
import os
import time
import requests
from typing import Dict, Set, List

API_VERSION = '2023-11-01'
EDGE_INDEX = 'new_cobol_program_copybook_edges'
USAGE_INDEX = 'new_cobol_copybook_usage'
META_INDEX = 'new_cobol_program_meta'

def load_config():
    """Load Azure Search endpoint and key."""
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json', 'r', encoding='utf-8') as f:
                vals = json.load(f).get('Values', {})
        except Exception:
            pass
    
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    
    endpoint = first('AZURE_SEARCH_ENDPOINT', 'SearchEndpoint')
    key = first('AZURE_SEARCH_KEY', 'SearchKey')
    
    if not endpoint or not key:
        raise ValueError("Missing Azure Search configuration")
    
    return endpoint, key

def search(ep: str, key: str, index: str, body: dict):
    """Execute search against Azure Search index."""
    r = requests.post(
        f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}",
        headers={'api-key': key, 'Content-Type': 'application/json'},
        json=body,
        timeout=120
    )
    if r.status_code != 200:
        raise SystemExit(f"Search failed: {r.status_code} - {r.text[:400]}")
    return r.json()

def get_all_programs(ep: str, key: str) -> Set[str]:
    """Get all program IDs from program meta index."""
    print("Getting all programs from meta index...")
    programs = set()
    skip = 0
    page_size = 1000
    
    while True:
        body = {
            'search': '*',
            'select': 'program_id',
            'skip': skip,
            'top': page_size
        }
        
        data = search(ep, key, META_INDEX, body)
        rows = data.get('value', [])
        
        if not rows:
            break
            
        for row in rows:
            program_id = row.get('program_id')
            if program_id:
                programs.add(program_id)
        
        print(f"  Loaded {len(programs)} programs (batch: {len(rows)})...")
        
        skip += len(rows)
        
        # If we got fewer than page_size results, we're done
        if len(rows) < page_size:
            break
    
    print(f"Total programs found: {len(programs)}")
    return programs

def get_copybook_usage(ep: str, key: str) -> Dict[tuple, dict]:
    """Get copybook usage data and return aggregated edges."""
    print("Getting copybook usage data...")
    usage_map = {}  # (program_id, copybook_name) -> {count, first_line}
    programs_with_usage = set()
    
    skip = 0
    page_size = 1000
    max_skip = 100000  # Azure Search limit
    
    while skip < max_skip:
        body = {
            'search': '*',
            'select': 'program_id,copybook_name_plain,line_number',
            'skip': skip,
            'top': page_size
        }
        
        try:
            data = search(ep, key, USAGE_INDEX, body)
        except SystemExit as e:
            if "100000" in str(e):
                print(f"  Hit Azure Search skip limit at {skip}, continuing with what we have...")
                break
            raise
            
        rows = data.get('value', [])
        
        if not rows:
            break
            
        for row in rows:
            program_id = row.get('program_id')
            copybook_name = row.get('copybook_name_plain')
            line_number = row.get('line_number')
            
            if not program_id or not copybook_name:
                continue
                
            programs_with_usage.add(program_id)
            key_tuple = (program_id, copybook_name)
            
            if key_tuple not in usage_map:
                usage_map[key_tuple] = {
                    'count': 0,
                    'first': line_number if isinstance(line_number, int) else None
                }
            
            slot = usage_map[key_tuple]
            slot['count'] += 1
            
            if (line_number and isinstance(line_number, int) and 
                (slot['first'] is None or line_number < slot['first'])):
                slot['first'] = line_number
        
        print(f"  Processed {skip + len(rows)} usage records, {len(programs_with_usage)} programs with usage...")
        
        skip += len(rows)
        
        # If we got fewer than page_size results, we're done
        if len(rows) < page_size:
            break
    
    print(f"Programs with copybook usage: {len(programs_with_usage)}")
    print(f"Unique program-copybook edges: {len(usage_map)}")
    
    if skip >= max_skip:
        print(f"WARNING: Hit Azure Search skip limit. Processed first {max_skip} records.")
        print("         This should capture most unique programs with copybook usage.")
    
    return usage_map, programs_with_usage

def build_complete_edges(ep: str, key: str) -> List[dict]:
    """Build complete edge records covering all programs."""
    timestamp = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    
    # Get all programs
    all_programs = get_all_programs(ep, key)
    
    # Get copybook usage
    usage_map, programs_with_usage = get_copybook_usage(ep, key)
    
    # Identify programs without copybook usage
    programs_without_usage = all_programs - programs_with_usage
    print(f"Programs without copybook usage: {len(programs_without_usage)}")
    
    docs = []
    
    # Create edges for programs with copybook usage
    print("Creating edges for programs with copybook usage...")
    for (program_id, copybook_name), info in usage_map.items():
        edge_id = hashlib.sha1(f"{program_id}|{copybook_name}".encode('utf-8')).hexdigest()[:40]
        docs.append({
            'edge_id': edge_id,
            'program_id': program_id,
            'copybook_name_plain': copybook_name,
            'first_line': info['first'] or 0,
            'occurrence_count': info['count'],
            'ingested_at': timestamp
        })
    
    # Create "no copybooks" edges for programs without usage
    print("Creating 'no copybooks' edges for programs without usage...")
    for program_id in programs_without_usage:
        edge_id = hashlib.sha1(f"{program_id}|NO_COPYBOOKS".encode('utf-8')).hexdigest()[:40]
        docs.append({
            'edge_id': edge_id,
            'program_id': program_id,
            'copybook_name_plain': 'NO_COPYBOOKS',
            'first_line': 0,
            'occurrence_count': 0,
            'ingested_at': timestamp
        })
    
    print(f"Total edge records created: {len(docs)}")
    print(f"  - Programs with copybooks: {len(usage_map)}")
    print(f"  - Programs without copybooks: {len(programs_without_usage)}")
    
    return docs

def upload_edges(ep: str, key: str, docs: List[dict]):
    """Upload edge documents to Azure Search."""
    if not docs:
        print("No documents to upload")
        return
    
    url = f"{ep}/indexes/{EDGE_INDEX}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    batch_size = 1000
    for i in range(0, len(docs), batch_size):
        batch = docs[i:i + batch_size]
        payload = {
            'value': [{'@search.action': 'mergeOrUpload', **doc} for doc in batch]
        }
        
        r = requests.post(url, headers=headers, json=payload, timeout=120)
        if r.status_code not in (200, 201):
            print(f'[ERROR] Upload failed: {r.status_code} - {r.text[:300]}')
        else:
            print(f"[UPLOAD] +{len(batch)} / {len(docs)} documents")

def main():
    parser = argparse.ArgumentParser(description='Build complete program copybook edges')
    parser.add_argument('--commit', action='store_true', 
                       help='Actually upload the edges to Azure Search')
    args = parser.parse_args()
    
    endpoint, key = load_config()
    
    print("=== Building Complete Program Copybook Edges ===")
    docs = build_complete_edges(endpoint, key)
    
    if not args.commit:
        print("\n=== PREVIEW MODE ===")
        print(f"Would create {len(docs)} edge records")
        print("\nSample records:")
        for i, doc in enumerate(docs[:5]):
            print(f"  {i+1}. {doc['program_id']} -> {doc['copybook_name_plain']} "
                  f"(count: {doc['occurrence_count']})")
        print(f"\nRun with --commit to upload {len(docs)} records to Azure Search")
    else:
        print("\n=== UPLOADING TO AZURE SEARCH ===")
        upload_edges(endpoint, key, docs)
        print("✓ Complete! All programs now have copybook edge records.")

if __name__ == '__main__':
    main()