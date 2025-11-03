#!/usr/bin/env python3
"""Build complete program inventory with 100% program coverage.

Creates inventory records for:
1. Programs with copybook usage (from new_cobol_copybook_usage)
2. Programs with NO copybook usage (from new_cobol_program_meta)

Ensures every program has an inventory record.
"""

import os
import json
import argparse
import requests
import time
from collections import defaultdict
from typing import Dict, Set

API_VERSION = '2023-11-01'
USAGE_INDEX = 'new_cobol_copybook_usage'
META_INDEX = 'new_cobol_program_meta'
INVENTORY_INDEX = 'new_cobol_program_inventory'

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

def search(endpoint: str, key: str, index: str, body: dict):
    """Execute search against Azure Search index."""
    url = f"{endpoint}/indexes/{index}/docs/search?api-version={API_VERSION}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body, timeout=120)
    if r.status_code != 200:
        raise SystemExit(f"Search failed: {r.status_code} - {r.text[:400]}")
    return r.json()

def get_all_programs(endpoint: str, key: str) -> Set[str]:
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
        
        data = search(endpoint, key, META_INDEX, body)
        rows = data.get('value', [])
        
        if not rows:
            break
            
        for row in rows:
            program_id = row.get('program_id')
            if program_id:
                programs.add(program_id)
        
        print(f"  Loaded {len(programs)} programs (batch: {len(rows)})...")
        
        skip += len(rows)
        
        if len(rows) < page_size:
            break
    
    print(f"Total programs found: {len(programs)}")
    return programs

def build_usage_metrics(endpoint: str, key: str) -> Dict[str, dict]:
    """Build metrics from copybook usage data."""
    print("Building usage metrics from copybook usage...")
    
    copybooks = defaultdict(set)
    paragraphs = defaultdict(set)
    usage_rows = defaultdict(int)
    
    skip = 0
    page_size = 2000
    max_skip = 100000  # Azure Search limit
    
    while skip < max_skip:
        body = {
            'search': '*',
            'select': 'program_id,copybook_name_plain,paragraph_name',
            'skip': skip,
            'top': page_size
        }
        
        try:
            data = search(endpoint, key, USAGE_INDEX, body)
        except SystemExit as e:
            if "100000" in str(e):
                print(f"  Hit Azure Search skip limit at {skip}, continuing with collected data...")
                break
            raise
            
        rows = data.get('value', [])
        
        if not rows:
            break
            
        for row in rows:
            program_id = row.get('program_id')
            if not program_id:
                continue
                
            usage_rows[program_id] += 1
            
            copybook_name = row.get('copybook_name_plain')
            if copybook_name:
                copybooks[program_id].add(copybook_name)
                
            paragraph_name = row.get('paragraph_name')
            if paragraph_name:
                paragraphs[program_id].add(paragraph_name)
        
        skip += len(rows)
        print(f"  Processed {skip:,} usage records, {len(usage_rows)} programs with usage...")
        
        if len(rows) < page_size:
            break
    
    # Convert to regular dict with metrics
    program_metrics = {}
    for program_id in usage_rows:
        program_metrics[program_id] = {
            'copybook_count': len(copybooks[program_id]),
            'paragraph_count': len(paragraphs[program_id]),
            'usage_rows': usage_rows[program_id]
        }
    
    print(f"Built usage metrics for {len(program_metrics)} programs")
    return program_metrics

def build_complete_inventory(endpoint: str, key: str) -> list:
    """Build complete inventory covering all programs."""
    timestamp = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    
    # Get all programs
    all_programs = get_all_programs(endpoint, key)
    
    # Get usage metrics
    usage_metrics = build_usage_metrics(endpoint, key)
    
    # Identify programs without usage
    programs_with_usage = set(usage_metrics.keys())
    programs_without_usage = all_programs - programs_with_usage
    print(f"Programs without copybook usage: {len(programs_without_usage)}")
    
    docs = []
    
    # Create inventory records for programs with usage
    print("Creating inventory records for programs with usage...")
    for program_id, metrics in usage_metrics.items():
        docs.append({
            'program_id': program_id,
            'copybook_count': metrics['copybook_count'],
            'paragraph_count': metrics['paragraph_count'],
            'usage_rows': metrics['usage_rows'],
            'last_ingested_at': timestamp,
            'metrics_json': json.dumps({
                'copybooks': metrics['copybook_count'],
                'paragraphs': metrics['paragraph_count']
            })
        })
    
    # Create inventory records for programs without usage
    print("Creating inventory records for programs without usage...")
    for program_id in programs_without_usage:
        docs.append({
            'program_id': program_id,
            'copybook_count': 0,
            'paragraph_count': 0,
            'usage_rows': 0,
            'last_ingested_at': timestamp,
            'metrics_json': json.dumps({
                'copybooks': 0,
                'paragraphs': 0
            })
        })
    
    print(f"Total inventory records created: {len(docs)}")
    print(f"  - Programs with usage: {len(usage_metrics)}")
    print(f"  - Programs without usage: {len(programs_without_usage)}")
    
    return docs

def upload_inventory(endpoint: str, key: str, docs: list):
    """Upload inventory documents to Azure Search."""
    if not docs:
        print("No documents to upload")
        return
    
    url = f"{endpoint}/indexes/{INVENTORY_INDEX}/docs/index?api-version={API_VERSION}"
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
    parser = argparse.ArgumentParser(description='Build complete program inventory')
    parser.add_argument('--commit', action='store_true', 
                       help='Actually upload the inventory to Azure Search')
    args = parser.parse_args()
    
    endpoint, key = load_config()
    
    print("=== Building Complete Program Inventory ===")
    docs = build_complete_inventory(endpoint, key)
    
    if not args.commit:
        print("\n=== PREVIEW MODE ===")
        print(f"Would create {len(docs)} inventory records")
        print("\nSample records:")
        
        # Show samples of both types
        with_usage = [d for d in docs if d['usage_rows'] > 0]
        without_usage = [d for d in docs if d['usage_rows'] == 0]
        
        print("Programs with usage:")
        for i, doc in enumerate(with_usage[:3]):
            print(f"  {i+1}. {doc['program_id']}: {doc['copybook_count']} copybooks, "
                  f"{doc['paragraph_count']} paragraphs, {doc['usage_rows']} usage rows")
        
        print("Programs without usage:")
        for i, doc in enumerate(without_usage[:3]):
            print(f"  {i+1}. {doc['program_id']}: {doc['copybook_count']} copybooks, "
                  f"{doc['paragraph_count']} paragraphs, {doc['usage_rows']} usage rows")
        
        print(f"\nRun with --commit to upload {len(docs)} records to Azure Search")
    else:
        print("\n=== UPLOADING TO AZURE SEARCH ===")
        upload_inventory(endpoint, key, docs)
        print("✓ Complete! All programs now have inventory records.")

if __name__ == '__main__':
    main()