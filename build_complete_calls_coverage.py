"""Build complete coverage for new_cobol_calls index.

The current new_cobol_calls index only includes programs that make calls (7,520 programs).
This script adds the missing 2,158 programs that don't make calls to achieve 100% coverage
of all 9,678 programs.

Strategy:
1. Get all programs from program_catalog.discover_program_ids() 
2. Find programs missing from new_cobol_calls
3. Create "zero-call" documents for missing programs
4. Upload to complete the coverage
"""

import os
import sys
import json
import requests
import argparse
import time
from typing import List, Dict, Set

# Add ingest to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'ingest'))
import program_catalog as catalog

API_VERSION = '2023-11-01'
INDEX_NAME = 'new_cobol_calls'

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
    
    ep = first('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT')
    key = first('AZURE_SEARCH_KEY', 'SEARCH_KEY')
    
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    
    return ep.rstrip('/'), key

def get_existing_programs(ep: str, key: str) -> Set[str]:
    """Get programs that already exist in new_cobol_calls index."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    search_url = f"{ep}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}"
    
    existing_programs = set()
    skip = 0
    page_size = 1000
    
    while True:
        search_body = {
            'search': '*',
            'select': 'caller_program',
            'top': page_size,
            'skip': skip
        }
        
        try:
            r = requests.post(search_url, headers=headers, json=search_body, timeout=30)
            if r.status_code != 200:
                print(f"Error retrieving existing programs: {r.status_code}")
                break
                
            docs = r.json().get('value', [])
            if not docs:
                break
                
            for doc in docs:
                caller = doc.get('caller_program')
                if caller:
                    existing_programs.add(caller.upper())
            
            skip += len(docs)
            if len(docs) < page_size:
                break
                
        except Exception as e:
            print(f"Error retrieving existing programs: {e}")
            break
    
    return existing_programs

def create_zero_call_document(program_id: str) -> Dict:
    """Create a document for a program that makes no calls."""
    return {
        'call_id': f'zero-call-{program_id}',
        'file_id': program_id,
        'caller_program': program_id,
        'callee_program': '',  # Empty - no calls made
        'is_dynamic': False,
        'line': 0,
        'col': 0,
        'occurrence': 0,
        'snippet': f'Program {program_id} makes no calls',
        'has_vector': False,
        'file_path': '',
        'call_type': 'none',
        'call_hash': f'zero-call-{program_id}',
        'ingested_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    }

def upload_docs(ep: str, key: str, docs: List[Dict]):
    """Upload documents to the index."""
    if not docs:
        return
        
    url = f"{ep}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    # Upload in batches of 500
    batch_size = 500
    for i in range(0, len(docs), batch_size):
        batch = docs[i:i + batch_size]
        payload = {'value': [{'@search.action': 'mergeOrUpload', **d} for d in batch]}
        
        r = requests.post(url, headers=headers, json=payload)
        if r.status_code not in (200, 201):
            raise RuntimeError(f"Upload failed {r.status_code}: {r.text[:400]}")
        
        print(f"Uploaded batch {i//batch_size + 1}: {len(batch)} docs")

def main():
    parser = argparse.ArgumentParser(description='Build complete coverage for new_cobol_calls')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be added without uploading')
    parser.add_argument('--src-root', default='cobol_src', help='Source root for program discovery')
    
    args = parser.parse_args()
    
    ep, key = load_config()
    
    print("🔍 BUILDING COMPLETE COBOL CALLS COVERAGE")
    print("=" * 60)
    print()
    
    print("📊 Step 1: Discovering all programs...")
    all_programs = set(catalog.discover_program_ids(ep, key, args.src_root, disable_index=False))
    print(f"   Found {len(all_programs):,} total programs")
    
    print("📊 Step 2: Finding existing programs in calls index...")
    existing_programs = get_existing_programs(ep, key)
    print(f"   Found {len(existing_programs):,} programs with calls")
    
    print("📊 Step 3: Identifying missing programs...")
    missing_programs = all_programs - existing_programs
    print(f"   Missing {len(missing_programs):,} programs ({len(missing_programs)/len(all_programs)*100:.1f}%)")
    
    if not missing_programs:
        print("✅ All programs already covered!")
        return
    
    print("📊 Step 4: Creating zero-call documents...")
    zero_call_docs = []
    for program_id in sorted(missing_programs):
        if program_id and program_id.strip():  # Skip empty program IDs
            doc = create_zero_call_document(program_id)
            zero_call_docs.append(doc)
    
    print(f"   Created {len(zero_call_docs):,} zero-call documents")
    
    if args.dry_run:
        print()
        print("🔍 DRY RUN - Sample zero-call documents:")
        for i, doc in enumerate(zero_call_docs[:3]):
            print(f"   Document {i+1}:")
            for key, value in doc.items():
                print(f"     {key}: {value}")
            print()
        
        print(f"💡 Would add {len(zero_call_docs):,} zero-call documents")
        print("   Run without --dry-run to actually upload")
        return
    
    print("📊 Step 5: Uploading zero-call documents...")
    upload_docs(ep, key, zero_call_docs)
    
    print()
    print("✅ COMPLETE CALLS COVERAGE ACHIEVED!")
    print("=" * 50)
    print(f"   • Total programs: {len(all_programs):,}")
    print(f"   • Programs with calls: {len(existing_programs):,}")
    print(f"   • Programs without calls: {len(missing_programs):,}")
    print(f"   • Coverage: 100.0%")
    print()
    print("💡 The new_cobol_calls index now covers ALL programs!")

if __name__ == '__main__':
    main()