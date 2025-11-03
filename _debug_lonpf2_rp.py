#!/usr/bin/env python3
"""
Debug: Check what LONPF2 code chunks exist and search for RP transaction logic.
"""

import json
import requests

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

url = f"{settings['SEARCH_ENDPOINT']}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"

# First, check if LONPF2 exists
print("=" * 80)
print("Step 1: Checking if LONPF2 exists in new_code_chunks index")
print("=" * 80)

body = {
    'search': '*',
    'filter': "program_id eq 'LONPF2'",
    'select': 'program_id,start_line,end_line',
    'top': 5
}

r = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
docs = r.json().get('value', [])

print(f"\nFound {len(docs)} chunks for program_id='LONPF2'")

if len(docs) == 0:
    # Try searching for LONPF2 in content
    print("\n" + "=" * 80)
    print("Step 2: Searching for 'LONPF2' in content")
    print("=" * 80)
    
    body = {
        'search': 'LONPF2',
        'top': 10,
        'select': 'program_id,file_path,start_line,end_line'
    }
    
    r = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
    docs = r.json().get('value', [])
    
    print(f"\nFound {len(docs)} chunks mentioning 'LONPF2'")
    
    if docs:
        # Show unique program_ids
        program_ids = set(doc.get('program_id', 'N/A') for doc in docs)
        file_paths = set(doc.get('file_path', 'N/A') for doc in docs)
        
        print(f"\nUnique program_ids: {program_ids}")
        print(f"\nUnique file_paths: {file_paths}")
        
        # Show first few docs
        print("\nFirst 5 documents:")
        for i, doc in enumerate(docs[:5], 1):
            print(f"  {i}. program_id='{doc.get('program_id')}', file_path='{doc.get('file_path')}'")
            print(f"     Lines: {doc.get('start_line')}-{doc.get('end_line')}")

else:
    print("\n✓ LONPF2 chunks found with program_id='LONPF2'")
    
    # Now search for RP transaction logic
    print("\n" + "=" * 80)
    print("Step 3: Searching for RP transaction logic in LONPF2")
    print("=" * 80)
    
    # Try various search patterns
    patterns = [
        'RP',
        'TRCD',
        'LP-TRCD',
        'HOLD-LP-TRCD',
        'transaction code',
        'EVALUATE',
        'WHEN'
    ]
    
    for pattern in patterns:
        body = {
            'search': pattern,
            'filter': "program_id eq 'LONPF2'",
            'top': 3,
            'select': 'start_line,end_line,content'
        }
        
        r = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
        docs = r.json().get('value', [])
        
        print(f"\nPattern '{pattern}': {len(docs)} matches")
        
        if docs:
            for doc in docs[:1]:  # Show first match
                content = doc.get('content', '')[:300]
                print(f"  Lines {doc.get('start_line')}-{doc.get('end_line')}")
                print(f"  Content preview: {content}...")
                break
