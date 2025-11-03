#!/usr/bin/env python3
"""
Check what program IDs exist in the indexes and find the correct ID for LONPF2.
"""

import json
import requests

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

# Check new_code_chunks index
print("=" * 80)
print("Checking new_code_chunks index")
print("=" * 80)

url = f"{settings['SEARCH_ENDPOINT']}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"

body = {
    'search': 'LONPF',
    'top': 10,
    'select': 'program_id,file_path'
}

r = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
docs = r.json().get('value', [])

print(f"\nSearching for 'LONPF': Found {len(docs)} results")

if docs:
    print("\nPrograms containing 'LONPF':")
    seen = set()
    for doc in docs:
        pid = doc.get('program_id', 'N/A')
        fpath = doc.get('file_path', 'N/A')
        key = (pid, fpath)
        if key not in seen:
            seen.add(key)
            print(f"  - program_id: '{pid}'")
            print(f"    file_path: '{fpath}'")

# Check copybook_usage index
print("\n" + "=" * 80)
print("Checking new_cobol_copybook_usage index")
print("=" * 80)

url2 = f"{settings['SEARCH_ENDPOINT']}/indexes/new_cobol_copybook_usage/docs/search?api-version=2025-08-01-preview"

body2 = {
    'search': 'LONPF2',
    'top': 5,
    'select': 'program_id,copybook_name'
}

r2 = requests.post(url2, headers={'api-key': settings['SEARCH_KEY']}, json=body2)
docs2 = r2.json().get('value', [])

print(f"\nSearching for 'LONPF2' in copybook_usage: Found {len(docs2)} results")

if docs2:
    program_ids = set(doc.get('program_id', 'N/A') for doc in docs2)
    print(f"\nUnique program_ids: {program_ids}")
    
    print("\nFirst 5 copybook usage records:")
    for i, doc in enumerate(docs2[:5], 1):
        print(f"  {i}. program_id='{doc.get('program_id')}', copybook='{doc.get('copybook_name')}'")

# Try to find what the actual program_id format is
print("\n" + "=" * 80)
print("Checking program_id format in new_code_chunks")
print("=" * 80)

body3 = {
    'search': '*',
    'top': 20,
    'select': 'program_id,file_path'
}

r3 = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body3)
docs3 = r3.json().get('value', [])

print(f"\nSample program_ids from new_code_chunks (first 20):")
seen_ids = set()
for doc in docs3:
    pid = doc.get('program_id')
    if pid and pid not in seen_ids:
        seen_ids.add(pid)
        print(f"  - '{pid}'")
        if len(seen_ids) >= 15:
            break
