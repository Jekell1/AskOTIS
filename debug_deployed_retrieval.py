#!/usr/bin/env python3
"""
Test LONPF2 code chunk retrieval directly to debug the issue.
"""

import json
import requests

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

SEARCH_ENDPOINT = settings['AZURE_SEARCH_ENDPOINT']
SEARCH_KEY = settings['AZURE_SEARCH_KEY']

print("=" * 80)
print("DEBUG: Testing LONPF2 Code Chunk Retrieval")
print("=" * 80)

# Test 1: Filter by name
print("\nTest 1: Filter by name eq 'LONPF2.CBL'")
url = f"{SEARCH_ENDPOINT}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"

body = {
    "search": "*",
    "filter": "name eq 'LONPF2.CBL'",
    "top": 10,
    "select": "chunk_id,name,path,start_line,end_line,text",
    "orderby": "start_line asc"
}

headers = {
    'api-key': SEARCH_KEY,
    'Content-Type': 'application/json'
}

r = requests.post(url, headers=headers, json=body)
print(f"Status: {r.status_code}")

if r.status_code == 200:
    data = r.json()
    count = data.get('@odata.count') or len(data.get('value', []))
    print(f"Results: {count} chunks")
    
    if count > 0:
        print("\n✅ SUCCESS: Found LONPF2 chunks!")
        print("\nFirst 3 chunks:")
        for i, chunk in enumerate(data['value'][:3], 1):
            print(f"\n  Chunk {i}:")
            print(f"    Lines: {chunk['start_line']}-{chunk['end_line']}")
            print(f"    Text preview: {chunk['text'][:150]}...")
    else:
        print("\n❌ PROBLEM: No chunks found with name eq 'LONPF2.CBL'")
else:
    print(f"Error: {r.text}")

# Test 2: Check for RP pattern
print("\n" + "=" * 80)
print("Test 2: Check for RP transaction entry points")
print("=" * 80)

body = {
    "search": "*",
    "filter": "name eq 'LONPF2.CBL'",
    "top": 1000,
    "select": "chunk_id,start_line,end_line,text",
    "orderby": "start_line asc"
}

r = requests.post(url, headers=headers, json=body)

if r.status_code == 200:
    import re
    
    data = r.json()
    chunks = data.get('value', [])
    
    tx_pattern = re.compile(r"IF\s+.*TRCD.*=.*[\"']RP[\"']", re.IGNORECASE)
    
    matches = []
    for chunk in chunks:
        text = chunk.get('text', '')
        if tx_pattern.search(text):
            matches.append(chunk)
    
    print(f"\nTotal chunks: {len(chunks)}")
    print(f"Chunks with RP pattern: {len(matches)}")
    
    if matches:
        print("\n✅ Found RP transaction entry points!")
        for i, chunk in enumerate(matches[:3], 1):
            print(f"\n  Match {i} at lines {chunk['start_line']}-{chunk['end_line']}:")
            # Find the matching line
            for line in chunk['text'].split('\n'):
                if tx_pattern.search(line):
                    print(f"    {line.strip()}")
    else:
        print("\n⚠️  No RP patterns found in LONPF2 code chunks")
        print("    This means the deterministic handler will fall back to all copybooks")

print("\n" + "=" * 80)
print("DIAGNOSIS")
print("=" * 80)

if r.status_code == 200 and len(chunks) > 0:
    if matches:
        print("\n✓ Code chunks are being retrieved correctly")
        print("✓ RP patterns are present in the code")
        print("\n⚠️  But production is still returning too many copybooks")
        print("\nPossible causes:")
        print("  1. Deployment hasn't fully propagated yet (wait 2-3 minutes)")
        print("  2. Function is using cached old code")
        print("  3. Error in copybook retrieval logic after finding entry points")
        print("\nRecommendation:")
        print("  - Wait 5 minutes and test again")
        print("  - Or restart the Azure Function: az functionapp restart -n func-otis-rag -g <resource-group>")
    else:
        print("\n✓ Code chunks are being retrieved correctly")
        print("✗ But no RP transaction patterns found")
        print("\nThis is why it's falling back to all copybooks.")
        print("The pattern matching logic may need adjustment.")
else:
    print("\n✗ Code chunks are NOT being retrieved")
    print("This is the root cause - field names may still be wrong in deployed version.")
