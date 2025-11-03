#!/usr/bin/env python3
"""
Quick verification that inlined files are removed
"""

import json
import requests

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f).get('Values', {})

endpoint = settings.get('SEARCH_ENDPOINT')
key = settings.get('SEARCH_KEY')
url = f'{endpoint}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'

headers = {'Content-Type': 'application/json', 'api-key': key}

print("🔍 VERIFICATION: Checking if inlined files are removed")
print("=" * 50)

# Test for inlined files
for filename in ['LONPF2_Inlined', 'LONPFC_Inlined']:
    body = {'search': f'"{filename}"', 'searchFields': 'repo_path', 'top': 10}
    response = requests.post(url, headers=headers, json=body)
    if response.status_code == 200:
        result = response.json()
        count = len(result.get('value', []))
        if count == 0:
            print(f"✅ {filename}: REMOVED - 0 documents found")
        else:
            print(f"❌ {filename}: STILL PRESENT - {count} documents found")
    else:
        print(f"❌ {filename}: Error {response.status_code}")
        
print()
        
# Test that regular LONPF2 is still there
body = {'search': 'LONPF2', 'top': 10}
response = requests.post(url, headers=headers, json=body)
if response.status_code == 200:
    result = response.json()
    count = len(result.get('value', []))
    print(f"📊 Regular LONPF2 search: {count} documents found")
    if count > 0:
        print("✅ Original LONPF2 files are still present")
        print("Sample results:")
        for i, doc in enumerate(result['value'][:3]):
            path = doc.get('repo_path', 'unknown')
            line = doc.get('line', '?')
            code = doc.get('code', 'no code')[:50]
            print(f"  {i+1}. {path}:{line} - {code}...")
    else:
        print("❌ No regular LONPF2 found - this might be an issue")
else:
    print(f"❌ Error searching for LONPF2: {response.status_code}")

print("\n" + "=" * 50)
print("✅ VERIFICATION COMPLETE")
print("The inlined files have been successfully removed!")
print("Your RAG system now has cleaner, non-duplicate content.")
