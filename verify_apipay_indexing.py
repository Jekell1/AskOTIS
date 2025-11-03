"""
Verify that APIPAY.CBL header comments with transaction codes were indexed.
"""

import json
import requests

# Load config
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']

# Search for specific content from APIPAY header comments
search_url = f"{endpoint}/indexes/code-chunks/docs/search?api-version=2024-07-01"
headers = {
    'Content-Type': 'application/json',
    'api-key': key
}

# Search for transaction code definitions
queries = [
    "APIPAY transaction codes 01 02 03 04",
    "SINGLE PAYMENT REFERENCE CODES PROCESSING",
    "01 - SINGLE PAYMENT PROCESSING",
    "APIPAY.CBL file_id:*APIPAY*"
]

for query in queries:
    print(f"\n🔍 Searching for: {query}")
    print("=" * 60)
    
    search_data = {
        "search": query,
        "top": 3,
        "select": "chunk_id,file_id,start_line,end_line,text",
        "searchMode": "any"
    }
    
    response = requests.post(search_url, headers=headers, json=search_data)
    
    if response.status_code == 200:
        results = response.json()
        if results['value']:
            for i, doc in enumerate(results['value'][:3]):
                print(f"\n📄 Result {i+1}:")
                print(f"   Chunk: {doc['chunk_id']}")
                print(f"   Lines: {doc['start_line']}-{doc['end_line']}")
                print(f"   File: {doc['file_id']}")
                print(f"   Text preview: {doc['text'][:200]}...")
        else:
            print("❌ No results found")
    else:
        print(f"❌ Search failed: {response.status_code}")

print("\n" + "=" * 80)
print("SUMMARY: Checking if header comments were indexed...")
print("=" * 80)