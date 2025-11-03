"""
Search for transaction code definitions in indexed APIPAY chunks.
"""

import json
import requests

# Load config
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']

search_url = f"{endpoint}/indexes/code-chunks/docs/search?api-version=2024-07-01"
headers = {
    'Content-Type': 'application/json',
    'api-key': key
}

# Search specifically for APIPAY chunks containing transaction codes
search_data = {
    "search": "file_id:cobol_src/SP/APIPAY.CBL",
    "top": 20,
    "select": "chunk_id,start_line,end_line,text",
    "searchMode": "all"
}

response = requests.post(search_url, headers=headers, json=search_data)

if response.status_code == 200:
    results = response.json()
    print(f"Found {len(results['value'])} APIPAY chunks")
    print("=" * 80)
    
    # Look for chunks that might contain transaction codes
    for doc in results['value']:
        text = doc['text'].lower()
        if any(code in text for code in ['01 -', '02 -', '03 -', 'transaction', 'codes']):
            print(f"\n📄 Chunk: {doc['chunk_id']}")
            print(f"   Lines: {doc['start_line']}-{doc['end_line']}")
            print(f"   Text:\n{doc['text']}")
            print("-" * 40)

else:
    print(f"❌ Search failed: {response.status_code}")