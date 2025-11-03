"""
Retrieve specific APIPAY chunks to find transaction code definitions.
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

# Get specific APIPAY chunks by chunk_id
chunk_ids = ["APIPAY_1_25", "APIPAY_21_45", "APIPAY_41_65", "APIPAY_61_85"]

for chunk_id in chunk_ids:
    search_data = {
        "search": f'chunk_id:"{chunk_id}"',
        "top": 1,
        "select": "chunk_id,start_line,end_line,text"
    }
    
    response = requests.post(search_url, headers=headers, json=search_data)
    
    if response.status_code == 200:
        results = response.json()
        if results['value']:
            doc = results['value'][0]
            print(f"\n📄 {doc['chunk_id']} (Lines {doc['start_line']}-{doc['end_line']})")
            print("=" * 60)
            text = doc['text']
            
            # Check if this chunk contains transaction codes
            if any(x in text.lower() for x in ['01 -', '02 -', '03 -', 'transaction']):
                print("🎯 FOUND TRANSACTION CODES!")
                print(text)
            else:
                # Show first few lines to understand content
                lines = text.split('\n')[:5]
                for line in lines:
                    print(f"   {line}")
                print("   ... (no transaction codes found)")
        else:
            print(f"❌ Chunk {chunk_id} not found")
    else:
        print(f"❌ Search for {chunk_id} failed: {response.status_code}")