#!/usr/bin/env python3
"""Direct query to Azure Search to verify LPMENU screens."""

import requests
import os
from dotenv import load_dotenv

load_dotenv()
ep = os.getenv('AZURE_SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_KEY')

# Use POST method like build_screen_nodes.py does
url = f'{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version=2024-07-01'
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': 'MASTER MENU',
    'select': 'screen_id,program_id,summary_text',
    'count': True,
    'top': 5
}

r = requests.post(url, headers=headers, json=payload)

if r.status_code == 200:
    results = r.json()
    total = results.get('@odata.count', 0)
    docs = results.get('value', [])
    
    print(f'✅ Found {total} total screens')
    print(f'Showing first {len(docs)} results:\n')
    
    for i, doc in enumerate(docs, 1):
        print(f'{i}. Screen ID: {doc.get("screen_id", "")[:60]}')
        print(f'   Program ID: {doc.get("program_id", "")[:50]}')
        summary = doc.get('summary_text', '')[:200].replace('\n', ' ')
        print(f'   Summary: {summary}')
        print()
else:
    print(f'❌ Search failed: {r.status_code}')
    print(f'Response: {r.text[:500]}')
