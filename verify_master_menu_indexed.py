#!/usr/bin/env python3
"""Verify MASTER MENU screens are indexed and searchable."""

import requests
import os
from dotenv import load_dotenv

load_dotenv()
ep = os.getenv('AZURE_SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_KEY')

url = f'{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version=2024-07-01'
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Search for MASTER MENU content
payload = {
    'search': 'MASTER MENU',
    'select': 'screen_id,program_id,summary_text',
    'top': 5
}

r = requests.post(url, headers=headers, json=payload)

if r.status_code != 200:
    print(f'❌ Search failed: {r.status_code}')
    print(f'Response: {r.text[:500]}')
    exit(1)

results = r.json()

if results.get('value'):
    print(f'✅ Found {len(results["value"])} MASTER MENU screens:')
    for doc in results['value']:
        print(f'  • Screen: {doc["screen_id"][:60]}')
        print(f'    Program: {doc["program_id"][:50]}')
        summary = doc['summary_text'][:200].replace('\n', ' ')
        print(f'    Summary: {summary}...')
        print()
else:
    print('❌ No MASTER MENU screens found')
    print(f'Response: {results}')
