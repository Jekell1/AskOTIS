#!/usr/bin/env python3
"""
Search for CALL statements in LONPF2 files to understand the data structure
"""
import requests
import json

search_endpoint = 'https://az-use1-ai-search.search.windows.net'
search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
headers = {'api-key': search_key, 'Content-Type': 'application/json'}

print('=== SEARCHING FOR CALL STATEMENTS IN LONPF2 FILES ===')

# Search for any content with 'CALL' in LONPF2 files
search_body = {
    'search': 'CALL',
    'select': 'repo_path,line,code',
    'filter': 'search.ismatch(\'LONPF2\', \'repo_path\')',
    'top': 20
}

response = requests.post(
    f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
    headers=headers,
    json=search_body
)

if response.status_code == 200:
    results = response.json()
    print(f'Found {len(results.get("value", []))} lines containing CALL in LONPF2 files')
    
    for doc in results.get('value', []):
        print(f'File: {doc.get("repo_path", "")}')
        print(f'Line {doc.get("line", 0)}: {doc.get("code", "")}')
        print('-' * 60)
        
    # Also check what fields are available
    if results.get('value'):
        first_doc = results['value'][0]
        print('\n=== AVAILABLE FIELDS IN DOCUMENTS ===')
        for field in first_doc.keys():
            print(f'- {field}: {type(first_doc[field])}')
else:
    print(f'Error: {response.status_code} - {response.text}')
