#!/usr/bin/env python3
"""Analyze copybook meta coverage."""

import requests, os, json

def load_config():
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json', 'r', encoding='utf-8') as f:
                vals = json.load(f).get('Values', {})
        except: pass
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v: return v
        return None
    return first('AZURE_SEARCH_ENDPOINT', 'SearchEndpoint'), first('AZURE_SEARCH_KEY', 'SearchKey')

def main():
    endpoint, key = load_config()

    # Check total copybook files expected
    print('=== Analyzing copybook coverage ===')

    # Get sample from copybook meta to understand structure
    url = f'{endpoint}/indexes/new_cobol_copybook_meta/docs/search?api-version=2023-11-01'
    body = {'search': '*', 'top': 3}
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    if r.status_code == 200:
        docs = r.json().get('value', [])
        print(f'Sample copybook meta records ({len(docs)}):')
        for doc in docs:
            name = doc.get('copybook_name')
            paths = doc.get('file_paths_json')
            summary = doc.get('summary', '')[:100] + '...' if doc.get('summary', '') else 'No summary'
            print(f'  {name}: {paths}')
            print(f'    Summary: {summary}')
    else:
        print(f'Error: {r.status_code}')

    # Count unique copybooks from usage
    print('\n=== Checking unique copybooks from usage ===')
    url = f'{endpoint}/indexes/new_cobol_copybook_usage/docs/search?api-version=2023-11-01'
    
    # Sample approach to count unique copybooks
    unique_copybooks = set()
    skip = 0
    page_size = 1000
    max_records = 10000
    
    while skip < max_records:
        body = {
            'search': '*',
            'select': 'copybook_name_plain',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
            
        for record in records:
            copybook_name = record.get('copybook_name_plain')
            if copybook_name:
                unique_copybooks.add(copybook_name)
        
        skip += len(records)
        
        if len(records) < page_size:
            break
    
    print(f'Sampled unique copybooks from usage: {len(unique_copybooks)}')
    print('Sample copybook names:')
    for i, name in enumerate(sorted(unique_copybooks)[:10]):
        print(f'  {name}')

    # Check how many of these are in copybook_meta
    print(f'\n=== Checking coverage in copybook_meta ===')
    in_meta_count = 0
    sample_missing = []
    
    for copybook_name in list(unique_copybooks)[:100]:  # Sample check
        meta_url = f'{endpoint}/indexes/new_cobol_copybook_meta/docs/search?api-version=2023-11-01'
        meta_body = {
            'search': f'copybook_name:{copybook_name}',
            'select': 'copybook_name'
        }
        
        r = requests.post(meta_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=meta_body)
        if r.status_code == 200:
            meta_docs = r.json().get('value', [])
            if meta_docs:
                in_meta_count += 1
            else:
                sample_missing.append(copybook_name)
    
    print(f'Sample check: {in_meta_count}/100 copybooks found in meta')
    print('Sample missing copybooks:')
    for name in sample_missing[:5]:
        print(f'  {name}')

if __name__ == '__main__':
    main()