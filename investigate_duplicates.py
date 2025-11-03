#!/usr/bin/env python3
"""
Investigate duplicate line entries in the search index
"""
import requests
import json
from collections import Counter

def investigate_duplicates():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = 'ytClysW2tFUN8FxpSCRZMw8vU5sgpHbskgNjuGOdLgAzSeB2V0Ef'
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== INVESTIGATING DUPLICATE LINE ENTRIES ===')
    
    # Get a sample of lines from LONPF2 to check for duplicates
    search_body = {
        'search': '*',
        'select': 'repo_path,line,code,id',
        'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
        'orderby': 'line',
        'top': 200
    }
    
    response = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body
    )
    
    if response.status_code == 200:
        results = response.json()
        documents = results.get('value', [])
        print(f'Retrieved {len(documents)} documents from LONPF2')
        
        # Count duplicates by line number
        line_counts = Counter()
        line_content = {}
        
        for doc in documents:
            line_num = doc.get('line', 0)
            code = doc.get('code', '').strip()
            doc_id = doc.get('id', 'unknown')
            
            line_counts[line_num] += 1
            
            if line_num not in line_content:
                line_content[line_num] = []
            line_content[line_num].append({
                'code': code,
                'id': doc_id,
                'path': doc.get('repo_path', '')
            })
        
        # Find duplicates
        duplicates = {line: count for line, count in line_counts.items() if count > 1}
        
        if duplicates:
            print(f'\n🔍 Found {len(duplicates)} line numbers with multiple entries:')
            
            for line_num, count in sorted(duplicates.items())[:10]:  # Show first 10
                print(f'\nLine {line_num}: {count} copies')
                for i, entry in enumerate(line_content[line_num]):
                    code_preview = entry['code'][:60]
                    print(f'  Copy {i+1}: {code_preview}...')
                    print(f'         ID: {entry["id"]}')
                    print(f'       Path: {entry["path"]}')
        else:
            print('✅ No duplicate line numbers found in this sample')
        
        # Check for exact duplicate content
        print(f'\n=== CHECKING FOR IDENTICAL CONTENT DUPLICATES ===')
        content_groups = {}
        
        for doc in documents:
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            key = f'{line_num}:{code}'
            
            if key not in content_groups:
                content_groups[key] = []
            content_groups[key].append(doc.get('id', 'unknown'))
        
        exact_duplicates = {key: ids for key, ids in content_groups.items() if len(ids) > 1}
        
        if exact_duplicates:
            print(f'Found {len(exact_duplicates)} sets of exact duplicates:')
            for key, ids in list(exact_duplicates.items())[:5]:  # Show first 5
                line_num, code = key.split(':', 1)
                print(f'\nLine {line_num}: {code[:50]}...')
                print(f'  Duplicate IDs: {ids}')
        else:
            print('✅ No exact content duplicates found in this sample')
        
        # Check the specific example we saw earlier (line 2450)
        print(f'\n=== CHECKING LINE 2450 SPECIFICALLY ===')
        if 2450 in line_content:
            entries_2450 = line_content[2450]
            print(f'Line 2450 has {len(entries_2450)} entries:')
            for i, entry in enumerate(entries_2450):
                print(f'  {i+1}. {entry["code"]}')
                print(f'     ID: {entry["id"]}')
        else:
            print('Line 2450 not found in this sample')
            
        # Summary
        print(f'\n=== SUMMARY ===')
        print(f'Total documents examined: {len(documents)}')
        print(f'Unique line numbers: {len(line_counts)}')
        print(f'Line numbers with duplicates: {len(duplicates)}')
        print(f'Sets of exact content duplicates: {len(exact_duplicates)}')
        
        if duplicates or exact_duplicates:
            print(f'\n⚠️  INDEX DATA QUALITY ISSUE DETECTED')
            print(f'Recommendation: Consider reindexing to eliminate duplicates')
        else:
            print(f'\n✅ NO SIGNIFICANT DUPLICATION ISSUES FOUND')
    
    else:
        print(f'❌ Error: {response.status_code} - {response.text}')

if __name__ == "__main__":
    investigate_duplicates()
