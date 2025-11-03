#!/usr/bin/env python3
"""
Find all direct CALL statements in LONPF2 programs
"""
import requests
import json
import re

def find_direct_calls():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== SEARCHING FOR DIRECT CALL STATEMENTS IN LONPF2 ===')
    
    # First, get all LONPF2 file content
    search_body = {
        'search': '*',
        'select': 'repo_path,line,code',
        'filter': 'search.ismatch(\'LONPF2\', \'repo_path\')',
        'top': 1000
    }
    
    response = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body
    )
    
    if response.status_code == 200:
        results = response.json()
        print(f'Found {len(results.get("value", []))} lines in LONPF2 files')
        
        all_calls = set()
        call_lines = []
        
        for doc in results.get('value', []):
            code = doc.get('code', '').strip().upper()
            line_num = doc.get('line', 0)
            
            # Look for CALL statements
            if code.startswith('CALL '):
                call_lines.append((line_num, doc.get('code', '').strip()))
                
                # Extract program name from quoted strings
                match = re.search(r'CALL\s+"([^"]+)"', code, re.IGNORECASE)
                if match:
                    all_calls.add(match.group(1))
                else:
                    # Try single quotes
                    match = re.search(r"CALL\s+'([^']+)'", code, re.IGNORECASE)
                    if match:
                        all_calls.add(match.group(1))
                    else:
                        # Try unquoted program names
                        match = re.search(r'CALL\s+([A-Z0-9]+)', code, re.IGNORECASE)
                        if match:
                            all_calls.add(match.group(1))
        
        print(f'\n=== CALL STATEMENTS FOUND ({len(call_lines)}) ===')
        for line_num, code in sorted(call_lines):
            print(f'Line {line_num}: {code}')
        
        print(f'\n=== PROGRAMS DIRECTLY CALLED BY LONPF2 ({len(all_calls)}) ===')
        for i, prog in enumerate(sorted(all_calls), 1):
            print(f'{i}. {prog}')
            
        return sorted(all_calls)
    
    else:
        print(f'Error: {response.status_code} - {response.text}')
        return []

if __name__ == "__main__":
    calls = find_direct_calls()
    print(f'\n=== SUMMARY ===')
    print(f'Total direct calls found: {len(calls)}')
