#!/usr/bin/env python3
"""
Extract all actual CALL statements from LONPF2
"""
import requests
import json
import re

def extract_calls_from_lonpf2():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== EXTRACTING ALL CALL STATEMENTS FROM LONPF2 ===')
    
    # Search for all lines containing CALL (not just starting with it)
    search_body = {
        'search': 'CALL',
        'select': 'repo_path,line,code',
        'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
        'top': 100
    }
    
    response = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body
    )
    
    if response.status_code == 200:
        results = response.json()
        print(f'Found {len(results.get("value", []))} lines containing CALL')
        
        actual_calls = []
        called_programs = set()
        
        for doc in results.get('value', []):
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            
            # Filter for actual CALL statements (not PERFORM CALL-xxx or comments)
            if 'CALL ' in code and not code.strip().startswith('*') and 'PERFORM' not in code.upper():
                # Extract the program being called
                code_clean = code.strip()
                if code_clean.upper().startswith('CALL '):
                    actual_calls.append((line_num, code))
                    
                    # Extract program name using regex
                    match = re.search(r'CALL\s+([A-Z0-9_-]+)', code.upper())
                    if match:
                        prog = match.group(1)
                        if prog not in ['USING', 'ON', 'OVERFLOW', 'EXCEPTION']:
                            called_programs.add(prog)
        
        print(f'\n=== ACTUAL CALL STATEMENTS ({len(actual_calls)}) ===')
        for line_num, code in sorted(actual_calls):
            print(f'Line {line_num}: {code}')
        
        print(f'\n=== PROGRAMS CALLED BY LONPF2 ({len(called_programs)}) ===')
        for i, prog in enumerate(sorted(called_programs), 1):
            print(f'{i}. {prog}')
        
        return sorted(called_programs)
    
    else:
        print(f'Error: {response.status_code} - {response.text}')
        return []

if __name__ == "__main__":
    programs = extract_calls_from_lonpf2()
    print(f'\n=== SUMMARY ===')
    print(f'Total programs called by LONPF2: {len(programs)}')
