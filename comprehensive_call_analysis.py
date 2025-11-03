#!/usr/bin/env python3
"""
Find FORM-PROGX calls and other CALL statements in LONPF2
"""
import requests
import json
import re

def find_all_calls():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== COMPREHENSIVE CALL ANALYSIS FOR LONPF2 ===')
    
    # First, get FORM-PROGX calls
    search_body = {
        'search': 'FORM-PROGX',
        'select': 'repo_path,line,code',
        'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
        'top': 10
    }
    
    response = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body
    )
    
    all_calls = []
    called_programs = set()
    
    if response.status_code == 200:
        results = response.json()
        print(f'Found {len(results.get("value", []))} lines with FORM-PROGX')
        
        for doc in results.get('value', []):
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            print(f'Line {line_num}: {code}')
            
            # Check if this is an actual CALL statement
            if code.strip().upper().startswith('CALL '):
                all_calls.append((line_num, code))
                called_programs.add('FORM-PROGX')
    
    # Now search through all lines for other CALL statements
    print('\n=== SEARCHING FOR ALL CALL STATEMENTS ===')
    search_body2 = {
        'search': '*',
        'select': 'repo_path,line,code',
        'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
        'top': 1000
    }
    
    response2 = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body2
    )
    
    if response2.status_code == 200:
        results2 = response2.json()
        print(f'Examining {len(results2.get("value", []))} total lines')
        
        for doc in results2.get('value', []):
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            
            # Look for lines that start with CALL and aren't comments
            code_upper = code.upper().strip()
            if (code_upper.startswith('CALL ') and 
                not code.strip().startswith('*')):
                
                # Skip if already found
                if (line_num, code) not in all_calls:
                    all_calls.append((line_num, code))
                    
                    # Extract program name
                    # Look for patterns like CALL "PROGRAM" or CALL PROGRAM
                    match = re.search(r'CALL\s+"([^"]+)"', code_upper)
                    if match:
                        called_programs.add(match.group(1))
                    else:
                        match = re.search(r"CALL\s+'([^']+)'", code_upper)
                        if match:
                            called_programs.add(match.group(1))
                        else:
                            match = re.search(r'CALL\s+([A-Z0-9_-]+)', code_upper)
                            if match:
                                prog = match.group(1)
                                if prog not in ['USING', 'ON', 'OVERFLOW', 'EXCEPTION']:
                                    called_programs.add(prog)
    
    print(f'\n=== ALL CALL STATEMENTS FOUND ({len(all_calls)}) ===')
    all_calls.sort()
    for line_num, code in all_calls:
        print(f'Line {line_num}: {code}')
    
    print(f'\n=== PROGRAMS CALLED BY LONPF2 ({len(called_programs)}) ===')
    for i, prog in enumerate(sorted(called_programs), 1):
        print(f'{i}. {prog}')
    
    return sorted(called_programs)

if __name__ == "__main__":
    programs = find_all_calls()
    print(f'\n=== FINAL SUMMARY ===')
    print(f'Total unique programs called by LONPF2: {len(programs)}')
    if programs:
        print('Programs called:')
        for prog in programs:
            print(f'  - {prog}')
