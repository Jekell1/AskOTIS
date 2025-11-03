#!/usr/bin/env python3
"""
Examine all CALL-related lines in LONPF2 to understand patterns
"""
import requests
import json
import re

def examine_call_lines():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== EXAMINING ALL CALL-RELATED LINES IN LONPF2 ===')
    
    # Search for all lines containing CALL
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
        
        all_lines = []
        for doc in results.get('value', []):
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            all_lines.append((line_num, code))
        
        # Sort and display all
        all_lines.sort()
        
        actual_calls = []
        called_programs = set()
        
        print('\n=== ALL CALL-RELATED LINES ===')
        for line_num, code in all_lines:
            print(f'Line {line_num}: {code}')
            
            # Look for actual CALL statements
            code_upper = code.upper()
            if (('CALL FORM-PROGX' in code_upper) or 
                ('CALL "' in code_upper) or 
                ('CALL \'' in code_upper) or
                (code.strip().upper().startswith('CALL ') and 
                 not code.strip().startswith('*') and 
                 'PERFORM' not in code_upper)):
                
                actual_calls.append((line_num, code))
                
                # Extract program name
                match = re.search(r'CALL\s+([A-Z0-9_-]+)', code_upper)
                if match:
                    prog = match.group(1)
                    if prog not in ['USING', 'ON', 'OVERFLOW', 'EXCEPTION']:
                        called_programs.add(prog)
        
        print(f'\n=== IDENTIFIED CALL STATEMENTS ({len(actual_calls)}) ===')
        for line_num, code in actual_calls:
            print(f'Line {line_num}: {code}')
        
        print(f'\n=== CALLED PROGRAMS ({len(called_programs)}) ===')
        for i, prog in enumerate(sorted(called_programs), 1):
            print(f'{i}. {prog}')
        
        return sorted(called_programs)
    
    else:
        print(f'Error: {response.status_code} - {response.text}')
        return []

if __name__ == "__main__":
    programs = examine_call_lines()
    print(f'\n=== FINAL SUMMARY ===')
    print(f'Programs directly called by LONPF2: {len(programs)}')
