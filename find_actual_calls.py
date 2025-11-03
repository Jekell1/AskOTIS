#!/usr/bin/env python3
"""
Find actual CALL statements in LONPF2.CBL
"""
import requests
import json
import re

search_endpoint = 'https://az-use1-ai-search.search.windows.net'
search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
headers = {'api-key': search_key, 'Content-Type': 'application/json'}

print('=== FINDING ACTUAL CALL STATEMENTS IN LONPF2 ===')

# Get all lines from LONPF2.CBL
search_body = {
    'search': '*',
    'select': 'repo_path,line,code',
    'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
    'top': 1000
}

response = requests.post(
    f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
    headers=headers,
    json=search_body
)

if response.status_code == 200:
    results = response.json()
    print(f'Examining {len(results.get("value", []))} lines from LONPF2.CBL')
    
    call_statements = []
    called_programs = set()
    
    for doc in results.get('value', []):
        code = doc.get('code', '').strip()
        line_num = doc.get('line', 0)
        
        # Look for actual CALL statements (not PERFORM CALL-xxx or comments)
        code_upper = code.upper().strip()
        if code_upper.startswith('CALL ') and not code.strip().startswith('*'):
            call_statements.append((line_num, code))
            
            # Extract program name
            # Pattern: CALL PROGRAM-NAME or CALL 'PROGRAM-NAME' or CALL "PROGRAM-NAME"
            match = re.search(r'CALL\s+([A-Z0-9_-]+)', code_upper)
            if match:
                prog = match.group(1)
                # Skip USING and other keywords
                if prog not in ['USING', 'ON', 'OVERFLOW', 'EXCEPTION']:
                    called_programs.add(prog)
    
    print(f'\n=== CALL STATEMENTS FOUND ({len(call_statements)}) ===')
    for line_num, code in sorted(call_statements):
        print(f'Line {line_num}: {code}')
    
    print(f'\n=== CALLED PROGRAMS ({len(called_programs)}) ===')
    for i, prog in enumerate(sorted(called_programs), 1):
        print(f'{i}. {prog}')
        
else:
    print(f'Error: {response.status_code} - {response.text}')
