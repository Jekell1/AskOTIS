#!/usr/bin/env python3
"""
Complete dependency analysis for LONPF2 - find copybooks and summarize all dependencies
"""
import requests
import json
import re

def complete_dependency_analysis():
    search_endpoint = 'https://az-use1-ai-search.search.windows.net'
    search_key = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
    headers = {'api-key': search_key, 'Content-Type': 'application/json'}
    
    print('=== COMPLETE DEPENDENCY ANALYSIS FOR LONPF2 ===')
    
    # Find copybooks (COPY statements)
    search_body = {
        'search': 'COPY',
        'select': 'repo_path,line,code',
        'filter': 'search.ismatch(\'LONPF2.CBL\', \'repo_path\')',
        'top': 50
    }
    
    response = requests.post(
        f'{search_endpoint}/indexes/cobol-index/docs/search?api-version=2024-07-01',
        headers=headers,
        json=search_body
    )
    
    copybooks = set()
    
    if response.status_code == 200:
        results = response.json()
        print(f'Found {len(results.get("value", []))} lines with COPY')
        
        copy_statements = []
        
        for doc in results.get('value', []):
            code = doc.get('code', '').strip()
            line_num = doc.get('line', 0)
            
            # Look for actual COPY statements
            if code.upper().strip().startswith('COPY ') and not code.strip().startswith('*'):
                copy_statements.append((line_num, code))
                
                # Extract copybook name
                match = re.search(r'COPY\s+([A-Z0-9_.-]+)', code.upper())
                if match:
                    copybook = match.group(1)
                    if copybook not in ['REPLACING', 'OF', 'IN']:
                        copybooks.add(copybook)
        
        print(f'\n=== COPY STATEMENTS FOUND ({len(copy_statements)}) ===')
        copy_statements.sort()
        for line_num, code in copy_statements:
            print(f'Line {line_num}: {code}')
    
    print(f'\n=== COPYBOOKS USED BY LONPF2 ({len(copybooks)}) ===')
    for i, copybook in enumerate(sorted(copybooks), 1):
        print(f'{i}. {copybook}')
    
    # We already know the external programs from previous analysis
    external_programs = ['FORM-PROGX']
    
    print(f'\n=== COMPLETE LONPF2 DEPENDENCY SUMMARY ===')
    print(f'Program: LONPF2.CBL')
    print(f'Location: S35-Source/LP/LONPF2.CBL')
    print(f'\nExternal Programs Called: {len(external_programs)}')
    for prog in external_programs:
        print(f'  - {prog}')
    
    print(f'\nCopybooks Used: {len(copybooks)}')
    for copybook in sorted(copybooks):
        print(f'  - {copybook}')
    
    print(f'\nTotal Dependencies: {len(external_programs) + len(copybooks)}')
    
    return {
        'external_programs': external_programs,
        'copybooks': sorted(copybooks),
        'total_dependencies': len(external_programs) + len(copybooks)
    }

if __name__ == "__main__":
    deps = complete_dependency_analysis()
    
    print(f'\n=== DEPENDENCY BREAKDOWN ===')
    print(f'External Programs: {len(deps["external_programs"])}')
    print(f'Copybooks: {len(deps["copybooks"])}')
    print(f'Total: {deps["total_dependencies"]}')
    
    print(f'\n=== NEXT STEPS FOR DEEPER ANALYSIS ===')
    print(f'1. Analyze dependencies of FORM-PROGX')
    print(f'2. Check which other programs use these same copybooks')
    print(f'3. Build a complete dependency tree for the entire system')
