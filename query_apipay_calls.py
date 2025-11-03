"""Query the calls index directly to find what APIPAY calls."""

import json
import requests

# Load config
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']

# Search the calls index for APIPAY - get ALL calls
url = f'{endpoint}/indexes/new_cobol_calls/docs/search?api-version=2025-08-01-preview'
headers = {'api-key': key, 'Content-Type': 'application/json'}
body = {
    'filter': "caller_program eq 'APIPAY'",
    'select': 'caller_program,callee_program,line,snippet',
    'top': 100  # Get up to 100 calls
}

response = requests.post(url, headers=headers, json=body)
results = response.json().get('value', [])

print('=' * 80)
print('PROGRAMS CALLED BY APIPAY')
print('=' * 80)
print()

if results:
    print(f'Found {len(results)} call(s) from APIPAY:')
    print()
    
    # Get unique programs called
    unique_programs = {}
    for doc in results:
        callee = doc.get('callee_program', '???')
        line = doc.get('line', '???')
        
        if callee not in unique_programs:
            unique_programs[callee] = []
        unique_programs[callee].append(line)
    
    # Show summary
    print(f'APIPAY calls {len(unique_programs)} unique program(s):')
    print('=' * 60)
    for program, lines in sorted(unique_programs.items()):
        print(f'\n{program}')
        print(f'  Called {len(lines)} time(s) at line(s): {", ".join(map(str, sorted(lines)))}')
    
    print()
    print('=' * 60)
    print('DETAILED CALLS:')
    print('=' * 60)
    
    for i, doc in enumerate(results, 1):
        callee = doc.get('callee_program', '???')
        line = doc.get('line', '???')
        print(f'\n{i}. Line {line}: CALL {callee}')
        
        snippet = doc.get('snippet', '')
        if snippet:
            print(f'   {snippet[:120]}...')
    print()
else:
    print('No results found in calls index')
