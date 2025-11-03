#!/usr/bin/env python3
"""Verify LPMENU chunks are in index with correct program_id."""

import requests
import os
import json

def load_settings():
    try:
        data = json.load(open('local.settings.json', 'r'))
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT', 'SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except:
        pass

load_settings()

ep = os.getenv('SEARCH_ENDPOINT')
key = os.getenv('SEARCH_KEY')
program_id = 'ea66581c142e6ba08a83d2dc773bc990db403c3c'

print("=" * 80)
print("VERIFY LPMENU IN INDEX")
print("=" * 80)
print(f"\nSearching for program_id: {program_id}")

url = f'{ep}/indexes/new_code_chunks/docs/search?api-version=2024-07-01'
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'filter': f"program_id eq '{program_id}'",
    'select': 'chunk_id,program_id,name,start_line,end_line,text',
    'top': 10
}

r = requests.post(url, headers=headers, json=payload)

if r.status_code == 200:
    results = r.json().get('value', [])
    print(f'\n✅ Found {len(results)} LPMENU chunks with program_id {program_id[:16]}...\n')
    
    if results:
        for i, doc in enumerate(results, 1):
            text_preview = doc.get('text', '')[:100].replace('\n', ' ')
            print(f'  {i}. chunk_id: {doc["chunk_id"][:16]}...')
            print(f'     Lines {doc["start_line"]}-{doc["end_line"]}: {text_preview}...')
            print()
        
        # Check if any contain "M A S T E R"
        master_chunks = [d for d in results if 'M A S T E R' in d.get('text', '').upper()]
        if master_chunks:
            print(f'✅ {len(master_chunks)} chunk(s) contain "M A S T E R"')
            for chunk in master_chunks[:1]:
                print(f'\n📋 Sample content with MASTER MENU:')
                lines = chunk.get('text', '').split('\n')
                for line in lines:
                    if 'M A S T E R' in line.upper():
                        print(f'   {line.strip()}')
        else:
            print('⚠️  No chunks contain "M A S T E R" text')
    else:
        print('⚠️  Documents uploaded but not yet indexed (wait a few more seconds)')
else:
    print(f'❌ Search failed: {r.status_code}')
    print(r.text[:500])

print("\n" + "=" * 80)
