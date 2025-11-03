"""Check if APIPAY file is in the files index and compare dates."""

import json, requests
from pathlib import Path
import datetime

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']

print('=' * 80)
print('WHY APIPAY IS NOT INDEXED')
print('=' * 80)
print()

# Check actual file first
file_path = Path('cobol_src/SP/APIPAY.CBL')
if file_path.exists():
    mtime = file_path.stat().st_mtime
    mod_date = datetime.datetime.fromtimestamp(mtime)
    print('ACTUAL FILE ON DISK:')
    print(f'  Path: {file_path}')
    print(f'  Modified: {mod_date.strftime("%Y-%m-%d %H:%M:%S")}')
    print(f'  Size: {file_path.stat().st_size:,} bytes')
    print()

# Search files index for APIPAY
url = f'{endpoint}/indexes/new-cobol-files/docs/search?api-version=2025-08-01-preview'
headers = {'api-key': key, 'Content-Type': 'application/json'}
body = {
    'filter': "file_path eq 'cobol_src/SP/APIPAY.CBL'",
    'select': 'file_id,file_path,file_type,last_modified,size_bytes,ingested_at',
    'top': 5
}

response = requests.post(url, headers=headers, json=body)
results = response.json().get('value', [])

print('FILES INDEX (new-cobol-files):')
if results:
    for doc in results:
        print(f"  File: {doc.get('file_path', 'N/A')}")
        print(f"  Type: {doc.get('file_type', 'N/A')}")
        print(f"  Modified: {doc.get('last_modified', 'N/A')}")
        print(f"  Ingested: {doc.get('ingested_at', 'N/A')}")
        size = doc.get('size_bytes', 0)
        print(f"  Size: {size:,} bytes")
else:
    print('  ❌ APIPAY.CBL not found in files index')
print()

# Check program_meta for APIPAY
url = f'{endpoint}/indexes/new_cobol_program_meta/docs/search?api-version=2025-08-01-preview'
body = {
    'filter': "program_name eq 'APIPAY'",
    'select': 'program_name,file_path,ingested_at',
    'top': 1
}

response = requests.post(url, headers=headers, json=body)
results = response.json().get('value', [])

print('PROGRAM META INDEX (new_cobol_program_meta):')
if results:
    for doc in results:
        print(f"  Program: {doc.get('program_name', 'N/A')}")
        print(f"  File: {doc.get('file_path', 'N/A')}")
        print(f"  Ingested: {doc.get('ingested_at', 'N/A')}")
else:
    print('  ❌ APIPAY not found in program_meta index')
print()

# Check when indexing generally ran (sample from code-chunks)
print('=' * 80)
print('GENERAL INDEX TIMESTAMPS')
print('=' * 80)
print()

url = f'{endpoint}/indexes/code-chunks/docs/search?api-version=2025-08-01-preview'
body = {'search': '*', 'top': 1, 'select': 'ingested_at', 'orderby': 'ingested_at desc'}
response = requests.post(url, headers=headers, json=body)
sample = response.json().get('value', [])
if sample:
    print(f"code-chunks most recent ingestion: {sample[0].get('ingested_at', 'N/A')}")

url = f'{endpoint}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview'
body = {'search': '*', 'top': 1, 'select': 'ingested_at', 'orderby': 'ingested_at desc'}
response = requests.post(url, headers=headers, json=body)
sample = response.json().get('value', [])
if sample:
    print(f"new_code_chunks most recent ingestion: {sample[0].get('ingested_at', 'N/A')}")

print()
print('=' * 80)
print('DIAGNOSIS')
print('=' * 80)
print()

if file_path.exists() and not results:
    print('CONCLUSION: APIPAY.CBL exists on disk but was NOT indexed!')
    print()
    print('Possible reasons:')
    print('  1. File was created/modified AFTER indexing ran')
    print('  2. File was in SP/ subdirectory which may have been excluded')
    print('  3. Indexing script had a filter that excluded this file')
    print('  4. Indexing failed for this specific file')
    print()
    print('Next steps:')
    print('  • Check when code chunking script last ran')
    print('  • Check if SP/ directory is in the indexing path')
    print('  • Re-run code chunking to include this file')
