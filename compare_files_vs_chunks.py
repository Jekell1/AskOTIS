"""
Compare new-cobol-files vs new_code_chunks to determine if they're redundant.
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json') as f:
    creds = json.load(f)['Values']

endpoint = creds['AZURE_SEARCH_ENDPOINT']
key = creds['AZURE_SEARCH_KEY']

print('=' * 80)
print('COMPARING: new-cobol-files vs new_code_chunks')
print('=' * 80)

# Analyze new-cobol-files
print('\n--- INDEX: new-cobol-files ---')
try:
    files_client = SearchClient(endpoint, 'new-cobol-files', AzureKeyCredential(key))
    
    # Get total count
    total = files_client.search(search_text='*', top=0, include_total_count=True).get_count()
    print(f'Total documents: {total:,}')
    
    # Sample documents
    samples = list(files_client.search(search_text='*', top=5))
    print(f'\nSample documents (first 5):')
    for i, doc in enumerate(samples, 1):
        print(f'\n  Document {i}:')
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            if isinstance(value, str) and len(value) > 100:
                print(f'    {key}: {value[:100]}...')
            else:
                print(f'    {key}: {value}')
    
    # Get field names
    if samples:
        fields = [k for k in samples[0].keys() if not k.startswith('@')]
        print(f'\nFields in index: {", ".join(fields)}')
    
except Exception as e:
    print(f'Error: {e}')

# Analyze new_code_chunks
print('\n\n--- INDEX: new_code_chunks ---')
try:
    chunks_client = SearchClient(endpoint, 'new_code_chunks', AzureKeyCredential(key))
    
    # Get total count
    total = chunks_client.search(search_text='*', top=0, include_total_count=True).get_count()
    print(f'Total documents: {total:,}')
    
    # Sample documents
    samples = list(chunks_client.search(search_text='*', top=5))
    print(f'\nSample documents (first 5):')
    for i, doc in enumerate(samples, 1):
        print(f'\n  Document {i}:')
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            if isinstance(value, str) and len(value) > 100:
                print(f'    {key}: {value[:100]}...')
            else:
                print(f'    {key}: {value}')
    
    # Get field names
    if samples:
        fields = [k for k in samples[0].keys() if not k.startswith('@')]
        print(f'\nFields in index: {", ".join(fields)}')
    
except Exception as e:
    print(f'Error: {e}')

# Compare file counts
print('\n\n' + '=' * 80)
print('FILE COVERAGE COMPARISON')
print('=' * 80)

try:
    # Get unique files from new-cobol-files
    files_results = files_client.search(
        search_text='*',
        facets=['name,count:10000'] if 'name' in fields else [],
        top=0
    )
    files_facets = files_results.get_facets()
    files_names = set()
    if 'name' in files_facets:
        for facet in files_facets['name']:
            files_names.add(facet.get('value', ''))
    
    print(f'new-cobol-files unique filenames: {len(files_names):,}')
    
    # Get unique files from new_code_chunks
    chunks_results = chunks_client.search(
        search_text='*',
        facets=['name,count:10000'],
        top=0
    )
    chunks_facets = chunks_results.get_facets()
    chunks_names = set()
    if 'name' in chunks_facets:
        for facet in chunks_facets['name']:
            chunks_names.add(facet.get('value', ''))
    
    print(f'new_code_chunks unique filenames: {len(chunks_names):,}')
    
    # Find differences
    only_in_files = files_names - chunks_names
    only_in_chunks = chunks_names - files_names
    
    print(f'\nFiles ONLY in new-cobol-files: {len(only_in_files):,}')
    if only_in_files and len(only_in_files) <= 20:
        for name in sorted(only_in_files)[:20]:
            print(f'  - {name}')
    
    print(f'\nFiles ONLY in new_code_chunks: {len(only_in_chunks):,}')
    if only_in_chunks and len(only_in_chunks) <= 20:
        for name in sorted(only_in_chunks)[:20]:
            print(f'  - {name}')
    
except Exception as e:
    print(f'Error during comparison: {e}')

print('\n' + '=' * 80)
print('ANALYSIS')
print('=' * 80)
print("""
Key Differences:
1. Document Structure:
   - new-cobol-files: Likely file-level metadata (1 doc per file)
   - new_code_chunks: Code chunks with overlapping windows (multiple docs per file)

2. Document Count:
   - new-cobol-files: ~10k docs (matches ~10k source files)
   - new_code_chunks: ~152k docs (chunked content)

3. Purpose:
   - new-cobol-files: File listing/metadata index
   - new_code_chunks: Source code content for RAG retrieval

4. Redundancy Assessment:
   - If new-cobol-files only has file metadata, it's REDUNDANT (can query chunks)
   - If new-cobol-files has unique data not in chunks, it's NEEDED
   - Need to check what queries use new-cobol-files
""")
