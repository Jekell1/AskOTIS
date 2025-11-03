"""
Index the 8 missing .CBL programs to achieve 100% coverage.
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from pathlib import Path
import json
from typing import List, Dict

# Load credentials
with open('local.settings.json') as f:
    creds = json.load(f)['Values']

SEARCH_ENDPOINT = creds['AZURE_SEARCH_ENDPOINT']
SEARCH_KEY = creds['AZURE_SEARCH_KEY']

code_client = SearchClient(SEARCH_ENDPOINT, 'new_code_chunks', AzureKeyCredential(SEARCH_KEY))

# Missing programs identified
MISSING_PROGRAMS = [
    'BHMAIN',
    'BYINQ',
    'LCAPF2',
    'LCAPGA',
    'LCAPGC',
    'LCAPHC',
    'LNAGED',
    'RZMAIN'
]

def find_file_in_filesystem(program_name: str) -> Path:
    """Find the .CBL file in the filesystem."""
    cobol_src = Path('cobol_src')
    
    # Try common patterns
    patterns = [
        f'{program_name}.CBL',
        f'{program_name}.cbl',
        f'{program_name.lower()}.cbl',
        f'{program_name.upper()}.CBL'
    ]
    
    for pattern in patterns:
        matches = list(cobol_src.rglob(pattern))
        if matches:
            return matches[0]
    
    return None

def chunk_file_content(file_path: Path, chunk_size: int = 60, stride: int = 30) -> List[Dict]:
    """Chunk file content with sliding window."""
    import hashlib
    from datetime import datetime
    
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"  ❌ Error reading {file_path}: {e}")
        return []
    
    chunks = []
    total_lines = len(lines)
    file_id = file_path.stem
    path_str = str(file_path.relative_to('cobol_src')).replace('\\', '/')
    
    # Create sliding window chunks
    start = 0
    window_idx = 0
    
    while start < total_lines:
        end = min(start + chunk_size, total_lines)
        chunk_text = ''.join(lines[start:end])
        
        # Generate unique chunk_id using hash
        chunk_id = hashlib.sha1(f"{file_id}_{start}".encode()).hexdigest()
        
        chunk_doc = {
            'chunk_id': chunk_id,
            'file_id': file_id,
            'path': path_str,
            'program_id': file_id,
            'scope': 'file',
            'name': file_path.name,
            'start_line': str(start + 1),  # String type
            'end_line': str(end),           # String type
            'text': chunk_text,
            'has_vector': False,
            'window_index': str(window_idx),  # String type
            'window_size': str(chunk_size),    # String type
            'stride': str(stride),             # String type
            'created_at': datetime.utcnow().isoformat() + 'Z',
            'tokens_estimate': str(len(chunk_text.split()))  # String type
        }
        
        chunks.append(chunk_doc)
        window_idx += 1
        
        # Move by stride
        start += stride
        
        # If we're at the end but haven't captured it fully, do one more chunk
        if start < total_lines and start + stride >= total_lines and end < total_lines:
            start = total_lines - chunk_size if total_lines > chunk_size else 0
    
    return chunks

print('=' * 80)
print('INDEXING MISSING PROGRAMS')
print('=' * 80)

all_chunks = []
found_programs = []
not_found = []

for program in MISSING_PROGRAMS:
    print(f'\nSearching for {program}...')
    file_path = find_file_in_filesystem(program)
    
    if file_path:
        print(f'  ✓ Found: {file_path}')
        chunks = chunk_file_content(file_path)
        
        if chunks:
            print(f'  ✓ Created {len(chunks)} chunks')
            all_chunks.extend(chunks)
            found_programs.append(program)
        else:
            print(f'  ❌ Failed to create chunks')
            not_found.append(program)
    else:
        print(f'  ❌ File not found in cobol_src/')
        not_found.append(program)

print('\n' + '=' * 80)
print('UPLOAD TO AZURE SEARCH')
print('=' * 80)

if all_chunks:
    print(f'\nUploading {len(all_chunks)} chunks from {len(found_programs)} programs...')
    
    try:
        # Upload in batches
        batch_size = 100
        for i in range(0, len(all_chunks), batch_size):
            batch = all_chunks[i:i+batch_size]
            result = code_client.upload_documents(documents=batch)
            
            succeeded = sum(1 for r in result if r.succeeded)
            failed = sum(1 for r in result if not r.succeeded)
            
            print(f'  Batch {i//batch_size + 1}: {succeeded} succeeded, {failed} failed')
            
            if failed > 0:
                for r in result:
                    if not r.succeeded:
                        print(f'    ❌ Failed: {r.key} - {r.error_message}')
        
        print(f'\n✅ Upload complete!')
        
    except Exception as e:
        print(f'\n❌ Upload failed: {e}')
else:
    print('\n⚠️ No chunks to upload')

print('\n' + '=' * 80)
print('SUMMARY')
print('=' * 80)
print(f'Programs found and indexed: {len(found_programs)}')
if found_programs:
    for prog in found_programs:
        print(f'  ✓ {prog}')

print(f'\nPrograms not found: {len(not_found)}')
if not_found:
    for prog in not_found:
        print(f'  ❌ {prog}')

print(f'\nTotal chunks uploaded: {len(all_chunks)}')

if len(found_programs) == len(MISSING_PROGRAMS):
    print('\n🎉 100% COVERAGE ACHIEVED!')
else:
    print(f'\n⚠️ {len(not_found)} programs still missing')
