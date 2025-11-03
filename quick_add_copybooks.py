"""Quick script to add copybooks to new_code_chunks WITHOUT embeddings.
Embeddings can be added later via backfill script.
"""
import os, json, requests, hashlib
from pathlib import Path
from typing import List, Dict, Any

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('SEARCH_ENDPOINT', os.getenv('AZURE_SEARCH_ENDPOINT')).rstrip('/')
KEY = os.getenv('SEARCH_KEY', os.getenv('AZURE_SEARCH_KEY'))
API_VERSION = '2024-07-01'
INDEX_NAME = 'new_code_chunks'

HEADERS = {
    'Content-Type': 'application/json',
    'api-key': KEY
}

LINES_PER_CHUNK = 1500
OVERLAP_LINES = 300

def sha1_bytes(data: bytes) -> str:
    return hashlib.sha1(data).hexdigest()

def chunk_file_by_lines(text: str, lines_per_chunk: int = 1500, overlap: int = 300) -> List[Dict]:
    """Chunk text by lines"""
    lines = text.split('\n')
    chunks = []
    start = 0
    
    while start < len(lines):
        end = min(start + lines_per_chunk, len(lines))
        chunk_lines = lines[start:end]
        chunk_text = '\n'.join(chunk_lines)
        
        chunks.append({
            'start_line': start + 1,
            'end_line': end,
            'text': chunk_text
        })
        
        if end >= len(lines):
            break
        
        start = end - overlap
        if start >= end:
            break
    
    return chunks

def discover_copybook_files(root_path: str = 'cobol_src') -> List[Path]:
    """Find all .CPY files"""
    root = Path(root_path)
    cpy_files = list(root.rglob('*.CPY'))
    print(f'Found {len(cpy_files)} .CPY files')
    return sorted(cpy_files)

def upload_batch(chunks: List[Dict]) -> bool:
    """Upload a batch of chunks"""
    if not chunks:
        return True
    
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}'
    
    docs = []
    for chunk in chunks:
        docs.append({
            '@search.action': 'mergeOrUpload',
            **chunk
        })
    
    payload = {'value': docs}
    r = requests.post(url, headers=HEADERS, json=payload, timeout=120)
    
    if r.status_code >= 300:
        print(f'ERROR: Upload failed {r.status_code}: {r.text[:500]}')
        return False
    
    return True

def main():
    print('=' * 80)
    print(f'QUICK ADD COPYBOOKS TO {INDEX_NAME}')
    print('=' * 80)
    print()
    
    # Discover files
    copybooks = discover_copybook_files('cobol_src')
    
    if not copybooks:
        print('ERROR: No copybook files found!')
        return
    
    print(f'Ready to process {len(copybooks)} copybook files')
    print()
    
    response = input('Proceed? (yes/no): ')
    if response.lower() != 'yes':
        print('Aborted.')
        return
    
    print()
    print('Processing...')
    print()
    
    upload_buffer = []
    total_chunks = 0
    batch_size = 100
    
    for idx, cpy_file in enumerate(copybooks, 1):
        try:
            data = cpy_file.read_bytes()
            file_id = sha1_bytes(data)
            
            try:
                text = data.decode('utf-8', 'replace')
            except:
                text = data.decode('latin-1', 'replace')
            
            chunks = chunk_file_by_lines(text, LINES_PER_CHUNK, OVERLAP_LINES)
            
            for chunk_idx, chunk_data in enumerate(chunks):
                chunk_id = f'{file_id}_{chunk_idx}'
                
                doc = {
                    'chunk_id': chunk_id,
                    'file_id': file_id,
                    'program_id': file_id,
                    'name': cpy_file.name,
                    'path': str(cpy_file),
                    'scope': 'file',
                    'start_line': chunk_data['start_line'],
                    'end_line': chunk_data['end_line'],
                    'text': chunk_data['text'],
                    'window_index': chunk_idx,
                    'window_size': LINES_PER_CHUNK,
                    'stride': LINES_PER_CHUNK - OVERLAP_LINES,
                    'has_vector': False,
                    'tokens_estimate': int(len(chunk_data['text']) / 4)
                }
                
                upload_buffer.append(doc)
                total_chunks += 1
            
            # Upload batch
            if len(upload_buffer) >= batch_size:
                if upload_batch(upload_buffer):
                    print(f'Progress: {idx}/{len(copybooks)} files, {total_chunks:,} chunks', flush=True)
                else:
                    print(f'ERROR: Upload failed at file {idx}')
                    return
                upload_buffer.clear()
        
        except Exception as e:
            print(f'ERROR processing {cpy_file.name}: {e}')
    
    # Upload remaining
    if upload_buffer:
        if upload_batch(upload_buffer):
            print(f'Uploaded final {len(upload_buffer)} chunks')
        else:
            print('ERROR: Final upload failed')
            return
    
    print()
    print('=' * 80)
    print('COMPLETE!')
    print('=' * 80)
    print(f'  Files processed: {len(copybooks):,}')
    print(f'  Chunks created: {total_chunks:,}')
    print()
    print('NOTE: Embeddings NOT generated (has_vector=False)')
    print('      Text search will work immediately')
    print('      Run embedding backfill later if needed for semantic search')
    print('=' * 80)

if __name__ == '__main__':
    main()
