"""
Add copybook chunks to the code-chunks index.

This script chunks all .CPY (copybook) files and adds them to the existing
code-chunks index alongside the .CBL program chunks.

WHY: LLMs need copybook source code to understand data structures referenced
     in programs. Without copybooks, LLMs see "COPY LIBGB/GB01SE.CPY" but
     don't know what fields or structures it defines.

PROCESS:
1. Discover all .CPY files in cobol_src (should find ~8,211 files)
2. Chunk each file into ~25-line segments
3. Upload chunks to code-chunks index
4. Generate embeddings for all chunks
5. Verify completion

ESTIMATED:
- Files: ~8,211 copybooks
- Chunks: ~484,000 new chunks (avg 59 per file)
- Time: 30min chunking + 4-6 hours embedding
- Cost: ~$150-200 for embeddings
"""

import os
import json
import sys
import hashlib
import time
import requests
from pathlib import Path
from typing import List, Dict, Any

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
KEY = os.getenv('AZURE_SEARCH_KEY')
API_VERSION = '2024-07-01'
INDEX_NAME = 'code-chunks'  # Existing legacy index

# Azure OpenAI for embeddings
OPENAI_ENDPOINT = os.getenv('AZURE_OPENAI_ENDPOINT').rstrip('/')
OPENAI_KEY = os.getenv('AZURE_OPENAI_KEY')
EMBED_DEPLOYMENT = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-3-large')

HEADERS = {
    'Content-Type': 'application/json',
    'api-key': KEY
}

# Chunking parameters (match existing chunks in index)
LINES_PER_CHUNK = 25
OVERLAP_LINES = 3

def sha1_bytes(data: bytes) -> str:
    """Generate SHA1 hash for file content"""
    return hashlib.sha1(data).hexdigest()

def chunk_file_by_lines(text: str, lines_per_chunk: int = 25, overlap: int = 3) -> List[Dict]:
    """
    Chunk text by lines (matching existing code-chunks logic).
    Returns list of {'start_line', 'end_line', 'text'} dicts.
    """
    lines = text.split('\n')
    chunks = []
    start = 0
    
    while start < len(lines):
        end = min(start + lines_per_chunk, len(lines))
        chunk_lines = lines[start:end]
        chunk_text = '\n'.join(chunk_lines)
        
        chunks.append({
            'start_line': start + 1,  # 1-indexed
            'end_line': end,
            'text': chunk_text
        })
        
        if end >= len(lines):
            break
        
        # Move forward with overlap
        start = end - overlap
        if start >= end:
            break
    
    return chunks

def get_existing_copybook_count() -> int:
    """Check how many copybook chunks already exist"""
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
    
    # Try to find .CPY files (check multiple ways since paths vary)
    body = {
        'search': 'CPY',
        'searchFields': 'path',
        'top': 0,
        'count': True
    }
    
    r = requests.post(url, headers=HEADERS, json=body)
    if r.status_code == 200:
        return r.json().get('@odata.count', 0)
    return 0

def discover_copybook_files(root_path: str = 'cobol_src') -> List[Path]:
    """Find all .CPY files"""
    root = Path(root_path)
    if not root.exists():
        print(f'ERROR: {root_path} does not exist!')
        sys.exit(1)
    
    cpy_files = list(root.rglob('*.CPY'))
    print(f'Found {len(cpy_files)} .CPY files in {root_path}')
    return sorted(cpy_files)

def upload_chunks_batch(chunks: List[Dict[str, Any]]) -> bool:
    """Upload a batch of chunks to Azure Search"""
    if not chunks:
        return True
    
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/index?api-version={API_VERSION}'
    
    # Prepare documents with mergeOrUpload action
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
    
    result = r.json()
    failed = [v for v in result.get('value', []) if not v.get('status')]
    if failed:
        print(f'WARNING: {len(failed)} chunks failed to upload')
        print(f'Failed samples: {failed[:3]}')
        return False
    
    return True

def generate_embeddings(texts: List[str], batch_size: int = 16) -> List[List[float]]:
    """Generate embeddings for text chunks"""
    url = f'{OPENAI_ENDPOINT}/openai/deployments/{EMBED_DEPLOYMENT}/embeddings?api-version=2024-02-15-preview'
    
    all_embeddings = []
    
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i+batch_size]
        
        payload = {
            'input': batch,
            'dimensions': 1536  # CRITICAL: Match index dimension
        }
        
        headers = {
            'api-key': OPENAI_KEY,
            'Content-Type': 'application/json'
        }
        
        max_retries = 5
        for attempt in range(max_retries):
            r = requests.post(url, headers=headers, json=payload, timeout=120)
            
            if r.status_code == 200:
                data = r.json().get('data', [])
                for item in data:
                    all_embeddings.append(item['embedding'])
                break
            elif r.status_code == 429:
                wait = 2 ** attempt + 0.5
                print(f'  Rate limited, waiting {wait:.1f}s...', end='', flush=True)
                time.sleep(wait)
            else:
                print(f'ERROR: Embedding failed {r.status_code}: {r.text[:300]}')
                return None
        else:
            print(f'ERROR: Failed after {max_retries} retries')
            return None
    
    return all_embeddings

def process_copybook(file_path: Path, upload_batch: List[Dict], phase: str = 'chunk') -> int:
    """
    Process a single copybook file.
    phase: 'chunk' = just chunk and upload, 'embed' = add embeddings
    Returns number of chunks created.
    """
    try:
        # Read file
        data = file_path.read_bytes()
        file_id = sha1_bytes(data)
        
        try:
            text = data.decode('utf-8', 'replace')
        except:
            text = data.decode('latin-1', 'replace')
        
        # Generate chunks
        chunks = chunk_file_by_lines(text, LINES_PER_CHUNK, OVERLAP_LINES)
        
        # Create documents
        for idx, chunk_data in enumerate(chunks):
            chunk_id = f'{file_id}_{idx}'
            
            doc = {
                'chunk_id': chunk_id,
                'file_id': file_id,
                'program_id': file_id,  # For copybooks, program_id = file_id
                'name': file_path.name,
                'path': str(file_path).replace('\\', '\\\\'),
                'scope': 'file',
                'start_line': chunk_data['start_line'],
                'end_line': chunk_data['end_line'],
                'text': chunk_data['text'],
                'has_vector': False  # Will be updated during embedding phase
            }
            
            upload_batch.append(doc)
        
        return len(chunks)
    
    except Exception as e:
        print(f'ERROR processing {file_path.name}: {e}')
        return 0

def main():
    print('=' * 80)
    print('ADD COPYBOOKS TO code-chunks INDEX')
    print('=' * 80)
    print()
    
    # Check current state
    print('1. Checking current index state...')
    existing_cpy = get_existing_copybook_count()
    print(f'   Current copybook chunks in index: {existing_cpy}')
    print()
    
    # Discover files
    print('2. Discovering copybook files...')
    copybooks = discover_copybook_files('cobol_src')
    
    if not copybooks:
        print('ERROR: No copybook files found!')
        sys.exit(1)
    
    print(f'   Ready to process {len(copybooks)} .CPY files')
    print()
    
    # Confirm
    print('=' * 80)
    print('PLAN:')
    print(f'  - Chunk {len(copybooks):,} copybook files')
    print(f'  - Estimated chunks: ~{len(copybooks) * 59:,} (avg 59 per file)')
    print(f'  - Upload to: {INDEX_NAME}')
    print(f'  - Embeddings: Will be added in separate step')
    print('=' * 80)
    print()
    
    response = input('Proceed with chunking? (yes/no): ')
    if response.lower() != 'yes':
        print('Aborted.')
        sys.exit(0)
    
    print()
    print('3. Chunking and uploading copybooks...')
    print()
    
    upload_buffer = []
    total_chunks = 0
    batch_size = 100  # Upload every 100 chunks
    
    start_time = time.time()
    
    for idx, cpy_file in enumerate(copybooks, 1):
        chunks_added = process_copybook(cpy_file, upload_buffer, phase='chunk')
        total_chunks += chunks_added
        
        # Upload batch
        if len(upload_buffer) >= batch_size:
            if upload_chunks_batch(upload_buffer):
                print(f'Progress: {idx}/{len(copybooks)} files, {total_chunks:,} chunks uploaded', flush=True)
            else:
                print(f'ERROR: Upload failed at file {idx}')
                sys.exit(1)
            upload_buffer.clear()
        
        # Progress update every 100 files
        if idx % 100 == 0:
            elapsed = time.time() - start_time
            rate = idx / elapsed
            remaining = (len(copybooks) - idx) / rate
            print(f'Processed {idx}/{len(copybooks)} files ({total_chunks:,} chunks) - ETA: {remaining/60:.1f} min')
    
    # Upload remaining
    if upload_buffer:
        if upload_chunks_batch(upload_buffer):
            print(f'Uploaded final {len(upload_buffer)} chunks')
        else:
            print('ERROR: Final upload failed')
            sys.exit(1)
    
    elapsed = time.time() - start_time
    
    print()
    print('=' * 80)
    print('CHUNKING COMPLETE!')
    print('=' * 80)
    print(f'  Files processed: {len(copybooks):,}')
    print(f'  Chunks created: {total_chunks:,}')
    print(f'  Time: {elapsed/60:.1f} minutes')
    print(f'  Rate: {len(copybooks)/(elapsed/60):.1f} files/min')
    print()
    print('NEXT STEP: Run embedding backfill script to add vectors')
    print('  Estimated time: 4-6 hours')
    print('  Command: python backfill_copybook_chunks_embeddings.py')
    print('=' * 80)

if __name__ == '__main__':
    main()
