#!/usr/bin/env python3
"""
Parallel version of copybook indexing - processes files concurrently.
"""

import json
import requests
import hashlib
from pathlib import Path
from datetime import datetime
from typing import List, Dict
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

SEARCH_ENDPOINT = settings['AZURE_SEARCH_ENDPOINT']
SEARCH_KEY = settings['AZURE_SEARCH_KEY']
OPENAI_ENDPOINT = settings['AZURE_OPENAI_ENDPOINT']
OPENAI_KEY = settings['AZURE_OPENAI_KEY']
EMBED_DEPLOYMENT = settings['AZURE_OPENAI_EMBEDDING_DEPLOYMENT']

# Thread-safe counters
lock = threading.Lock()
processed_count = 0
failed_count = 0
chunk_count = 0

print("=" * 80)
print("PARALLEL COPYBOOK INDEXING")
print("=" * 80)

# Step 1: Get list of already indexed files
print("\nStep 1: Getting already indexed files...")

url = f"{SEARCH_ENDPOINT}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"
body = {
    'search': '*',
    'facets': ['path,count:20000'],
    'top': 0
}

r = requests.post(url, headers={'api-key': SEARCH_KEY, 'Content-Type': 'application/json'}, json=body)
indexed_paths = {f['value'].replace('\\', '/').upper() for f in r.json().get('@search.facets', {}).get('path', [])}

print(f"Found {len(indexed_paths)} already indexed files")

# Step 2: Get all .CPY files from cobol_src
print("\nStep 2: Finding .CPY files in cobol_src...")

cobol_src = Path('cobol_src')
cpy_files = list(cobol_src.rglob('*.CPY')) + list(cobol_src.rglob('*.cpy'))

print(f"Found {len(cpy_files)} .CPY files in cobol_src")

# Step 3: Find missing files
missing_files = []
for f in cpy_files:
    rel_path = f.relative_to(cobol_src)
    index_path = f"cobol_src/{rel_path}".replace('\\', '/')
    
    if index_path.upper() not in indexed_paths:
        missing_files.append(f)

print(f"\nFound {len(missing_files)} missing copybook files to index")

if not missing_files:
    print("\n✅ All copybook files are already indexed!")
    exit(0)

# Ask for confirmation
print("\n" + "=" * 80)
print("CONFIRMATION")
print("=" * 80)
print(f"""
This script will:
1. Chunk {len(missing_files)} copybook files (60-line windows, 30-line stride)
2. Generate embeddings in parallel using {min(10, len(missing_files))} threads
3. Upload to new_code_chunks index in batches

Estimated time: {len(missing_files) * 0.1 / 60:.1f} minutes (10x faster with parallelization)
""")

response = input("Proceed? (yes/no): ")
if response.lower() != 'yes':
    print("Aborted.")
    exit(0)

# Helper functions
def chunk_file(file_path: Path, window_size: int = 60, stride: int = 30) -> List[Dict]:
    """Chunk a copybook file into overlapping windows."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
    except Exception as e:
        return []
    
    chunks = []
    file_id = hashlib.sha1(str(file_path).encode()).hexdigest()
    rel_path = file_path.relative_to(cobol_src)
    path_str = f"cobol_src/{rel_path}".replace('\\', '/')
    
    for i in range(0, len(lines), stride):
        window = lines[i:i + window_size]
        if not window:
            continue
        
        text = ''.join(window)
        start_line = i + 1
        end_line = min(i + window_size, len(lines))
        
        chunk_id = hashlib.sha1(f"{file_id}_{i}".encode()).hexdigest()
        
        chunk = {
            'chunk_id': chunk_id,
            'file_id': file_id,
            'path': path_str,
            'program_id': file_id,
            'scope': 'file',
            'name': file_path.name,
            'start_line': start_line,
            'end_line': end_line,
            'text': text,
            'has_vector': False,
            'window_index': i // stride,
            'window_size': window_size,
            'stride': stride,
            'created_at': datetime.utcnow().isoformat() + 'Z',
            'tokens_estimate': len(text.split())
        }
        
        chunks.append(chunk)
    
    return chunks

def generate_embedding(text: str) -> List[float]:
    """Generate embedding using Azure OpenAI."""
    url = f"{OPENAI_ENDPOINT}/openai/deployments/{EMBED_DEPLOYMENT}/embeddings?api-version=2024-02-01"
    
    body = {
        'input': text,
        'encoding_format': 'float'
    }
    
    headers = {
        'api-key': OPENAI_KEY,
        'Content-Type': 'application/json'
    }
    
    r = requests.post(url, headers=headers, json=body, timeout=30)
    r.raise_for_status()
    
    return r.json()['data'][0]['embedding']

def process_file(file_path: Path) -> List[Dict]:
    """Process a single file: chunk + embed."""
    global processed_count, failed_count, chunk_count
    
    try:
        # Chunk the file
        chunks = chunk_file(file_path)
        
        if not chunks:
            with lock:
                failed_count += 1
            return []
        
        # Generate embeddings for each chunk
        for chunk in chunks:
            try:
                embedding = generate_embedding(chunk['text'])
                chunk['text_vector'] = embedding
                chunk['has_vector'] = True
            except Exception as e:
                # Keep chunk without vector
                pass
        
        with lock:
            processed_count += 1
            chunk_count += len(chunks)
        
        return chunks
        
    except Exception as e:
        with lock:
            failed_count += 1
        return []

def upload_batch(chunks: List[Dict]):
    """Upload chunks to new_code_chunks index."""
    url = f"{SEARCH_ENDPOINT}/indexes/new_code_chunks/docs/index?api-version=2025-08-01-preview"
    
    headers = {
        'api-key': SEARCH_KEY,
        'Content-Type': 'application/json'
    }
    
    actions = []
    for chunk in chunks:
        action = {
            '@search.action': 'mergeOrUpload',
            **chunk
        }
        actions.append(action)
    
    body = {'value': actions}
    
    r = requests.post(url, headers=headers, json=body, timeout=60)
    r.raise_for_status()
    
    return r.json()

# Step 4: Process files in parallel
print("\n" + "=" * 80)
print("PROCESSING COPYBOOKS IN PARALLEL")
print("=" * 80)

batch_chunks = []
max_workers = 10  # Number of parallel threads

with ThreadPoolExecutor(max_workers=max_workers) as executor:
    # Submit all files for processing
    future_to_file = {executor.submit(process_file, f): f for f in missing_files}
    
    # Process completed files as they finish
    for future in as_completed(future_to_file):
        file_path = future_to_file[future]
        
        try:
            chunks = future.result()
            
            if chunks:
                batch_chunks.extend(chunks)
                
                # Upload in batches of 100
                if len(batch_chunks) >= 100:
                    print(f"\r[{processed_count}/{len(missing_files)}] Uploading batch of {len(batch_chunks)} chunks...", end="", flush=True)
                    upload_batch(batch_chunks)
                    batch_chunks = []
                    print(" ✓")
                
                # Progress update every 50 files
                if processed_count % 50 == 0:
                    print(f"\n  Progress: {processed_count} files, {chunk_count} chunks, {failed_count} failed")
            
        except Exception as e:
            print(f"\n  ❌ Error processing {file_path.name}: {e}")

# Upload remaining chunks
if batch_chunks:
    print(f"\nUploading final batch of {len(batch_chunks)} chunks...")
    upload_batch(batch_chunks)

# Summary
print("\n" + "=" * 80)
print("INDEXING COMPLETE")
print("=" * 80)

print(f"""
Files processed: {processed_count}/{len(missing_files)}
Files failed: {failed_count}
Total chunks created: {chunk_count}

Status: {'✅ SUCCESS' if failed_count == 0 else f'⚠️ PARTIAL ({failed_count} failures)'}
""")

print("\nNext steps:")
print("  1. Wait 1-2 minutes for index to update")
print("  2. Test with: python check_unique_files.py")
print("  3. Verify coverage is now close to 100%")
