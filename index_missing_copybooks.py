#!/usr/bin/env python3
"""
Index missing .CPY copybook files into new_code_chunks index.

This will chunk and embed the 4,938 missing copybook files to complete coverage.
"""

import json
import requests
import hashlib
from pathlib import Path
from datetime import datetime
from typing import List, Dict

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

SEARCH_ENDPOINT = settings['AZURE_SEARCH_ENDPOINT']
SEARCH_KEY = settings['AZURE_SEARCH_KEY']
OPENAI_ENDPOINT = settings['AZURE_OPENAI_ENDPOINT']
OPENAI_KEY = settings['AZURE_OPENAI_KEY']
EMBED_DEPLOYMENT = settings['AZURE_OPENAI_EMBEDDING_DEPLOYMENT']

print("=" * 80)
print("INDEXING MISSING COPYBOOKS INTO new_code_chunks")
print("=" * 80)

# Step 1: Get list of already indexed files
print("\nStep 1: Getting already indexed files...")

url = f"{SEARCH_ENDPOINT}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"
body = {
    'search': '*',
    'facets': ['path,count:5000'],
    'top': 0
}

r = requests.post(url, headers={'api-key': SEARCH_KEY}, json=body)
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

# Check for checkpoint file to resume
checkpoint_file = Path('_copybook_indexing_checkpoint.txt')
start_index = 0

if checkpoint_file.exists():
    with open(checkpoint_file, 'r') as f:
        start_index = int(f.read().strip())
    print(f"\n📌 Found checkpoint - will resume from file #{start_index}")
    missing_files = missing_files[start_index:]
    print(f"   Remaining files to process: {len(missing_files)}")

# Ask for confirmation
print("\n" + "=" * 80)
print("CONFIRMATION")
print("=" * 80)
print(f"""
This script will:
1. Chunk {len(missing_files)} copybook files (60-line windows, 30-line stride)
2. Generate embeddings for each chunk (~{len(missing_files) * 20} chunks estimated)
3. Upload to new_code_chunks index

Estimated time: {len(missing_files) * 0.5 / 60:.1f} minutes
Estimated cost: ~${len(missing_files) * 20 * 0.0001:.2f} (embedding API calls)
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
        print(f"  ❌ Error reading {file_path}: {e}")
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
            'program_id': file_id,  # Use file_id as program_id for copybooks
            'scope': 'file',
            'name': file_path.name,
            'start_line': start_line,
            'end_line': end_line,
            'text': text,
            'has_vector': False,  # Will be set after embedding
            'window_index': i // stride,
            'window_size': window_size,
            'stride': stride,
            'created_at': datetime.utcnow().isoformat() + 'Z',
            'tokens_estimate': len(text.split())  # Rough estimate
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

def upload_batch(chunks: List[Dict], batch_size: int = 100):
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

# Step 4: Process files
print("\n" + "=" * 80)
print("PROCESSING COPYBOOKS")
print("=" * 80)

total_chunks = 0
processed_files = 0
failed_files = 0

batch_chunks = []

for i, file_path in enumerate(missing_files, 1):
    actual_file_number = start_index + i  # Track actual position in original list
    try:
        print(f"\n[{i}/{len(missing_files)}] {file_path.name}...", end=" ", flush=True)
        
        # Chunk the file
        chunks = chunk_file(file_path)
        
        if not chunks:
            print("❌ No chunks")
            failed_files += 1
            continue
        
        print(f"{len(chunks)} chunks...", end=" ", flush=True)
        
        # Generate embeddings for each chunk
        for chunk in chunks:
            try:
                embedding = generate_embedding(chunk['text'])
                chunk['text_vector'] = embedding
                chunk['has_vector'] = True
            except Exception as e:
                print(f"⚠️ Embedding failed: {e}", end=" ")
                # Keep chunk without vector
        
        batch_chunks.extend(chunks)
        total_chunks += len(chunks)
        processed_files += 1
        
        print(f"✓", end="")
        
        # Upload in batches of 100
        if len(batch_chunks) >= 100:
            print(f" [uploading {len(batch_chunks)} chunks]...", end=" ", flush=True)
            upload_batch(batch_chunks)
            batch_chunks = []
            print("✓")
        
        # Progress checkpoint every 50 files
        if i % 50 == 0:
            print(f"\n  Progress: {processed_files} files, {total_chunks} chunks")
            # Save checkpoint
            with open(checkpoint_file, 'w') as f:
                f.write(str(actual_file_number))
            print(f"  📌 Checkpoint saved at file #{actual_file_number}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
        failed_files += 1

# Upload remaining chunks
if batch_chunks:
    print(f"\nUploading final batch of {len(batch_chunks)} chunks...")
    upload_batch(batch_chunks)

# Summary
print("\n" + "=" * 80)
print("INDEXING COMPLETE")
print("=" * 80)

print(f"""
Files processed: {processed_files}/{len(missing_files)}
Files failed: {failed_files}
Total chunks created: {total_chunks}

Status: {'✅ SUCCESS' if failed_files == 0 else f'⚠️ PARTIAL ({failed_files} failures)'}
""")

# Clean up checkpoint on success
if failed_files == 0 and checkpoint_file.exists():
    checkpoint_file.unlink()
    print("📌 Checkpoint file deleted (completed successfully)")

print("\nNext steps:")
print("  1. Wait 1-2 minutes for index to update")
print("  2. Test with: python _check_source_coverage.py")
print("  3. Verify coverage is now close to 100%")
