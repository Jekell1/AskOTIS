"""
Index all remaining CBL and CPY files in parallel with 10 workers.
"""
import os
import sys
import threading
from pathlib import Path
from typing import List, Dict
from concurrent.futures import ThreadPoolExecutor, as_completed

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'otis_rag'))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from azure.core.exceptions import HttpResponseError
from openai import AzureOpenAI
from config import Config

# Thread-safe counters
lock = threading.Lock()
processed_count = 0
failed_count = 0
chunk_count = 0
skipped_count = 0

config = Config()

# Setup Azure Search client
search_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name=config.get_index_name("code_chunks"),
    credential=AzureKeyCredential(config.search_key)
)

# Setup Azure OpenAI client for embeddings
openai_client = AzureOpenAI(
    api_key=config.openai_key,
    api_version="2024-08-01-preview",
    azure_endpoint=config.openai_endpoint
)

print("=" * 80)
print("PARALLEL INDEXING - ALL REMAINING FILES")
print("=" * 80)

# Step 1: Get already indexed files
print("\nStep 1: Getting already indexed files...")
try:
    # Use facets with high limit
    results = search_client.search(
        search_text="*",
        select=["path"],
        facets=["path,count:20000"],
        top=0
    )
    facets = results.get_facets()
    indexed_files = set()
    if facets and 'path' in facets:
        for facet in facets['path']:
            indexed_files.add(facet['value'])
    
    print(f"Found {len(indexed_files)} already indexed files")
except Exception as e:
    print(f"Error getting indexed files: {e}")
    print("Starting with empty set (will create duplicates)")
    indexed_files = set()

# Step 2: Find all source files
print("\nStep 2: Finding all .CBL and .CPY files in cobol_src...")
cobol_src = Path("cobol_src")
all_files = sorted(list(cobol_src.rglob("*.CBL")) + list(cobol_src.rglob("*.CPY")))
print(f"Found {len(all_files)} total source files")

# Step 3: Filter to missing files
missing_files = []
for file_path in all_files:
    relative_path = str(file_path).replace('\\', '\\')
    if relative_path not in indexed_files:
        missing_files.append(file_path)

print(f"\nFound {len(missing_files)} missing files to index")
if not missing_files:
    print("\n✅ All files are already indexed!")
    sys.exit(0)

cbl_missing = sum(1 for f in missing_files if f.suffix == '.CBL')
cpy_missing = sum(1 for f in missing_files if f.suffix == '.CPY')
print(f"  - .CBL files: {cbl_missing}")
print(f"  - .CPY files: {cpy_missing}")

def chunk_file(file_path: Path) -> List[Dict]:
    """Chunk a single file into 60-line chunks with 30-line overlap."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
        
        if not lines:
            return []
        
        chunks = []
        chunk_size = 60
        stride = 30  # 50% overlap
        
        relative_path = str(file_path).replace('\\', '\\')
        file_id = str(file_path.stem)
        program_id = file_path.stem  # Filename without extension
        
        for i in range(0, len(lines), stride):
            chunk_lines = lines[i:i + chunk_size]
            if not chunk_lines:
                continue
            
            start_line = i + 1
            end_line = i + len(chunk_lines)
            
            chunk_text = ''.join(chunk_lines)
            chunk_id = f"{file_id}_{start_line}_{end_line}"
            
            chunk = {
                'chunk_id': chunk_id,
                'file_id': file_id,
                'path': relative_path,
                'program_id': program_id,
                'name': file_path.name,
                'start_line': start_line,
                'end_line': end_line,
                'text': chunk_text,
                'has_vector': False
            }
            chunks.append(chunk)
        
        return chunks
    
    except Exception as e:
        print(f"\n❌ Error chunking {file_path}: {e}")
        return []

def generate_embedding(text: str) -> List[float]:
    """Generate embedding for text using Azure OpenAI."""
    try:
        # Truncate to ~8000 chars to stay within token limits
        text = text[:8000]
        
        response = openai_client.embeddings.create(
            input=text,
            model=config.embed_deployment
        )
        return response.data[0].embedding
    except Exception as e:
        print(f"\n❌ Embedding error: {e}")
        return []

def process_file(file_path: Path) -> List[Dict]:
    """Process a single file: chunk it and generate embeddings."""
    global processed_count, failed_count, chunk_count, skipped_count
    
    try:
        # Chunk the file
        chunks = chunk_file(file_path)
        
        if not chunks:
            with lock:
                skipped_count += 1
            return []
        
        # Generate embeddings for each chunk
        for chunk in chunks:
            embedding = generate_embedding(chunk['text'])
            if embedding:
                chunk['text_vector'] = embedding
                chunk['has_vector'] = True
            else:
                with lock:
                    failed_count += 1
                return []  # Skip this file if embedding fails
        
        with lock:
            processed_count += 1
            chunk_count += len(chunks)
        
        return chunks
    
    except Exception as e:
        with lock:
            failed_count += 1
        print(f"\n❌ Error processing {file_path}: {e}")
        return []

def upload_batch(chunks: List[Dict]):
    """Upload a batch of chunks to Azure Search."""
    if not chunks:
        return
    
    try:
        result = search_client.upload_documents(documents=chunks)
        success_count = sum(1 for r in result if r.succeeded)
        failed_count = sum(1 for r in result if not r.succeeded)
        
        if failed_count > 0:
            print(f"\n⚠️ Batch upload: {success_count} succeeded, {failed_count} failed")
    
    except HttpResponseError as e:
        print(f"\n❌ Upload batch failed: {e}")
    except Exception as e:
        print(f"\n❌ Unexpected upload error: {e}")

print("\n" + "=" * 80)
print("STARTING PARALLEL INDEXING")
print("=" * 80)
print(f"Workers: 10")
print(f"Files to process: {len(missing_files)}")
print()

# Process files in parallel
batch_chunks = []
max_workers = 10

with ThreadPoolExecutor(max_workers=max_workers) as executor:
    # Submit all file processing tasks
    future_to_file = {executor.submit(process_file, f): f for f in missing_files}
    
    # Process completed tasks
    for future in as_completed(future_to_file):
        file_path = future_to_file[future]
        
        try:
            chunks = future.result()
            
            if chunks:
                batch_chunks.extend(chunks)
                
                # Upload in batches of 100
                if len(batch_chunks) >= 100:
                    print(f"\r[{processed_count}/{len(missing_files)}] Uploading batch of {len(batch_chunks)} chunks... ", end='', flush=True)
                    upload_batch(batch_chunks)
                    batch_chunks = []
            
            # Progress update every 50 files
            if processed_count % 50 == 0:
                print(f"\r[{processed_count}/{len(missing_files)}] Processed: {processed_count}, Chunks: {chunk_count}, Failed: {failed_count}", flush=True)
        
        except Exception as e:
            print(f"\n❌ Error getting result for {file_path}: {e}")

# Upload final batch
if batch_chunks:
    print(f"\n\nUploading final batch of {len(batch_chunks)} chunks...")
    upload_batch(batch_chunks)

print("\n" + "=" * 80)
print("INDEXING COMPLETE")
print("=" * 80)
print(f"Files processed: {processed_count}/{len(missing_files)}")
print(f"Files skipped: {skipped_count}")
print(f"Files failed: {failed_count}")
print(f"Total chunks created: {chunk_count:,}")

if failed_count == 0:
    print("\n✅ SUCCESS: All files indexed with embeddings!")
else:
    print(f"\n⚠️ WARNING: {failed_count} files failed")

print("\n" + "=" * 80)
