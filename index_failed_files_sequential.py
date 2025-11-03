"""
Sequential indexing for failed files from parallel run.
Adds delay between embedding calls to avoid rate limits.
"""
import os
import sys
import time
from pathlib import Path
from typing import List, Dict

sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'otis_rag'))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from azure.core.exceptions import HttpResponseError
from openai import AzureOpenAI
from config import Config

config = Config()

# Setup Azure Search client
search_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name=config.get_index_name("code_chunks"),
    credential=AzureKeyCredential(config.search_key)
)

# Setup Azure OpenAI client
openai_client = AzureOpenAI(
    api_key=config.openai_key,
    api_version="2024-08-01-preview",
    azure_endpoint=config.openai_endpoint
)

print("=" * 80)
print("SEQUENTIAL INDEXING - RETRY FAILED FILES")
print("=" * 80)

# Get already indexed files
print("\nGetting indexed files...")
try:
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
    print(f"Error: {e}")
    indexed_files = set()

# Find all source files
cobol_src = Path("cobol_src")
all_files = sorted(list(cobol_src.rglob("*.CBL")) + list(cobol_src.rglob("*.CPY")))

# Find missing files
missing_files = []
for file_path in all_files:
    relative_path = str(file_path).replace('\\', '\\')
    if relative_path not in indexed_files:
        missing_files.append(file_path)

print(f"\nFound {len(missing_files)} files still missing from index")

if not missing_files:
    print("✅ All files are indexed!")
    sys.exit(0)

cbl_count = sum(1 for f in missing_files if f.suffix == '.CBL')
cpy_count = sum(1 for f in missing_files if f.suffix == '.CPY')
print(f"  - .CBL: {cbl_count}")
print(f"  - .CPY: {cpy_count}")

def chunk_file(file_path: Path) -> List[Dict]:
    """Chunk a single file."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
        
        if not lines:
            return []
        
        chunks = []
        chunk_size = 60
        stride = 30
        
        relative_path = str(file_path).replace('\\', '\\')
        file_id = str(file_path.stem)
        
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
                'program_id': file_path.stem,
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

def generate_embedding(text: str, retry_count: int = 0) -> List[float]:
    """Generate embedding with retry logic."""
    try:
        text = text[:8000]
        response = openai_client.embeddings.create(
            input=text,
            model=config.embed_deployment
        )
        return response.data[0].embedding
    except Exception as e:
        if "429" in str(e) and retry_count < 3:
            # Rate limited - wait and retry
            wait_time = 2 ** retry_count  # Exponential backoff: 1s, 2s, 4s
            print(f"\n⏳ Rate limited, waiting {wait_time}s...")
            time.sleep(wait_time)
            return generate_embedding(text, retry_count + 1)
        print(f"\n❌ Embedding error: {e}")
        return []

def upload_batch(chunks: List[Dict]):
    """Upload chunks to Azure Search."""
    if not chunks:
        return
    try:
        result = search_client.upload_documents(documents=chunks)
        success = sum(1 for r in result if r.succeeded)
        failed = sum(1 for r in result if not r.succeeded)
        if failed > 0:
            print(f" ({success} ok, {failed} failed)", end='')
    except Exception as e:
        print(f"\n❌ Upload error: {e}")

print("\n" + "=" * 80)
print("STARTING SEQUENTIAL INDEXING")
print("=" * 80)

processed = 0
failed = 0
total_chunks = 0
batch_chunks = []

for i, file_path in enumerate(missing_files, 1):
    print(f"\r[{i}/{len(missing_files)}] Processing {file_path.name}...", end='', flush=True)
    
    try:
        # Chunk the file
        chunks = chunk_file(file_path)
        if not chunks:
            failed += 1
            continue
        
        # Generate embeddings with delay
        all_embeddings_ok = True
        for chunk in chunks:
            embedding = generate_embedding(chunk['text'])
            if not embedding:
                all_embeddings_ok = False
                break
            chunk['text_vector'] = embedding
            chunk['has_vector'] = True
            
            # Small delay between embeddings to avoid rate limits
            time.sleep(0.1)
        
        if not all_embeddings_ok:
            failed += 1
            continue
        
        # Add to batch
        batch_chunks.extend(chunks)
        total_chunks += len(chunks)
        processed += 1
        
        # Upload in batches of 50
        if len(batch_chunks) >= 50:
            print(f"\r[{i}/{len(missing_files)}] Uploading {len(batch_chunks)} chunks...", end='', flush=True)
            upload_batch(batch_chunks)
            batch_chunks = []
        
        # Progress update every 10 files
        if processed % 10 == 0:
            print(f"\r[{i}/{len(missing_files)}] Processed: {processed}, Chunks: {total_chunks}, Failed: {failed}")
    
    except Exception as e:
        print(f"\n❌ Error: {e}")
        failed += 1

# Upload final batch
if batch_chunks:
    print(f"\n\nUploading final batch of {len(batch_chunks)} chunks...")
    upload_batch(batch_chunks)

print("\n" + "=" * 80)
print("INDEXING COMPLETE")
print("=" * 80)
print(f"Files processed: {processed}/{len(missing_files)}")
print(f"Files failed: {failed}")
print(f"Total chunks: {total_chunks:,}")

if failed == 0:
    print("\n✅ SUCCESS: All remaining files indexed!")
else:
    print(f"\n⚠️ {failed} files still failed")
