"""
Index APIPAY.CBL into the code-chunks index.
"""

import json
import requests
from pathlib import Path

# Load config
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT']
key = settings['SEARCH_KEY']
openai_endpoint = settings['AZURE_OPENAI_ENDPOINT']
openai_key = settings['AZURE_OPENAI_KEY']

# Read APIPAY.CBL
file_path = Path('cobol_src/SP/APIPAY.CBL')
with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
    content = f.read()

lines = content.split('\n')
total_lines = len(lines)

print("=" * 80)
print("INDEXING APIPAY.CBL")
print("=" * 80)
print(f"File: {file_path}")
print(f"Total lines: {total_lines}")
print(f"File size: {file_path.stat().st_size:,} bytes")
print()

# Chunk the file (25 lines per chunk with 5 line overlap)
CHUNK_SIZE = 25
OVERLAP = 5
chunks = []

for start in range(0, total_lines, CHUNK_SIZE - OVERLAP):
    end = min(start + CHUNK_SIZE, total_lines)
    chunk_lines = lines[start:end]
    chunk_text = '\n'.join(chunk_lines)
    
    if chunk_text.strip():  # Only include non-empty chunks
        chunks.append({
            'start_line': start + 1,
            'end_line': end,
            'text': chunk_text
        })
    
    if end >= total_lines:
        break

print(f"Created {len(chunks)} chunks")
print()

# Generate embeddings and upload to Azure Search
print("Generating embeddings and uploading to Azure Search...")
print()

from openai import AzureOpenAI

client = AzureOpenAI(
    api_key=openai_key,
    api_version="2024-02-01",
    azure_endpoint=openai_endpoint
)

upload_url = f"{endpoint}/indexes/code-chunks/docs/index?api-version=2024-07-01"
headers = {
    'Content-Type': 'application/json',
    'api-key': key
}

batch_size = 10
success_count = 0

for i in range(0, len(chunks), batch_size):
    batch = chunks[i:i + batch_size]
    
    print(f"Processing batch {i // batch_size + 1}/{(len(chunks) + batch_size - 1) // batch_size}...")
    
    # Generate embeddings for batch
    texts = [chunk['text'] for chunk in batch]
    
    try:
        response = client.embeddings.create(
            input=texts,
            model="text-embedding-3-large"
        )
        
        embeddings = [item.embedding for item in response.data]
        
        # Prepare documents for upload
        documents = []
        for j, chunk in enumerate(batch):
            chunk_id = f"APIPAY_{chunk['start_line']}_{chunk['end_line']}"
            documents.append({
                "@search.action": "mergeOrUpload",
                "chunk_id": chunk_id,
                "file_id": "cobol_src/SP/APIPAY.CBL",
                "path": "cobol_src/SP/APIPAY.CBL",
                "start_line": chunk['start_line'],
                "end_line": chunk['end_line'],
                "text": chunk['text'],
                "text_vector": embeddings[j]
            })
        
        # Upload to Azure Search
        upload_response = requests.post(
            upload_url,
            headers=headers,
            json={"value": documents}
        )
        
        if upload_response.status_code in [200, 201]:
            success_count += len(documents)
            print(f"  ✅ Uploaded {len(documents)} chunks (total: {success_count}/{len(chunks)})")
        else:
            print(f"  ❌ Upload failed: {upload_response.status_code} - {upload_response.text[:200]}")
    
    except Exception as e:
        print(f"  ❌ Error: {e}")

print()
print("=" * 80)
print("INDEXING COMPLETE")
print("=" * 80)
print(f"Successfully indexed: {success_count}/{len(chunks)} chunks")
print()
print("APIPAY.CBL is now searchable in the RAG system!")
