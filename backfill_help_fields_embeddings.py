"""
Backfill embeddings for help_fields index.
"""

import sys
sys.path.insert(0, '.')

import time
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config
from openai import AzureOpenAI

# Load config
config = Config()
INDEX_NAME = "help_fields"

# OpenAI client for embeddings
openai_client = AzureOpenAI(
    api_key=config.openai_key,
    api_version="2024-02-15-preview",
    azure_endpoint=config.openai_endpoint
)


def generate_embedding(text: str) -> list:
    """Generate embedding for text using text-embedding-3-large."""
    try:
        response = openai_client.embeddings.create(
            input=text,
            model=config.embed_deployment_large  # 3072-dim
        )
        return response.data[0].embedding
    except Exception as e:
        print(f"  ⚠️  Embedding error: {e}")
        return None


def backfill_embeddings(batch_size: int = 100, max_docs: int = None):
    """Backfill embeddings for help fields."""
    
    credential = AzureKeyCredential(config.search_key)
    search_client = SearchClient(
        endpoint=config.search_endpoint,
        index_name=INDEX_NAME,
        credential=credential
    )
    
    # Query for documents without vectors
    filter_query = "has_vector eq false"
    
    print(f"Querying for documents without embeddings...")
    results = list(search_client.search(
        search_text="*",
        filter=filter_query,
        select=["id", "full_identifier", "field_id", "screen_id", "help_text"],
        top=max_docs if max_docs else 50000
    ))
    
    total = len(results)
    print(f"Found {total:,} documents without embeddings")
    
    if total == 0:
        print("✅ All documents already have embeddings!")
        return
    
    print()
    print(f"Processing in batches of {batch_size}...")
    print()
    
    processed = 0
    failed = 0
    start_time = time.time()
    
    for i in range(0, total, batch_size):
        batch = results[i:i + batch_size]
        batch_num = (i // batch_size) + 1
        total_batches = (total + batch_size - 1) // batch_size
        
        batch_start = time.time()
        
        # Generate embeddings for batch
        docs_to_update = []
        for doc in batch:
            # Create embedding text from help_text
            embedding_text = doc['help_text']
            
            embedding = generate_embedding(embedding_text)
            
            if embedding:
                docs_to_update.append({
                    'id': doc['id'],
                    'help_vector': embedding,
                    'has_vector': True
                })
            else:
                failed += 1
        
        # Upload batch
        if docs_to_update:
            try:
                search_client.merge_documents(documents=docs_to_update)
                processed += len(docs_to_update)
            except Exception as e:
                print(f"  ❌ Batch {batch_num} upload error: {e}")
                failed += len(docs_to_update)
                continue
        
        batch_time = time.time() - batch_start
        elapsed = time.time() - start_time
        docs_per_sec = processed / elapsed if elapsed > 0 else 0
        remaining = total - processed - failed
        eta_seconds = remaining / docs_per_sec if docs_per_sec > 0 else 0
        
        print(f"Batch {batch_num:3d}/{total_batches}: "
              f"{len(docs_to_update):4d} embedded ({batch_time:.1f}s) | "
              f"Total: {processed:6,}/{total:,} | "
              f"{docs_per_sec:.1f} docs/sec | "
              f"ETA: {eta_seconds/60:.0f}m")
    
    print()
    print("=" * 80)
    print(f"✅ Embedding backfill complete!")
    print(f"   Processed: {processed:,}")
    if failed > 0:
        print(f"   Failed: {failed:,}")
    print(f"   Total time: {(time.time() - start_time)/60:.1f} minutes")
    print("=" * 80)


def check_status():
    """Check embedding status."""
    credential = AzureKeyCredential(config.search_key)
    search_client = SearchClient(
        endpoint=config.search_endpoint,
        index_name=INDEX_NAME,
        credential=credential
    )
    
    # Count total
    total_results = list(search_client.search(
        search_text="*",
        select=["id"],
        top=1
    ))
    
    # Count with vectors
    with_vector_results = list(search_client.search(
        search_text="*",
        filter="has_vector eq true",
        select=["id"],
        top=1
    ))
    
    print(f"Total documents: (need to query full count)")
    print(f"With embeddings: (need to query full count)")


def main():
    """Main function."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Backfill help_fields embeddings")
    parser.add_argument('--batch', type=int, default=100, help='Batch size')
    parser.add_argument('--max', type=int, help='Max documents to process')
    parser.add_argument('--status', action='store_true', help='Check status only')
    
    args = parser.parse_args()
    
    print("=" * 80)
    print("Help Fields Embedding Backfill")
    print("=" * 80)
    print()
    
    if args.status:
        check_status()
    else:
        backfill_embeddings(batch_size=args.batch, max_docs=args.max)


if __name__ == '__main__':
    main()
