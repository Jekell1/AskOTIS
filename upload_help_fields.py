"""
Upload help field documentation to Azure Search index.
"""

import sys
sys.path.insert(0, '.')

import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config
import hashlib

# Load config
config = Config()
INDEX_NAME = "help_fields"


def generate_id(entry: dict) -> str:
    """Generate a unique ID for a help entry."""
    # Use full_identifier as base
    identifier = entry['full_identifier']
    # Create a hash to ensure it's a valid Azure Search key
    return hashlib.md5(identifier.encode()).hexdigest()


def prepare_document(entry: dict) -> dict:
    """Prepare a document for upload to Azure Search."""
    
    # Extract validation flags
    validation = entry.get('validation', {})
    
    doc = {
        'id': generate_id(entry),
        'full_identifier': entry['full_identifier'],
        'field_id': entry['field_id'],
        'screen_id': entry['screen_id'],
        'module': entry['module'],
        'field_type': entry['field_type'],
        'help_text': entry['help_text'],
        'char_length': entry['char_length'],
        
        # Validation flags
        'has_scan_window': validation.get('has_scan_window', False),
        'required': validation.get('required', False),
        'optional': validation.get('optional', False),
        'numeric': validation.get('numeric', False),
        'date_field': validation.get('date_field', False),
        
        # Lists
        'function_keys': validation.get('function_keys', []),
        'valid_values_json': json.dumps(validation.get('valid_values', [])),
        
        # Vector placeholder
        'has_vector': False  # Will be backfilled later
    }
    
    return doc


def upload_in_batches(documents: list, batch_size: int = 1000):
    """Upload documents to Azure Search in batches."""
    
    credential = AzureKeyCredential(config.search_key)
    search_client = SearchClient(
        endpoint=config.search_endpoint,
        index_name=INDEX_NAME,
        credential=credential
    )
    
    total = len(documents)
    uploaded = 0
    failed = 0
    
    print(f"Uploading {total:,} documents in batches of {batch_size}...")
    print()
    
    for i in range(0, total, batch_size):
        batch = documents[i:i + batch_size]
        batch_num = (i // batch_size) + 1
        total_batches = (total + batch_size - 1) // batch_size
        
        try:
            result = search_client.upload_documents(documents=batch)
            
            # Count successes and failures
            succeeded = sum(1 for r in result if r.succeeded)
            failed_count = len(batch) - succeeded
            
            uploaded += succeeded
            failed += failed_count
            
            print(f"Batch {batch_num}/{total_batches}: {succeeded}/{len(batch)} succeeded", end="")
            if failed_count > 0:
                print(f" ({failed_count} failed)", end="")
            print()
            
        except Exception as e:
            print(f"❌ Error uploading batch {batch_num}: {e}")
            failed += len(batch)
    
    print()
    print("=" * 80)
    print(f"✅ Upload complete!")
    print(f"   Total: {total:,}")
    print(f"   Uploaded: {uploaded:,}")
    if failed > 0:
        print(f"   Failed: {failed:,}")
    print("=" * 80)


def main():
    """Main function."""
    print("=" * 80)
    print("Upload Help Fields to Azure Search")
    print("=" * 80)
    print()
    
    # Load the JSONL file
    input_file = "help_fields.jsonl"
    
    print(f"Loading from {input_file}...")
    entries = []
    with open(input_file, 'r', encoding='utf-8') as f:
        for line in f:
            if line.strip():
                entries.append(json.loads(line))
    
    print(f"Loaded {len(entries):,} entries")
    print()
    
    # Prepare documents
    print("Preparing documents...")
    documents = [prepare_document(entry) for entry in entries]
    print(f"Prepared {len(documents):,} documents")
    print()
    
    # Upload
    upload_in_batches(documents, batch_size=1000)
    
    # Show sample
    print()
    print("Sample uploaded document:")
    print(json.dumps(documents[0], indent=2))


if __name__ == '__main__':
    main()
