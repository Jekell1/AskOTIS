#!/usr/bin/env python3
"""
Generate embeddings for COBOL indexes in priority order.
Starts with core program-level indexes that provide maximum search value.
"""

import requests
import os
import json
import time
from typing import Dict, List, Any
import hashlib

#!/usr/bin/env python3
"""
Generate embeddings for COBOL indexes in priority order.
Starts with core program-level indexes that provide maximum search value.
"""

import requests
import os
import json
import time
from typing import Dict, List, Any
import hashlib
from secrets_loader import load_secrets

def load_config():
    """Load Azure Search and OpenAI configuration using existing pattern."""
    # Load secrets using the same pattern as other scripts
    load_secrets()
    
    # Azure Search configuration
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    # Azure OpenAI configuration  
    openai_endpoint = os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')
    openai_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    openai_version = os.getenv('OPENAI_API_VERSION', '2024-08-01-preview')
    
    if not all([search_endpoint, search_key, openai_endpoint, openai_key]):
        missing = []
        if not search_endpoint: missing.append('AZURE_SEARCH_ENDPOINT')
        if not search_key: missing.append('AZURE_SEARCH_KEY')  
        if not openai_endpoint: missing.append('AZURE_OPENAI_ENDPOINT')
        if not openai_key: missing.append('AZURE_OPENAI_KEY')
        raise ValueError(f"Missing configuration: {', '.join(missing)}")
    
    return {
        'search_endpoint': search_endpoint.rstrip('/'),
        'search_key': search_key,
        'openai_endpoint': openai_endpoint.rstrip('/'),
        'openai_key': openai_key,
        'openai_version': openai_version
    }

def generate_embedding(config: Dict, text: str) -> List[float]:
    """Generate embedding for text using Azure OpenAI."""
    # Use text-embedding-ada-002 deployment name (standard)
    deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-ada-002')
    
    url = f"{config['openai_endpoint']}/openai/deployments/{deployment_name}/embeddings?api-version={config['openai_version']}"
    
    headers = {
        'api-key': config['openai_key'],
        'Content-Type': 'application/json'
    }
    
    body = {
        'input': text[:8000]  # Truncate to avoid token limits
    }
    
    try:
        response = requests.post(url, headers=headers, json=body, timeout=30)
        if response.status_code == 200:
            data = response.json()
            return data['data'][0]['embedding']
        else:
            print(f"OpenAI API error: {response.status_code} - {response.text}")
            return None
    except Exception as e:
        print(f"Embedding generation error: {e}")
        return None

def get_records_needing_embeddings(config: Dict, index_name: str, vector_field: str, batch_size: int = 50) -> List[Dict]:
    """Get records that need embeddings generated."""
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    # Get records where has_vector=true but vector field is missing
    body = {
        'search': '*',
        'filter': 'has_vector eq true',
        'top': batch_size,
        'select': '*'
    }
    
    try:
        response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code == 200:
            data = response.json()
            docs = data.get('value', [])
            
            # Filter to only records missing the vector field
            missing_vectors = []
            for doc in docs:
                if not doc.get(vector_field):  # Vector field is empty/null
                    missing_vectors.append(doc)
            
            return missing_vectors
        else:
            print(f"Error fetching records: {response.status_code}")
            return []
            
    except Exception as e:
        print(f"Error fetching records: {e}")
        return []

def update_record_with_embedding(config: Dict, index_name: str, record: Dict, vector_field: str, embedding: List[float]) -> bool:
    """Update a record with its generated embedding."""
    update_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    
    # Create update document
    key_field = get_key_field(config, index_name)
    if not key_field or key_field not in record:
        print(f"Error: Cannot find key field for {index_name}")
        return False
    
    update_doc = {
        '@search.action': 'merge',
        key_field: record[key_field],
        vector_field: embedding
    }
    
    body = {'value': [update_doc]}
    
    try:
        response = requests.post(
            update_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        return response.status_code in [200, 201]
        
    except Exception as e:
        print(f"Error updating record: {e}")
        return False

def get_key_field(config: Dict, index_name: str) -> str:
    """Get the key field name for an index."""
    schema_url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            schema_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code == 200:
            schema = response.json()
            for field in schema.get('fields', []):
                if field.get('key', False):
                    return field.get('name')
        
        return None
        
    except Exception as e:
        print(f"Error getting schema: {e}")
        return None

def extract_text_for_embedding(record: Dict, index_name: str) -> str:
    """Extract meaningful text from a record for embedding generation."""
    
    if index_name == 'new_cobol_program_meta':
        # Program metadata - combine key fields
        parts = []
        if record.get('program_id'):
            parts.append(f"Program: {record['program_id']}")
        if record.get('program_summary'):
            parts.append(record['program_summary'])
        if record.get('program_role'):
            parts.append(f"Role: {record['program_role']}")
        return ' | '.join(parts)
    
    elif index_name == 'new_cobol_screen_nodes':
        # Screen nodes - combine screen info
        parts = []
        if record.get('screen_name'):
            parts.append(f"Screen: {record['screen_name']}")
        if record.get('summary_text'):
            parts.append(record['summary_text'])
        if record.get('raw_span_text'):
            parts.append(record['raw_span_text'][:1000])  # Truncate long text
        return ' | '.join(parts)
    
    elif index_name == 'new_cobol_calls':
        # Call relationships
        parts = []
        if record.get('caller_program'):
            parts.append(f"Caller: {record['caller_program']}")
        if record.get('callee_program'):
            parts.append(f"Callee: {record['callee_program']}")
        if record.get('context_snippet'):
            parts.append(record['context_snippet'])
        return ' | '.join(parts)
    
    elif index_name == 'new_cobol_copybook_meta':
        # Copybook metadata
        parts = []
        if record.get('copybook_name'):
            parts.append(f"Copybook: {record['copybook_name']}")
        if record.get('summary'):
            parts.append(record['summary'])
        return ' | '.join(parts)
    
    else:
        # Generic fallback - look for common text fields
        text_fields = ['summary', 'description', 'text', 'content', 'snippet']
        for field in text_fields:
            if record.get(field):
                return str(record[field])[:2000]
        
        # If no text fields, combine key identifiers
        id_fields = ['id', 'name', 'program_id', 'symbol_name']
        parts = []
        for field in id_fields:
            if record.get(field):
                parts.append(f"{field}: {record[field]}")
        
        return ' | '.join(parts) if parts else "No text content"

def process_index_embeddings(config: Dict, index_name: str, vector_field: str, max_records: int = None):
    """Process embeddings for a specific index and vector field."""
    print(f"\n=== PROCESSING {index_name}.{vector_field} ===")
    
    processed = 0
    successful = 0
    failed = 0
    
    while True:
        # Get batch of records needing embeddings
        records = get_records_needing_embeddings(config, index_name, vector_field, batch_size=10)
        
        if not records:
            print(f"✓ No more records need embeddings")
            break
            
        if max_records and processed >= max_records:
            print(f"✓ Reached max records limit ({max_records})")
            break
        
        print(f"Processing batch of {len(records)} records...")
        
        for record in records:
            if max_records and processed >= max_records:
                break
                
            # Extract text for embedding
            text = extract_text_for_embedding(record, index_name)
            
            if not text or text.strip() == "":
                print(f"  ⚠ Skipping record with no extractable text")
                failed += 1
                processed += 1
                continue
            
            # Generate embedding
            embedding = generate_embedding(config, text)
            
            if embedding:
                # Update record with embedding
                success = update_record_with_embedding(config, index_name, record, vector_field, embedding)
                if success:
                    successful += 1
                    print(f"  ✓ Generated embedding for record {processed + 1}")
                else:
                    failed += 1
                    print(f"  ✗ Failed to update record {processed + 1}")
            else:
                failed += 1
                print(f"  ✗ Failed to generate embedding for record {processed + 1}")
            
            processed += 1
            
            # Rate limiting
            time.sleep(0.1)
        
        print(f"Batch complete: {successful} successful, {failed} failed")
        
        # Brief pause between batches
        time.sleep(1)
    
    print(f"\n{index_name}.{vector_field} COMPLETE:")
    print(f"  Records processed: {processed}")
    print(f"  Embeddings generated: {successful}")
    print(f"  Failed: {failed}")
    
    return successful, failed

def main():
    print("=== COBOL INDEX EMBEDDINGS GENERATOR ===")
    
    config = load_config()
    print(f"✓ Azure Search: {config['search_endpoint']}")
    print(f"✓ Azure OpenAI: {config['openai_endpoint']}")
    
    # Test embedding generation first
    print(f"\n🧪 Testing embedding generation...")
    test_text = "Test program TESTPROG with sample functionality for embedding verification"
    test_embedding = generate_embedding(config, test_text)
    
    if test_embedding:
        print(f"✅ Embedding test successful! Generated {len(test_embedding)}-dimensional vector")
    else:
        print(f"❌ Embedding test failed - check Azure OpenAI configuration")
        return
    
    # Priority order - start with most impactful indexes
    priority_queue = [
        # Core program indexes (highest impact) - small batches first
        ('new_cobol_program_meta', 'summary_vector'),
        ('new_cobol_calls', 'snippet_vector'),
        ('new_cobol_screen_nodes', 'summary_vector'),
        
        # Secondary priority
        ('new_cobol_copybook_meta', 'summary_vector'),
        ('new_cobol_program_flows', 'flow_vector'),
        ('new_cobol_program_deps', 'dependency_blob_vector'),
        
        # Large volume indexes (process carefully)
        # ('new_cobol_paragraphs', 'para_vector'),  # 224K records
        # ('new_cobol_data_items', 'vector'),       # 267K records  
        # ('new_cobol_symbol_refs', 'excerpt_vector'), # 1.1M records
    ]
    
    print(f"\nProcessing {len(priority_queue)} high-priority vector fields...")
    
    total_generated = 0
    total_failed = 0
    
    for index_name, vector_field in priority_queue:
        try:
            generated, failed = process_index_embeddings(
                config, 
                index_name, 
                vector_field, 
                max_records=10  # Very small batches for initial testing
            )
            total_generated += generated
            total_failed += failed
            
        except Exception as e:
            print(f"Error processing {index_name}.{vector_field}: {e}")
            total_failed += 1
        
        # Pause between indexes
        time.sleep(2)
    
    print(f"\n=== EMBEDDINGS GENERATION COMPLETE ===")
    print(f"Total embeddings generated: {total_generated}")
    print(f"Total failures: {total_failed}")
    
    if total_generated > 0:
        print(f"\n✅ Successfully generated {total_generated} embeddings!")
        print("Run analyze_embeddings_coverage.py to see updated status.")
        print("\nTo process more records, increase max_records parameter and re-run.")

if __name__ == '__main__':
    main()