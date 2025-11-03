#!/usr/bin/env python3
"""
Systematic embeddings generation with progress tracking.
Processes all indexes in manageable batches with proper error handling.
"""

import requests
import os
import json
import time
from typing import Dict, List, Any
from secrets_loader import load_secrets

def load_config():
    """Load configuration."""
    load_secrets()
    search_endpoint = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    openai_endpoint = (os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')).rstrip('/')
    openai_key = os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY')
    openai_version = os.getenv('OPENAI_API_VERSION', '2024-08-01-preview')
    
    return {
        'search_endpoint': search_endpoint,
        'search_key': search_key,
        'openai_endpoint': openai_endpoint,
        'openai_key': openai_key,
        'openai_version': openai_version
    }

def generate_embedding(config: Dict, text: str) -> List[float]:
    """Generate embedding for text."""
    deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-ada-002')
    url = f"{config['openai_endpoint']}/openai/deployments/{deployment_name}/embeddings?api-version={config['openai_version']}"
    
    headers = {
        'api-key': config['openai_key'],
        'Content-Type': 'application/json'
    }
    
    body = {'input': text[:8000]}
    
    try:
        response = requests.post(url, headers=headers, json=body, timeout=30)
        if response.status_code == 200:
            return response.json()['data'][0]['embedding']
        else:
            print(f"  ⚠ Embedding API error: {response.status_code}")
            return None
    except Exception as e:
        print(f"  ⚠ Embedding error: {e}")
        return None

def process_embeddings_systematically():
    """Process embeddings for high-priority indexes systematically."""
    config = load_config()
    
    print("=== SYSTEMATIC EMBEDDINGS PROCESSING ===")
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Priority processing queue with realistic batch sizes
    priority_tasks = [
        {
            'index': 'new_cobol_program_meta',
            'vector_field': 'summary_vector', 
            'text_fields': ['program_summary', 'program_role', 'program_id'],
            'target_count': 100,  # Start with 100 records
            'description': 'Core program metadata'
        },
        {
            'index': 'new_cobol_calls', 
            'vector_field': 'snippet_vector',
            'text_fields': ['context_snippet', 'caller_program', 'callee_program'],
            'target_count': 100,
            'description': 'Program call relationships'
        },
        {
            'index': 'new_cobol_copybook_meta',
            'vector_field': 'summary_vector',
            'text_fields': ['summary', 'copybook_name'],
            'target_count': 100, 
            'description': 'Copybook metadata'
        }
    ]
    
    for task in priority_tasks:
        print(f"\n{'='*60}")
        print(f"PROCESSING: {task['description']}")
        print(f"Index: {task['index']}")
        print(f"Vector field: {task['vector_field']}")
        print(f"Target: {task['target_count']} records")
        print('='*60)
        
        success_count = process_single_index(config, task)
        
        print(f"✓ Completed {task['index']}: {success_count} embeddings generated")
        
        # Pause between indexes to avoid rate limits
        time.sleep(3)
    
    print(f"\n🎉 SYSTEMATIC PROCESSING COMPLETE!")
    print("Next: Run analyze_embeddings_coverage.py to see progress")

def process_single_index(config: Dict, task: Dict) -> int:
    """Process a single index for embeddings generation."""
    index_name = task['index']
    vector_field = task['vector_field']
    text_fields = task['text_fields']
    target_count = task['target_count']
    
    # Get records that need embeddings
    records = get_records_for_embedding(config, index_name, vector_field, target_count)
    
    print(f"Found {len(records)} records needing embeddings")
    
    if not records:
        print("No records need processing")
        return 0
    
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        print(f"Processing record {i+1}/{len(records)}...", end=' ')
        
        # Extract text for embedding
        text = extract_text_from_record(record, text_fields)
        
        if not text.strip():
            print("❌ No text to embed")
            failed += 1
            continue
        
        # Generate embedding
        embedding = generate_embedding(config, text)
        
        if not embedding:
            print("❌ Embedding generation failed")
            failed += 1
            continue
        
        # Update record with embedding
        if update_record_embedding(config, index_name, record, vector_field, embedding):
            print(f"✅ Success ({len(embedding)} dims)")
            successful += 1
        else:
            print("❌ Update failed")
            failed += 1
        
        # Rate limiting
        time.sleep(0.2)
    
    print(f"\nIndex summary: {successful} successful, {failed} failed")
    return successful

def get_records_for_embedding(config: Dict, index_name: str, vector_field: str, limit: int) -> List[Dict]:
    """Get records that need embeddings."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq true',  # Records that should have vectors
        'top': limit,
        'select': '*'
    }
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code == 200:
            data = response.json()
            docs = data.get('value', [])
            
            # Filter to records that don't already have embeddings
            return [doc for doc in docs if not doc.get(vector_field)]
        else:
            print(f"Error fetching records: {response.status_code}")
            return []
            
    except Exception as e:
        print(f"Error fetching records: {e}")
        return []

def extract_text_from_record(record: Dict, text_fields: List[str]) -> str:
    """Extract meaningful text from a record."""
    parts = []
    
    for field in text_fields:
        value = record.get(field)
        if value and str(value).strip():
            parts.append(str(value).strip())
    
    return ' | '.join(parts) if parts else ""

def update_record_embedding(config: Dict, index_name: str, record: Dict, vector_field: str, embedding: List[float]) -> bool:
    """Update a record with its embedding."""
    # Find the key field for this index
    key_field = get_key_field(config, index_name)
    if not key_field or key_field not in record:
        return False
    
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    
    update_doc = {
        '@search.action': 'merge',
        key_field: record[key_field],
        vector_field: embedding
    }
    
    body = {'value': [update_doc]}
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        return response.status_code in [200, 201]
        
    except Exception as e:
        return False

def get_key_field(config: Dict, index_name: str) -> str:
    """Get the key field name for an index."""
    url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
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
        return None

if __name__ == '__main__':
    process_embeddings_systematically()