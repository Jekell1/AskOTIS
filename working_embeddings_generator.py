#!/usr/bin/env python3
"""
WORKING EMBEDDINGS GENERATOR
Uses has_vector field to track successful embeddings updates.
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
    return {
        'search_endpoint': (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/'),
        'search_key': os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY'),
        'openai_endpoint': (os.getenv('AZURE_OPENAI_ENDPOINT') or os.getenv('OPENAI_API_BASE')).rstrip('/'),
        'openai_key': os.getenv('AZURE_OPENAI_KEY') or os.getenv('OPENAI_API_KEY'),
        'openai_version': os.getenv('OPENAI_API_VERSION', '2024-08-01-preview')
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
            return None
    except Exception as e:
        return None

def get_records_without_vectors(config: Dict, index_name: str, limit: int) -> List[Dict]:
    """Get records that don't have embeddings yet."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    # Look for records where has_vector is false or missing
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
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
            return data.get('value', [])
        else:
            print(f"❌ Error fetching records: {response.status_code}")
            return []
            
    except Exception as e:
        print(f"❌ Exception fetching records: {e}")
        return []

def extract_text_for_embedding(record: Dict, text_fields: List[str]) -> str:
    """Extract meaningful text from a record for embedding."""
    parts = []
    
    for field in text_fields:
        value = record.get(field)
        if value and str(value).strip():
            text = str(value).strip()
            
            # Handle JSON fields
            if field.endswith('_json') and isinstance(value, str):
                try:
                    json_obj = json.loads(value)
                    if isinstance(json_obj, dict):
                        # Extract meaningful info from JSON
                        text = json.dumps(json_obj, separators=(',', ':'))[:1000]
                    elif isinstance(json_obj, list):
                        text = str(json_obj)[:1000]
                except:
                    pass
            
            parts.append(text[:500])
    
    return ' | '.join(parts) if parts else record.get('program_id', 'default content')

def update_record_with_vector(config: Dict, index_name: str, key_field: str, record_key: str, vector_field: str, embedding: List[float]) -> bool:
    """Update a record with both the vector and has_vector flag."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    
    update_doc = {
        '@search.action': 'merge',
        key_field: record_key,
        vector_field: embedding,
        'has_vector': True
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

def count_records_with_vectors(config: Dict, index_name: str) -> Dict:
    """Count how many records have vectors."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
    
    try:
        # Total records
        total_response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        total_count = int(total_response.text) if total_response.status_code == 200 else 0
        
        # Records with vectors
        search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        vector_body = {
            'search': '*',
            'filter': 'has_vector eq true',
            'top': 0,
            'count': True
        }
        
        vector_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=vector_body,
            timeout=30
        )
        
        vector_count = 0
        if vector_response.status_code == 200:
            vector_count = vector_response.json().get('@odata.count', 0)
        
        return {
            'total': total_count,
            'with_vectors': vector_count,
            'without_vectors': total_count - vector_count,
            'coverage_percent': (vector_count / total_count * 100) if total_count > 0 else 0
        }
        
    except Exception as e:
        return {'total': 0, 'with_vectors': 0, 'without_vectors': 0, 'coverage_percent': 0}

def process_embeddings_for_index(config: Dict, index_config: Dict, target_count: int = 100):
    """Process embeddings for a single index."""
    index_name = index_config['index']
    vector_field = index_config['vector_field']
    text_fields = index_config['text_fields']
    key_field = index_config['key_field']
    
    print(f"\n🔄 Processing {index_name}")
    print(f"   Vector field: {vector_field}")
    print(f"   Key field: {key_field}")
    print("-" * 50)
    
    # Check current status
    status = count_records_with_vectors(config, index_name)
    print(f"   Current status: {status['with_vectors']}/{status['total']} ({status['coverage_percent']:.1f}%)")
    
    if status['without_vectors'] == 0:
        print(f"   ✅ Already complete!")
        return {'processed': 0, 'successful': 0, 'failed': 0}
    
    # Get records without vectors
    records = get_records_without_vectors(config, index_name, min(target_count, status['without_vectors']))
    print(f"   Found {len(records)} records to process")
    
    if not records:
        print(f"   ✅ No records need processing")
        return {'processed': 0, 'successful': 0, 'failed': 0}
    
    # Process each record
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        if i % 25 == 0:
            print(f"   Progress: {i}/{len(records)} ({i/len(records)*100:.1f}%) - {successful} successful")
        
        # Extract text
        text = extract_text_for_embedding(record, text_fields)
        
        if not text.strip():
            failed += 1
            continue
        
        # Generate embedding
        embedding = generate_embedding(config, text)
        
        if not embedding:
            failed += 1
            continue
        
        # Update record
        record_key = record.get(key_field)
        if not record_key:
            failed += 1
            continue
            
        if update_record_with_vector(config, index_name, key_field, record_key, vector_field, embedding):
            successful += 1
        else:
            failed += 1
        
        # Rate limiting
        time.sleep(0.1)
    
    # Final status check
    final_status = count_records_with_vectors(config, index_name)
    
    result = {
        'processed': len(records),
        'successful': successful,
        'failed': failed,
        'final_coverage': final_status['coverage_percent']
    }
    
    print(f"   ✅ Completed: {successful} successful, {failed} failed")
    print(f"   📊 Final coverage: {final_status['with_vectors']}/{final_status['total']} ({final_status['coverage_percent']:.1f}%)")
    
    return result

def run_priority_1_embeddings():
    """Run Priority 1 embeddings generation with proper tracking."""
    config = load_config()
    
    print("🚀 PRIORITY 1 EMBEDDINGS GENERATION")
    print("=" * 60)
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Priority 1 configurations
    priority_1_configs = [
        {
            'index': 'new_cobol_program_meta',
            'vector_field': 'summary_vector',
            'text_fields': ['program_summary', 'program_id', 'sample_call_lines'],
            'key_field': 'program_id',
            'description': 'Program metadata summaries'
        },
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'text_fields': ['flow_summary', 'program_id', 'condensed_mermaid'],
            'key_field': 'program_id',
            'description': 'Program flow graphs'
        },
        {
            'index': 'new_cobol_program_deps',
            'vector_field': 'dependency_blob_vector',
            'text_fields': ['dependency_blob', 'program_id'],
            'key_field': 'program_id',
            'description': 'Program dependencies'
        },
        {
            'index': 'new_cobol_screen_nodes',
            'vector_field': 'summary_vector',
            'text_fields': ['summary_text', 'screen_name', 'program_id'],
            'key_field': 'screen_id',
            'description': 'Screen structures'
        },
        {
            'index': 'new_cobol_ui_paths',
            'vector_field': 'path_vector',
            'text_fields': ['guard_summary', 'start_program_id', 'end_program_id'],
            'key_field': 'path_id',
            'description': 'UI navigation paths'
        }
    ]
    
    total_processed = 0
    total_successful = 0
    total_failed = 0
    
    for i, config_item in enumerate(priority_1_configs, 1):
        print(f"\n🎯 TASK {i}/{len(priority_1_configs)}: {config_item['description']}")
        
        result = process_embeddings_for_index(config, config_item, target_count=200)
        
        total_processed += result['processed']
        total_successful += result['successful']
        total_failed += result['failed']
        
        if i < len(priority_1_configs):
            print(f"   ⏸ Pausing 5 seconds before next task...")
            time.sleep(5)
    
    print(f"\n" + "=" * 60)
    print("🎉 PRIORITY 1 COMPLETE!")
    print("=" * 60)
    print(f"Total processed: {total_processed:,}")
    print(f"Total successful: {total_successful:,}")
    print(f"Total failed: {total_failed:,}")
    print(f"Success rate: {total_successful/(total_processed)*100:.1f}%" if total_processed > 0 else "N/A")

if __name__ == '__main__':
    run_priority_1_embeddings()