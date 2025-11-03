#!/usr/bin/env python3
"""
AUTOMATED EMBEDDINGS GENERATOR - PRIORITY 1 EXECUTION
Completes the remaining core program indexes with embeddings.
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

def get_records_for_embedding(config: Dict, index_name: str, vector_field: str, limit: int) -> List[Dict]:
    """Get records that need embeddings."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false',
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
            # Handle JSON fields
            if field.endswith('_json') and isinstance(value, str):
                try:
                    json_obj = json.loads(value)
                    if isinstance(json_obj, dict):
                        parts.append(json.dumps(json_obj)[:500])
                    elif isinstance(json_obj, list):
                        parts.append(str(json_obj)[:500])
                    else:
                        parts.append(str(value)[:500])
                except:
                    parts.append(str(value)[:500])
            else:
                parts.append(str(value).strip()[:500])
    
    return ' | '.join(parts) if parts else ""

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

def update_record_embedding(config: Dict, index_name: str, record: Dict, vector_field: str, embedding: List[float]) -> bool:
    """Update a record with its embedding."""
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

def process_priority_1_automatically():
    """Automatically process Priority 1 embeddings tasks."""
    config = load_config()
    
    print("🤖 AUTOMATED PRIORITY 1 EMBEDDINGS GENERATION")
    print("=" * 50)
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Priority 1 Tasks - Remaining core indexes
    tasks = [
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'text_fields': ['flow_graph_json', 'program_id', 'program_summary'],
            'target': 1000,  # Process 1000 records
            'description': 'Program flow graphs'
        },
        {
            'index': 'new_cobol_program_deps',
            'vector_field': 'dependency_blob_vector', 
            'text_fields': ['program_id', 'dependency_summary'],
            'target': 1000,
            'description': 'Program dependencies'
        },
        {
            'index': 'new_cobol_screen_nodes',
            'vector_field': 'summary_vector',
            'text_fields': ['summary_text', 'screen_name', 'program_id'],
            'target': 5700,  # Updated to match new record count
            'description': 'Screen structures and field definitions'
        },
        {
            'index': 'new_cobol_ui_paths',
            'vector_field': 'path_vector',
            'text_fields': ['path_json', 'guard_summary', 'start_program_id', 'end_program_id'],
            'target': 1000,
            'description': 'UI navigation paths'
        }
    ]
    
    total_generated = 0
    total_failed = 0
    
    for i, task in enumerate(tasks, 1):
        print(f"\n🚀 TASK {i}/4: {task['description']}")
        print(f"   Index: {task['index']}")
        print(f"   Vector Field: {task['vector_field']}")
        print(f"   Target: {task['target']} records")
        print("-" * 40)
        
        try:
            # Get records needing embeddings
            records = get_records_for_embedding(config, task['index'], task['vector_field'], task['target'])
            print(f"   Found {len(records)} records needing embeddings")
            
            if not records:
                print("   ✅ No records need processing")
                continue
            
            # Process each record
            successful = 0
            failed = 0
            
            for j, record in enumerate(records):
                if j % 50 == 0:  # Progress update every 50 records
                    print(f"   Progress: {j}/{len(records)} ({j/len(records)*100:.1f}%)")
                
                # Extract text
                text = extract_text_from_record(record, task['text_fields'])
                
                if not text.strip():
                    failed += 1
                    continue
                
                # Generate embedding
                embedding = generate_embedding(config, text)
                
                if not embedding:
                    failed += 1
                    continue
                
                # Update record
                if update_record_embedding(config, task['index'], record, task['vector_field'], embedding):
                    successful += 1
                else:
                    failed += 1
                
                # Rate limiting
                time.sleep(0.1)
            
            print(f"   ✅ COMPLETED: {successful} successful, {failed} failed")
            total_generated += successful
            total_failed += failed
            
        except Exception as e:
            print(f"   ❌ TASK FAILED: {str(e)}")
            total_failed += task['target']
        
        # Pause between tasks
        print(f"   ⏸ Pausing 10 seconds before next task...")
        time.sleep(10)
    
    # Final summary
    print(f"\n{'=' * 50}")
    print("🎉 PRIORITY 1 AUTOMATION COMPLETE!")
    print(f"{'=' * 50}")
    print(f"Total embeddings generated: {total_generated:,}")
    print(f"Total failures: {total_failed:,}")
    print(f"Success rate: {total_generated/(total_generated+total_failed)*100:.1f}%" if (total_generated+total_failed) > 0 else "N/A")
    
    print(f"\n🎯 NEXT STEPS:")
    print("1. Run analyze_embeddings_coverage.py to verify progress")
    print("2. Start Priority 2 tasks for medium-volume indexes")
    print("3. Queue Priority 3 for background processing")

if __name__ == '__main__':
    process_priority_1_automatically()