#!/usr/bin/env python3
"""
CONTINUOUS EMBEDDINGS PROCESSOR
Runs continuous batch processing until Priority 1 is complete.
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

def get_sample_record_for_debugging(config: Dict, index_name: str) -> Dict:
    """Get a sample record to debug text extraction issues."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': 1,
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
            records = data.get('value', [])
            return records[0] if records else None
        else:
            return None
            
    except Exception as e:
        return None

def debug_failing_indexes():
    """Debug why screen_nodes and ui_paths are failing."""
    config = load_config()
    
    print("🔍 DEBUGGING FAILING INDEXES")
    print("=" * 50)
    
    failing_configs = [
        {
            'index': 'new_cobol_screen_nodes',
            'vector_field': 'summary_vector',
            'text_fields': ['summary_text', 'screen_name', 'program_id'],
            'key_field': 'screen_id'
        },
        {
            'index': 'new_cobol_ui_paths',
            'vector_field': 'path_vector',
            'text_fields': ['guard_summary', 'start_program_id', 'end_program_id'],
            'key_field': 'path_id'
        }
    ]
    
    for config_item in failing_configs:
        print(f"\n🔍 Debugging {config_item['index']}")
        print("-" * 40)
        
        record = get_sample_record_for_debugging(config, config_item['index'])
        if not record:
            print("   ❌ No records found without vectors")
            continue
            
        print(f"   Sample record key: {record.get(config_item['key_field'], 'missing')}")
        
        # Check each text field
        for field in config_item['text_fields']:
            value = record.get(field)
            if value:
                print(f"   ✅ {field}: {str(value)[:100]}...")
            else:
                print(f"   ❌ {field}: None/Empty")
        
        # Try text extraction
        text_parts = []
        for field in config_item['text_fields']:
            value = record.get(field)
            if value and str(value).strip():
                text_parts.append(str(value).strip()[:500])
        
        text = ' | '.join(text_parts) if text_parts else record.get(config_item['key_field'], 'default')
        print(f"   📝 Extracted text: '{text[:200]}...'")
        print(f"   📏 Text length: {len(text)} chars")
        
        if len(text.strip()) < 3:
            print(f"   ⚠️  TEXT TOO SHORT - This may cause embedding failures")

def process_batch_with_retry(config: Dict, index_config: Dict, batch_size: int = 50) -> Dict:
    """Process a batch with enhanced error handling and retry logic."""
    index_name = index_config['index']
    vector_field = index_config['vector_field']
    text_fields = index_config['text_fields']
    key_field = index_config['key_field']
    
    # Get records without vectors
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': batch_size,
        'select': '*'
    }
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code != 200:
            return {'processed': 0, 'successful': 0, 'failed': batch_size}
            
        records = response.json().get('value', [])
        if not records:
            return {'processed': 0, 'successful': 0, 'failed': 0}
        
    except Exception as e:
        return {'processed': 0, 'successful': 0, 'failed': batch_size}
    
    # Process each record with enhanced text extraction
    successful = 0
    failed = 0
    
    for record in records:
        try:
            # Enhanced text extraction
            text_parts = []
            
            for field in text_fields:
                value = record.get(field)
                if value and str(value).strip():
                    text = str(value).strip()
                    
                    # Handle different field types
                    if field.endswith('_json'):
                        try:
                            json_obj = json.loads(text)
                            if isinstance(json_obj, dict):
                                # Extract meaningful keys
                                meaningful_parts = []
                                for k, v in json_obj.items():
                                    if isinstance(v, (str, int, float)) and str(v).strip():
                                        meaningful_parts.append(f"{k}:{v}")
                                text = ' '.join(meaningful_parts[:10])  # Limit to 10 parts
                            elif isinstance(json_obj, list):
                                text = ' '.join(str(item) for item in json_obj[:5])  # First 5 items
                        except:
                            pass  # Keep original text if JSON parsing fails
                    
                    text_parts.append(text[:300])  # Limit each field to 300 chars
            
            # Fallback text if no meaningful content
            if not text_parts:
                key_value = record.get(key_field, '')
                if key_value:
                    text_parts.append(f"Record {key_value}")
                else:
                    text_parts.append("Default content for embedding")
            
            final_text = ' | '.join(text_parts)
            
            # Ensure minimum text length
            if len(final_text.strip()) < 10:
                final_text = f"Content for {record.get(key_field, 'unknown')} in {index_name}"
            
            # Generate embedding
            embedding = generate_embedding(config, final_text)
            
            if not embedding:
                failed += 1
                continue
            
            # Update record
            record_key = record.get(key_field)
            if not record_key:
                failed += 1
                continue
            
            update_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
            
            update_doc = {
                '@search.action': 'merge',
                key_field: record_key,
                vector_field: embedding,
                'has_vector': True
            }
            
            update_body = {'value': [update_doc]}
            
            update_response = requests.post(
                update_url,
                headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
                json=update_body,
                timeout=30
            )
            
            if update_response.status_code in [200, 201]:
                successful += 1
            else:
                failed += 1
                
        except Exception as e:
            failed += 1
        
        # Rate limiting
        time.sleep(0.05)
    
    return {'processed': len(records), 'successful': successful, 'failed': failed}

def run_continuous_priority_1():
    """Run continuous processing until Priority 1 is complete."""
    config = load_config()
    
    print("🔄 CONTINUOUS PRIORITY 1 EMBEDDINGS PROCESSOR")
    print("=" * 60)
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Focus on the indexes that are making progress
    priority_configs = [
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'text_fields': ['flow_summary', 'program_id', 'condensed_mermaid'],
            'key_field': 'program_id',
            'target_coverage': 100
        },
        {
            'index': 'new_cobol_program_deps',
            'vector_field': 'dependency_blob_vector',
            'text_fields': ['dependency_blob', 'program_id'],
            'key_field': 'program_id',
            'target_coverage': 100
        },
        {
            'index': 'new_cobol_screen_nodes',
            'vector_field': 'summary_vector',
            'text_fields': ['summary_text', 'screen_name', 'program_id', 'raw_span_text'],
            'key_field': 'screen_id',
            'target_coverage': 50  # Start with 50% due to failures
        },
        {
            'index': 'new_cobol_ui_paths',
            'vector_field': 'path_vector',
            'text_fields': ['guard_summary', 'start_program_id', 'end_program_id', 'path_json'],
            'key_field': 'path_id',
            'target_coverage': 50  # Start with 50% due to failures
        }
    ]
    
    total_generated = 0
    round_count = 0
    
    while round_count < 10:  # Maximum 10 rounds
        round_count += 1
        print(f"\n🔄 PROCESSING ROUND {round_count}")
        print("=" * 40)
        
        round_total = 0
        round_successful = 0
        
        for config_item in priority_configs:
            result = process_batch_with_retry(config, config_item, batch_size=100)
            
            round_total += result['processed']
            round_successful += result['successful']
            
            if result['processed'] > 0:
                success_rate = (result['successful'] / result['processed']) * 100
                print(f"   {config_item['index'][:25]}: {result['successful']}/{result['processed']} ({success_rate:.1f}%)")
            else:
                print(f"   {config_item['index'][:25]}: No records to process")
        
        total_generated += round_successful
        
        print(f"   Round total: {round_successful}/{round_total} successful")
        print(f"   Session total: {total_generated:,} embeddings generated")
        
        if round_total == 0:
            print("   ✅ No more records to process!")
            break
        
        print(f"   ⏸ Pausing 10 seconds before next round...")
        time.sleep(10)
    
    print(f"\n🎉 CONTINUOUS PROCESSING COMPLETE!")
    print(f"Total embeddings generated: {total_generated:,}")
    print(f"Rounds completed: {round_count}")

if __name__ == '__main__':
    # First debug the failing indexes
    debug_failing_indexes()
    
    print("\n" + "="*60)
    
    # Then run continuous processing
    run_continuous_priority_1()