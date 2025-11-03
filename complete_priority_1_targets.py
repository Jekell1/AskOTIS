#!/usr/bin/env python3
"""
TARGETED PRIORITY 1 COMPLETION
Focus on completing program_flows and program_deps which are working well.
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

def get_completion_status(config: Dict, index_name: str) -> Dict:
    """Get current completion status for an index."""
    # Total records
    count_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
    
    try:
        total_response = requests.get(
            count_url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        total_count = int(total_response.text) if total_response.status_code == 200 else 0
    except:
        total_count = 0
    
    # Records with vectors
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    vector_body = {
        'search': '*',
        'filter': 'has_vector eq true',
        'top': 0,
        'count': True
    }
    
    try:
        vector_response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=vector_body,
            timeout=30
        )
        
        vector_count = 0
        if vector_response.status_code == 200:
            vector_count = vector_response.json().get('@odata.count', 0)
    except:
        vector_count = 0
    
    return {
        'total': total_count,
        'with_vectors': vector_count,
        'remaining': total_count - vector_count,
        'coverage_percent': (vector_count / total_count * 100) if total_count > 0 else 0
    }

def process_large_batch(config: Dict, index_config: Dict, batch_size: int = 500) -> Dict:
    """Process a large batch efficiently."""
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
    
    print(f"   Processing batch of {len(records)} records...")
    
    # Process records in parallel-friendly chunks
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        if i % 100 == 0 and i > 0:
            print(f"   Progress: {i}/{len(records)} ({i/len(records)*100:.1f}%) - {successful} successful")
        
        try:
            # Extract text efficiently
            text_parts = []
            for field in text_fields:
                value = record.get(field)
                if value and str(value).strip():
                    text_parts.append(str(value).strip()[:400])
            
            text = ' | '.join(text_parts) if text_parts else f"Content for {record.get(key_field, 'unknown')}"
            
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
        
        # Minimal rate limiting for efficiency
        time.sleep(0.02)
    
    return {'processed': len(records), 'successful': successful, 'failed': failed}

def complete_target_indexes():
    """Complete the remaining program_flows and program_deps indexes."""
    config = load_config()
    
    print("🚀 TARGETED PRIORITY 1 COMPLETION")
    print("=" * 60)
    print(f"Focusing on indexes with high success rates")
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Target the working indexes
    target_configs = [
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'text_fields': ['flow_summary', 'program_id', 'condensed_mermaid'],
            'key_field': 'program_id'
        },
        {
            'index': 'new_cobol_program_deps',
            'vector_field': 'dependency_blob_vector',
            'text_fields': ['dependency_blob', 'program_id'],
            'key_field': 'program_id'
        }
    ]
    
    for config_item in target_configs:
        index_name = config_item['index']
        print(f"\n🎯 COMPLETING {index_name}")
        print("-" * 50)
        
        # Check initial status
        status = get_completion_status(config, index_name)
        print(f"   Current: {status['with_vectors']:,}/{status['total']:,} ({status['coverage_percent']:.1f}%)")
        print(f"   Remaining: {status['remaining']:,} records")
        
        if status['remaining'] == 0:
            print(f"   ✅ Already complete!")
            continue
        
        # Process in batches until complete
        total_generated = 0
        batch_count = 0
        
        while batch_count < 20 and status['remaining'] > 0:  # Max 20 batches
            batch_count += 1
            print(f"\n   Batch {batch_count}:")
            
            batch_size = min(500, status['remaining'])
            result = process_large_batch(config, config_item, batch_size)
            
            total_generated += result['successful']
            
            if result['processed'] == 0:
                print(f"   ✅ No more records to process!")
                break
            
            success_rate = (result['successful'] / result['processed']) * 100
            print(f"   ✅ Batch complete: {result['successful']}/{result['processed']} ({success_rate:.1f}%)")
            
            # Update status
            status = get_completion_status(config, index_name)
            print(f"   📊 New status: {status['with_vectors']:,}/{status['total']:,} ({status['coverage_percent']:.1f}%)")
            
            if status['coverage_percent'] >= 99.5:
                print(f"   🎉 INDEX COMPLETE!")
                break
            
            print(f"   ⏸ Pausing 5 seconds before next batch...")
            time.sleep(5)
        
        print(f"\n   🎉 {index_name} processing complete!")
        print(f"   📈 Generated {total_generated:,} embeddings")
        print(f"   📊 Final coverage: {status['coverage_percent']:.1f}%")
    
    print(f"\n" + "=" * 60)
    print("🎉 TARGETED COMPLETION FINISHED!")
    print("=" * 60)
    print("Run comprehensive_status_report.py to see final results!")

if __name__ == '__main__':
    complete_target_indexes()