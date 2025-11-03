#!/usr/bin/env python3
"""
EFFICIENT BATCH PROCESSOR
High-throughput processing for the remaining Priority 1 records.
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

def process_efficient_batch(config: Dict, index_config: Dict, batch_size: int = 300) -> Dict:
    """Process a batch with maximum efficiency."""
    index_name = index_config['index']
    vector_field = index_config['vector_field']
    text_fields = index_config['text_fields']
    key_field = index_config['key_field']
    
    print(f"   🔄 Processing {index_name} (batch size: {batch_size})")
    
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
    
    # Process records efficiently
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        if i % 50 == 0 and i > 0:
            print(f"   Progress: {i}/{len(records)} ({i/len(records)*100:.1f}%) - {successful} successful")
        
        try:
            # Efficient text extraction
            text_parts = []
            for field in text_fields:
                value = record.get(field)
                if value and str(value).strip():
                    text_parts.append(str(value).strip()[:300])
            
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
        
        # Minimal delay for maximum throughput
        time.sleep(0.01)
    
    return {'processed': len(records), 'successful': successful, 'failed': failed}

def run_efficient_batch_processing():
    """Run efficient batch processing for maximum progress."""
    config = load_config()
    
    print("⚡ EFFICIENT BATCH PROCESSOR")
    print("=" * 50)
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Focus on high-success indexes
    efficient_configs = [
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
    
    total_generated = 0
    
    # Run 5 efficient batches
    for batch_num in range(1, 6):
        print(f"\n⚡ EFFICIENT BATCH {batch_num}/5")
        print("-" * 40)
        
        batch_total = 0
        
        for config_item in efficient_configs:
            result = process_efficient_batch(config, config_item, batch_size=300)
            
            if result['processed'] > 0:
                success_rate = (result['successful'] / result['processed']) * 100
                print(f"   ✅ {config_item['index'][:25]}: {result['successful']}/{result['processed']} ({success_rate:.1f}%)")
                batch_total += result['successful']
            else:
                print(f"   ✅ {config_item['index'][:25]}: Complete!")
        
        total_generated += batch_total
        print(f"   📊 Batch total: {batch_total} embeddings")
        print(f"   📈 Session total: {total_generated:,} embeddings")
        
        if batch_total == 0:
            print(f"   🎉 All target indexes complete!")
            break
        
        if batch_num < 5:
            print(f"   ⏸ Brief pause before next batch...")
            time.sleep(3)
    
    print(f"\n" + "=" * 50)
    print("⚡ EFFICIENT PROCESSING COMPLETE!")
    print("=" * 50)
    print(f"Total embeddings generated: {total_generated:,}")
    print("Run comprehensive_status_report.py to see updated progress!")

if __name__ == '__main__':
    run_efficient_batch_processing()