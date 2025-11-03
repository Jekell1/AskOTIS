#!/usr/bin/env python3
"""
MARATHON PROCESSOR - Continuous Processing Until Completion
Runs extended processing sessions to complete Priority 1 indexes.
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

def get_remaining_count(config: Dict, index_name: str) -> int:
    """Get count of remaining records without vectors."""
    search_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': 0,
        'count': True
    }
    
    try:
        response = requests.post(
            search_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json().get('@odata.count', 0)
        else:
            return 0
    except:
        return 0

def process_marathon_batch(config: Dict, index_config: Dict, batch_size: int = 500) -> Dict:
    """Process a large marathon batch."""
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
    
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        if i % 100 == 0 and i > 0:
            print(f"     Progress: {i}/{len(records)} ({i/len(records)*100:.1f}%) - {successful} successful")
        
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
        
        # Ultra-fast processing
        time.sleep(0.005)
    
    return {'processed': len(records), 'successful': successful, 'failed': failed}

def run_marathon_processing():
    """Run marathon processing until major completion."""
    config = load_config()
    
    print("🏃 MARATHON PROCESSOR - Extended Processing Session")
    print("=" * 70)
    print(f"Target: Complete program_flows and program_deps indexes")
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Target configs for marathon processing
    marathon_configs = [
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'text_fields': ['flow_summary', 'program_id', 'condensed_mermaid'],
            'key_field': 'program_id',
            'target_completion': 90  # 90% completion target
        },
        {
            'index': 'new_cobol_program_deps',
            'vector_field': 'dependency_blob_vector',
            'text_fields': ['dependency_blob', 'program_id'],
            'key_field': 'program_id',
            'target_completion': 90  # 90% completion target
        }
    ]
    
    session_total = 0
    round_count = 0
    
    # Run until targets are met or 20 rounds max
    while round_count < 20:
        round_count += 1
        print(f"\n🏃 MARATHON ROUND {round_count}/20")
        print("-" * 50)
        
        round_total = 0
        any_processing = False
        
        for config_item in marathon_configs:
            index_name = config_item['index']
            remaining = get_remaining_count(config, index_name)
            
            if remaining == 0:
                print(f"   ✅ {index_name[:25]}: COMPLETE!")
                continue
            
            print(f"   🔄 {index_name[:25]}: {remaining:,} remaining")
            
            # Process large batch
            batch_size = min(500, remaining)
            result = process_marathon_batch(config, config_item, batch_size)
            
            if result['processed'] > 0:
                any_processing = True
                success_rate = (result['successful'] / result['processed']) * 100
                print(f"     ✅ Processed: {result['successful']}/{result['processed']} ({success_rate:.1f}%)")
                round_total += result['successful']
        
        session_total += round_total
        print(f"   📊 Round total: {round_total} embeddings")
        print(f"   📈 Session total: {session_total:,} embeddings")
        
        if not any_processing:
            print(f"   🎉 All target indexes complete or no records to process!")
            break
        
        if round_count % 5 == 0:
            print(f"   ⏸ Brief pause after 5 rounds...")
            time.sleep(5)
        else:
            time.sleep(1)
    
    print(f"\n" + "=" * 70)
    print("🏃 MARATHON PROCESSING SESSION COMPLETE!")
    print("=" * 70)
    print(f"Total rounds: {round_count}")
    print(f"Total embeddings generated: {session_total:,}")
    print("Run comprehensive_status_report.py to see final results!")

if __name__ == '__main__':
    run_marathon_processing()