#!/usr/bin/env python3
"""
COMPLETE SCREEN NODES WITH FIXED DIMENSIONS
Completes embeddings for the recreated screen_nodes index with correct 3072 dimensions.
"""

import requests
import os
import json
import time
from typing import Dict, List, Optional
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

def get_records_without_embeddings(config: Dict, batch_size: int = 100) -> List[Dict]:
    """Get records that don't have embeddings yet."""
    url = f"{config['search_endpoint']}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': batch_size,
        'select': 'screen_id,program_id,screen_name,summary_text,raw_span_text,fields_json,actions_json,transitions_json'
    }
    
    try:
        response = requests.post(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=body,
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json().get('value', [])
        else:
            print(f"❌ Error fetching records: {response.status_code}")
            return []
            
    except Exception as e:
        print(f"❌ Exception fetching records: {e}")
        return []

def extract_text_content(record: Dict) -> str:
    """Extract meaningful text content from a screen node record."""
    text_parts = []
    
    # Primary content
    summary_text = record.get('summary_text', '').strip()
    if summary_text and summary_text != 'NO_SCREEN_CONTENT':
        text_parts.append(summary_text)
    
    # Screen identification
    screen_name = record.get('screen_name', '').strip()
    program_id = record.get('program_id', '').strip()
    if screen_name and screen_name != 'NO_SCREEN_STRUCTURE':
        text_parts.append(f"Screen: {screen_name}")
    if program_id:
        text_parts.append(f"Program: {program_id}")
    
    # Raw span text
    raw_span = record.get('raw_span_text', '').strip()
    if raw_span and raw_span != 'NO_SCREEN_CONTENT' and len(raw_span) > 10:
        text_parts.append(f"Content: {raw_span[:500]}")
    
    # JSON field content
    for json_field in ['fields_json', 'actions_json', 'transitions_json']:
        json_content = record.get(json_field)
        if json_content:
            try:
                if isinstance(json_content, str):
                    parsed = json.loads(json_content)
                else:
                    parsed = json_content
                
                if parsed and isinstance(parsed, list) and len(parsed) > 0:
                    field_name = json_field.replace('_json', '').title()
                    text_parts.append(f"{field_name}: {len(parsed)} items")
                    
                    # Add sample content
                    if len(parsed) > 0 and isinstance(parsed[0], dict):
                        sample_keys = list(parsed[0].keys())[:3]
                        text_parts.append(f"Sample {field_name.lower()}: {', '.join(sample_keys)}")
                        
            except (json.JSONDecodeError, TypeError):
                pass
    
    # Combine and clean
    combined_text = ' | '.join(text_parts)
    
    # Fallback for minimal content
    if len(combined_text.strip()) < 10:
        screen_id = record.get('screen_id', 'unknown')
        combined_text = f"Screen node {screen_id} from program {program_id or 'unknown'}"
    
    return combined_text[:8000]  # Azure OpenAI limit

def generate_embedding(config: Dict, text: str) -> Optional[List[float]]:
    """Generate embedding for text."""
    if not text or len(text.strip()) < 3:
        return None
    
    deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-3-large')
    url = f"{config['openai_endpoint']}/openai/deployments/{deployment_name}/embeddings?api-version={config['openai_version']}"
    
    headers = {
        'api-key': config['openai_key'],
        'Content-Type': 'application/json'
    }
    
    body = {'input': text}
    
    try:
        response = requests.post(url, headers=headers, json=body, timeout=30)
        if response.status_code == 200:
            return response.json()['data'][0]['embedding']
        else:
            print(f"   ❌ Embedding API error: {response.status_code}")
            return None
    except Exception as e:
        print(f"   ❌ Embedding exception: {e}")
        return None

def update_record_with_embedding(config: Dict, record: Dict, embedding: List[float]) -> bool:
    """Update a record with its embedding."""
    url = f"{config['search_endpoint']}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    update_doc = {
        '@search.action': 'merge',
        'screen_id': record['screen_id'],
        'summary_vector': embedding,
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
        print(f"   ❌ Update exception: {e}")
        return False

def process_batch(config: Dict, records: List[Dict]) -> tuple[int, int]:
    """Process a batch of records."""
    success_count = 0
    total_count = len(records)
    
    for i, record in enumerate(records):
        screen_id = record.get('screen_id', 'unknown')
        print(f"   Processing {i+1}/{total_count}: {screen_id}")
        
        # Extract text
        text_content = extract_text_content(record)
        print(f"     Text: {text_content[:100]}...")
        
        # Generate embedding
        embedding = generate_embedding(config, text_content)
        if not embedding:
            print(f"     ❌ Failed to generate embedding")
            continue
        
        print(f"     ✅ Generated {len(embedding)}-dimension embedding")
        
        # Update record
        if update_record_with_embedding(config, record, embedding):
            success_count += 1
            print(f"     ✅ Updated successfully")
        else:
            print(f"     ❌ Failed to update")
        
        # Rate limiting
        time.sleep(0.5)
    
    return success_count, total_count

def complete_screen_nodes():
    """Complete screen nodes embeddings."""
    config = load_config()
    
    print("🔧 COMPLETING SCREEN NODES EMBEDDINGS (FIXED DIMENSIONS)")
    print("=" * 70)
    print("Processing the recreated index with correct 3072 dimensions")
    
    total_processed = 0
    total_success = 0
    batch_num = 0
    
    while True:
        batch_num += 1
        print(f"\n📦 BATCH {batch_num}")
        print("-" * 30)
        
        # Get next batch
        records = get_records_without_embeddings(config, batch_size=50)
        if not records:
            print("✅ No more records to process")
            break
        
        print(f"Found {len(records)} records without embeddings")
        
        # Process batch
        success, total = process_batch(config, records)
        total_success += success
        total_processed += total
        
        print(f"   Batch result: {success}/{total} successful")
        print(f"   Overall: {total_success}/{total_processed} successful")
        
        # Small delay between batches
        time.sleep(2)
    
    print(f"\n" + "=" * 70)
    print("🎯 SCREEN NODES COMPLETION SUMMARY")
    print("=" * 70)
    print(f"Total records processed: {total_processed}")
    print(f"Successful embeddings: {total_success}")
    print(f"Success rate: {(total_success/total_processed*100):.1f}%" if total_processed > 0 else "N/A")
    
    if total_success > 0:
        print(f"✅ Screen nodes embeddings are now working with 3072 dimensions!")

if __name__ == '__main__':
    complete_screen_nodes()