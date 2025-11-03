#!/usr/bin/env python3
"""
COMPLETE UI PATHS WITH FIXED DIMENSIONS
Completes embeddings for the recreated ui_paths index with correct 3072 dimensions.
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
    url = f"{config['search_endpoint']}/indexes/new_cobol_ui_paths/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': batch_size,
        'select': 'path_id,start_program_id,end_program_id,guard_summary,path_json,program_sequence_json,screen_sequence_json,root_program_id,leaf_program_id'
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
    """Extract meaningful text content from a UI path record."""
    text_parts = []
    
    # Path identification
    path_id = (record.get('path_id') or '').strip()
    start_program = (record.get('start_program_id') or '').strip()
    end_program = (record.get('end_program_id') or '').strip()
    
    if path_id:
        text_parts.append(f"UI Path: {path_id}")
    
    if start_program and end_program:
        if start_program == end_program:
            text_parts.append(f"Internal path within program {start_program}")
        else:
            text_parts.append(f"Path from {start_program} to {end_program}")
    
    # Guard summary
    guard_summary = (record.get('guard_summary') or '').strip()
    if guard_summary and guard_summary.lower() not in ['none', 'null', '']:
        text_parts.append(f"Guards: {guard_summary}")
    
    # Path JSON content
    path_json = record.get('path_json')
    if path_json:
        try:
            if isinstance(path_json, str):
                path_data = json.loads(path_json)
            else:
                path_data = path_json
            
            if isinstance(path_data, dict):
                # Extract meaningful path information
                program_sequence = path_data.get('program_sequence', [])
                screen_ids = path_data.get('screen_ids', [])
                guards = path_data.get('guards', [])
                length = path_data.get('length', 0)
                ui_program_count = path_data.get('ui_program_count', 0)
                
                if length:
                    text_parts.append(f"Path length: {length}")
                if ui_program_count:
                    text_parts.append(f"UI programs: {ui_program_count}")
                if program_sequence and len(program_sequence) > 1:
                    sequence_preview = ' → '.join(program_sequence[:3])
                    if len(program_sequence) > 3:
                        sequence_preview += f" ... ({len(program_sequence)} total)"
                    text_parts.append(f"Sequence: {sequence_preview}")
                if screen_ids and len(screen_ids) > 0:
                    text_parts.append(f"Screens: {len(screen_ids)} involved")
                if guards and len(guards) > 0:
                    text_parts.append(f"Guards: {len(guards)} conditions")
                    
        except (json.JSONDecodeError, TypeError):
            pass
    
    # Program sequence JSON
    program_sequence = record.get('program_sequence_json')
    if program_sequence and isinstance(program_sequence, list) and len(program_sequence) > 1:
        sequence_text = ' → '.join(program_sequence[:4])
        if len(program_sequence) > 4:
            sequence_text += f" ... ({len(program_sequence)} programs)"
        text_parts.append(f"Program flow: {sequence_text}")
    
    # Screen sequence
    screen_sequence = record.get('screen_sequence_json')
    if screen_sequence and isinstance(screen_sequence, list) and len(screen_sequence) > 0:
        text_parts.append(f"Screen flow: {len(screen_sequence)} screens")
    
    # Root and leaf programs
    root_program = (record.get('root_program_id') or '').strip()
    leaf_program = (record.get('leaf_program_id') or '').strip()
    if root_program and leaf_program and root_program != leaf_program:
        text_parts.append(f"Flow: {root_program} → {leaf_program}")
    
    # Combine and clean
    combined_text = ' | '.join(text_parts)
    
    # Fallback for minimal content
    if len(combined_text.strip()) < 10:
        combined_text = f"UI path {path_id or 'unknown'} connecting {start_program or 'unknown'} to {end_program or 'unknown'}"
    
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
    url = f"{config['search_endpoint']}/indexes/new_cobol_ui_paths/docs/index?api-version=2023-11-01"
    
    update_doc = {
        '@search.action': 'merge',
        'path_id': record['path_id'],
        'path_vector': embedding,
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
        path_id = record.get('path_id', 'unknown')
        print(f"   Processing {i+1}/{total_count}: {path_id}")
        
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

def complete_ui_paths():
    """Complete UI paths embeddings."""
    config = load_config()
    
    print("🔧 COMPLETING UI PATHS EMBEDDINGS (FIXED DIMENSIONS)")
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
    print("🎯 UI PATHS COMPLETION SUMMARY")
    print("=" * 70)
    print(f"Total records processed: {total_processed}")
    print(f"Successful embeddings: {total_success}")
    print(f"Success rate: {(total_success/total_processed*100):.1f}%" if total_processed > 0 else "N/A")
    
    if total_success > 0:
        print(f"✅ UI paths embeddings are now working with 3072 dimensions!")

if __name__ == '__main__':
    complete_ui_paths()