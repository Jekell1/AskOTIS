#!/usr/bin/env python3
"""
PROGRAM DEPS COMPLETION PROCESSOR
Dedicated processor to complete new_cobol_program_deps to 100%.
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

def get_deps_status(config: Dict) -> Dict:
    """Get current status of program_deps index."""
    index_name = 'new_cobol_program_deps'
    
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

def process_deps_batch(config: Dict, batch_size: int = 500) -> Dict:
    """Process a batch of program_deps records."""
    index_name = 'new_cobol_program_deps'
    
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
    
    print(f"   Processing {len(records)} program_deps records...")
    
    successful = 0
    failed = 0
    
    for i, record in enumerate(records):
        if i % 100 == 0 and i > 0:
            print(f"   Progress: {i}/{len(records)} ({i/len(records)*100:.1f}%) - {successful} successful")
        
        try:
            # Extract text from dependency-specific fields
            text_parts = []
            
            # Priority fields for program dependencies
            fields_to_extract = [
                'dependency_blob',
                'program_id',
                'calls_out_json',
                'calls_in_json',
                'external_programs_json',
                'copybooks_used_json'
            ]
            
            for field in fields_to_extract:
                value = record.get(field)
                if value and str(value).strip():
                    text_content = str(value).strip()
                    
                    # Handle JSON fields
                    if field.endswith('_json'):
                        try:
                            json_obj = json.loads(text_content)
                            if isinstance(json_obj, (list, dict)):
                                # Convert to readable format
                                text_content = json.dumps(json_obj, separators=(',', ':'))[:500]
                        except:
                            pass  # Keep original text if JSON parsing fails
                    
                    text_parts.append(text_content[:500])
            
            # Fallback if no text found
            if not text_parts:
                program_id = record.get('program_id', 'unknown')
                text_parts.append(f"Program dependencies for {program_id}")
            
            text = ' | '.join(text_parts)
            
            # Generate embedding
            embedding = generate_embedding(config, text)
            if not embedding:
                failed += 1
                continue
            
            # Update record
            program_id = record.get('program_id')
            if not program_id:
                failed += 1
                continue
            
            update_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
            
            update_doc = {
                '@search.action': 'merge',
                'program_id': program_id,
                'dependency_blob_vector': embedding,
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
        
        # Fast processing
        time.sleep(0.01)
    
    return {'processed': len(records), 'successful': successful, 'failed': failed}

def complete_program_deps():
    """Complete the program_deps index to 100%."""
    config = load_config()
    
    print("🎯 PROGRAM DEPS COMPLETION PROCESSOR")
    print("=" * 60)
    print("Target: Complete new_cobol_program_deps to 100%")
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Check initial status
    initial_status = get_deps_status(config)
    print(f"\n📊 INITIAL STATUS:")
    print(f"   Current: {initial_status['with_vectors']:,}/{initial_status['total']:,} ({initial_status['coverage_percent']:.1f}%)")
    print(f"   Remaining: {initial_status['remaining']:,} records")
    
    if initial_status['remaining'] == 0:
        print(f"   🎉 Already complete!")
        return
    
    session_total = 0
    batch_count = 0
    
    # Process until complete
    while True:
        batch_count += 1
        current_status = get_deps_status(config)
        
        if current_status['remaining'] == 0:
            print(f"\n🎉 COMPLETION ACHIEVED!")
            break
        
        if batch_count > 50:  # Safety limit
            print(f"\n⚠️ Reached batch limit (50)")
            break
        
        print(f"\n🔄 BATCH {batch_count}")
        print(f"   Remaining: {current_status['remaining']:,} records")
        print(f"   Current coverage: {current_status['coverage_percent']:.1f}%")
        
        # Process batch
        batch_size = min(500, current_status['remaining'])
        result = process_deps_batch(config, batch_size)
        
        if result['processed'] == 0:
            print(f"   ⚠️ No records found to process")
            break
        
        session_total += result['successful']
        success_rate = (result['successful'] / result['processed']) * 100
        
        print(f"   ✅ Batch result: {result['successful']}/{result['processed']} ({success_rate:.1f}%)")
        print(f"   📈 Session total: {session_total:,} embeddings")
        
        # Brief pause between batches
        if current_status['remaining'] > 0:
            print(f"   ⏸ Brief pause...")
            time.sleep(2)
    
    # Final status
    final_status = get_deps_status(config)
    
    print(f"\n" + "=" * 60)
    print("🎯 PROGRAM DEPS COMPLETION SUMMARY")
    print("=" * 60)
    print(f"Initial coverage: {initial_status['coverage_percent']:.1f}%")
    print(f"Final coverage: {final_status['coverage_percent']:.1f}%")
    print(f"Embeddings generated: {session_total:,}")
    print(f"Batches processed: {batch_count}")
    
    if final_status['coverage_percent'] >= 99.9:
        print(f"🎉 SUCCESS: Program deps index is complete!")
    else:
        print(f"📊 Progress made: {session_total:,} embeddings added")
        print(f"Remaining: {final_status['remaining']:,} records")

if __name__ == '__main__':
    complete_program_deps()