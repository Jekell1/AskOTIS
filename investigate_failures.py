#!/usr/bin/env python3
"""
FAILURE INVESTIGATION TOOL
Investigates why screen_nodes and ui_paths are failing consistently.
"""

import requests
import os
import json
from typing import Dict, List
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

def analyze_failing_records(config: Dict, index_name: str, sample_size: int = 5):
    """Analyze records that are failing to get embeddings."""
    print(f"\n🔍 ANALYZING {index_name}")
    print("-" * 50)
    
    # Get sample records without vectors
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'filter': 'has_vector eq false or has_vector eq null',
        'top': sample_size,
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
            print(f"❌ Error fetching records: {response.status_code}")
            return
            
        records = response.json().get('value', [])
        if not records:
            print(f"✅ No records without vectors found")
            return
        
    except Exception as e:
        print(f"❌ Exception fetching records: {e}")
        return
    
    print(f"Found {len(records)} sample records without vectors")
    
    for i, record in enumerate(records[:3]):  # Analyze first 3
        print(f"\n📋 RECORD {i+1}:")
        print(f"   ID: {record.get('screen_id' if 'screen' in index_name else 'path_id', 'unknown')}")
        
        # Show all available fields
        print(f"   Available fields:")
        for key, value in record.items():
            if key.startswith('@'):
                continue
            value_preview = str(value)[:100] if value else "None/Empty"
            print(f"     {key}: {value_preview}")
        
        # Test embedding generation
        print(f"   🧪 Testing embedding generation...")
        
        # Try different text combinations
        text_attempts = []
        
        if 'screen' in index_name:
            # Screen nodes attempts
            attempts = [
                record.get('summary_text', ''),
                record.get('screen_name', ''),
                record.get('program_id', ''),
                record.get('raw_span_text', ''),
                f"Screen {record.get('screen_id', 'unknown')} from {record.get('program_id', 'unknown')}"
            ]
        else:
            # UI paths attempts
            attempts = [
                record.get('guard_summary', ''),
                record.get('start_program_id', ''),
                record.get('end_program_id', ''),
                record.get('path_json', ''),
                f"UI path {record.get('path_id', 'unknown')} from {record.get('start_program_id', 'unknown')} to {record.get('end_program_id', 'unknown')}"
            ]
        
        for j, attempt in enumerate(attempts):
            if attempt and str(attempt).strip():
                text = str(attempt).strip()[:100]
                print(f"     Attempt {j+1}: '{text}' (length: {len(str(attempt).strip())})")
                
                # Test embedding
                embedding_result = test_embedding_generation(config, str(attempt).strip())
                if embedding_result:
                    print(f"       ✅ Embedding successful: {len(embedding_result)} dimensions")
                    break
                else:
                    print(f"       ❌ Embedding failed")
            else:
                print(f"     Attempt {j+1}: Empty/None")

def test_embedding_generation(config: Dict, text: str) -> List:
    """Test embedding generation for a text."""
    if not text or len(text.strip()) < 3:
        return None
        
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
            print(f"       API Error: {response.status_code} - {response.text[:100]}")
            return None
    except Exception as e:
        print(f"       Exception: {e}")
        return None

def test_record_updates(config: Dict, index_name: str):
    """Test if record updates are working."""
    print(f"\n🔧 TESTING RECORD UPDATES FOR {index_name}")
    print("-" * 50)
    
    # Get one sample record
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
        
        if response.status_code != 200:
            print(f"❌ Error fetching test record: {response.status_code}")
            return
            
        records = response.json().get('value', [])
        if not records:
            print(f"✅ No records without vectors found")
            return
        
        record = records[0]
        
    except Exception as e:
        print(f"❌ Exception fetching test record: {e}")
        return
    
    # Generate test embedding
    test_text = f"Test embedding for {index_name}"
    embedding = test_embedding_generation(config, test_text)
    
    if not embedding:
        print(f"❌ Could not generate test embedding")
        return
    
    print(f"✅ Generated test embedding: {len(embedding)} dimensions")
    
    # Try to update the record
    key_field = 'screen_id' if 'screen' in index_name else 'path_id'
    vector_field = 'summary_vector' if 'screen' in index_name else 'path_vector'
    
    record_key = record.get(key_field)
    if not record_key:
        print(f"❌ No {key_field} found in record")
        return
    
    update_url = f"{config['search_endpoint']}/indexes/{index_name}/docs/index?api-version=2023-11-01"
    
    update_doc = {
        '@search.action': 'merge',
        key_field: record_key,
        vector_field: embedding,
        'has_vector': True
    }
    
    update_body = {'value': [update_doc]}
    
    try:
        update_response = requests.post(
            update_url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=update_body,
            timeout=30
        )
        
        print(f"Update response: {update_response.status_code}")
        if update_response.status_code not in [200, 201]:
            print(f"❌ Update failed: {update_response.text}")
        else:
            print(f"✅ Update successful for record {record_key}")
            
    except Exception as e:
        print(f"❌ Update exception: {e}")

def investigate_failures():
    """Investigate why screen_nodes and ui_paths are failing."""
    config = load_config()
    
    print("🔍 FAILURE INVESTIGATION")
    print("=" * 60)
    print("Investigating why screen_nodes and ui_paths consistently fail")
    print(f"Azure Search: {config['search_endpoint']}")
    print(f"Azure OpenAI: {config['openai_endpoint']}")
    
    # Analyze both failing indexes
    failing_indexes = [
        'new_cobol_screen_nodes',
        'new_cobol_ui_paths'
    ]
    
    for index_name in failing_indexes:
        analyze_failing_records(config, index_name)
        test_record_updates(config, index_name)
    
    print(f"\n" + "=" * 60)
    print("🎯 INVESTIGATION COMPLETE")
    print("=" * 60)
    print("Check the analysis above to identify the root cause of failures")

if __name__ == '__main__':
    investigate_failures()