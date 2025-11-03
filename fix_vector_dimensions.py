#!/usr/bin/env python3
"""
VECTOR DIMENSION FIX
Fixes the vector dimension mismatch by checking deployment and updating indexes.
"""

import requests
import os
import json
from typing import Dict
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

def check_embedding_deployment(config: Dict):
    """Check the embedding deployment details."""
    print("🔍 CHECKING EMBEDDING DEPLOYMENT")
    print("-" * 50)
    
    deployment_name = os.getenv('AZURE_OPENAI_EMBED_DEPLOYMENT', 'text-embedding-ada-002')
    print(f"Deployment name: {deployment_name}")
    
    # Test embedding to get actual dimensions
    url = f"{config['openai_endpoint']}/openai/deployments/{deployment_name}/embeddings?api-version={config['openai_version']}"
    
    headers = {
        'api-key': config['openai_key'],
        'Content-Type': 'application/json'
    }
    
    body = {'input': 'test'}
    
    try:
        response = requests.post(url, headers=headers, json=body, timeout=30)
        if response.status_code == 200:
            embedding = response.json()['data'][0]['embedding']
            print(f"✅ Actual embedding dimensions: {len(embedding)}")
            return len(embedding)
        else:
            print(f"❌ API Error: {response.status_code} - {response.text}")
            return None
    except Exception as e:
        print(f"❌ Exception: {e}")
        return None

def check_index_schema(config: Dict, index_name: str):
    """Check the vector field dimensions in index schema."""
    print(f"\n🔍 CHECKING {index_name} SCHEMA")
    print("-" * 50)
    
    url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"❌ Error fetching schema: {response.status_code}")
            return None
            
        schema = response.json()
        
        # Find vector fields
        vector_fields = []
        for field in schema.get('fields', []):
            if field.get('type') == 'Collection(Edm.Single)' and 'vector' in field.get('name', '').lower():
                dimensions = field.get('dimensions')
                vector_fields.append({
                    'name': field['name'],
                    'dimensions': dimensions
                })
                print(f"Vector field: {field['name']} - {dimensions} dimensions")
        
        return vector_fields
        
    except Exception as e:
        print(f"❌ Exception: {e}")
        return None

def update_vector_field_dimensions(config: Dict, index_name: str, vector_field_name: str, new_dimensions: int):
    """Update vector field dimensions in index."""
    print(f"\n🔧 UPDATING {index_name}.{vector_field_name} TO {new_dimensions} DIMENSIONS")
    print("-" * 70)
    
    # Get current schema
    url = f"{config['search_endpoint']}/indexes/{index_name}?api-version=2023-11-01"
    
    try:
        response = requests.get(
            url,
            headers={'api-key': config['search_key']},
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"❌ Error fetching schema: {response.status_code}")
            return False
            
        schema = response.json()
        
        # Update vector field dimensions
        updated = False
        for field in schema.get('fields', []):
            if field['name'] == vector_field_name:
                old_dimensions = field.get('dimensions')
                field['dimensions'] = new_dimensions
                print(f"Updated {vector_field_name}: {old_dimensions} → {new_dimensions} dimensions")
                updated = True
                break
        
        if not updated:
            print(f"❌ Vector field {vector_field_name} not found")
            return False
        
        # Update the index
        update_response = requests.put(
            url,
            headers={'api-key': config['search_key'], 'Content-Type': 'application/json'},
            json=schema,
            timeout=60
        )
        
        if update_response.status_code in [200, 204]:
            print(f"✅ Successfully updated {index_name}")
            return True
        else:
            print(f"❌ Update failed: {update_response.status_code} - {update_response.text}")
            return False
            
    except Exception as e:
        print(f"❌ Exception: {e}")
        return False

def fix_vector_dimensions():
    """Fix vector dimension mismatches."""
    config = load_config()
    
    print("🔧 VECTOR DIMENSION FIX")
    print("=" * 60)
    
    # Check actual embedding dimensions
    actual_dimensions = check_embedding_deployment(config)
    if not actual_dimensions:
        print("❌ Cannot determine actual embedding dimensions")
        return
    
    # Check and fix both indexes
    indexes_to_fix = [
        ('new_cobol_screen_nodes', 'summary_vector'),
        ('new_cobol_ui_paths', 'path_vector')
    ]
    
    for index_name, vector_field in indexes_to_fix:
        # Check current schema
        vector_fields = check_index_schema(config, index_name)
        if not vector_fields:
            continue
        
        # Find the vector field
        field_info = next((f for f in vector_fields if f['name'] == vector_field), None)
        if not field_info:
            print(f"❌ Vector field {vector_field} not found in {index_name}")
            continue
        
        current_dimensions = field_info['dimensions']
        if current_dimensions != actual_dimensions:
            print(f"🔧 Dimension mismatch: {current_dimensions} (index) vs {actual_dimensions} (model)")
            
            # Update the dimensions
            success = update_vector_field_dimensions(config, index_name, vector_field, actual_dimensions)
            if success:
                print(f"✅ Fixed {index_name}.{vector_field}")
            else:
                print(f"❌ Failed to fix {index_name}.{vector_field}")
        else:
            print(f"✅ {index_name}.{vector_field} dimensions already correct")
    
    print(f"\n" + "=" * 60)
    print("🎯 DIMENSION FIX COMPLETE")
    print("=" * 60)
    print("The vector dimension mismatches should now be resolved")

if __name__ == '__main__':
    fix_vector_dimensions()