#!/usr/bin/env python3
"""
DIRECT EMBEDDINGS VERIFICATION
Directly checks specific records to verify embeddings were generated.
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
    }

def check_specific_records(config: Dict, index_name: str, vector_field: str, sample_size: int = 10) -> Dict:
    """Check specific records to see if they have embeddings."""
    url = f"{config['search_endpoint']}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    body = {
        'search': '*',
        'top': sample_size,
        'select': f'*'
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
            
            total_docs = len(docs)
            with_vectors = 0
            vector_lengths = []
            
            for doc in docs:
                vector_value = doc.get(vector_field)
                if vector_value and isinstance(vector_value, list) and len(vector_value) > 0:
                    with_vectors += 1
                    vector_lengths.append(len(vector_value))
            
            return {
                'total_checked': total_docs,
                'with_vectors': with_vectors,
                'coverage_percent': (with_vectors / total_docs * 100) if total_docs > 0 else 0,
                'vector_lengths': vector_lengths,
                'avg_vector_length': sum(vector_lengths) / len(vector_lengths) if vector_lengths else 0
            }
        else:
            print(f"Error checking {index_name}: {response.status_code}")
            return None
            
    except Exception as e:
        print(f"Error checking {index_name}: {e}")
        return None

def verify_priority_1_results():
    """Verify that Priority 1 embeddings were actually generated."""
    config = load_config()
    
    print("🔍 DIRECT EMBEDDINGS VERIFICATION")
    print("=" * 50)
    
    # Priority 1 indexes that we just processed
    checks = [
        {
            'index': 'new_cobol_program_flows',
            'vector_field': 'flow_vector',
            'description': 'Program flow graphs'
        },
        {
            'index': 'new_cobol_program_deps', 
            'vector_field': 'dependency_blob_vector',
            'description': 'Program dependencies'
        },
        {
            'index': 'new_cobol_screen_nodes',
            'vector_field': 'summary_vector',
            'description': 'Screen structures'
        },
        {
            'index': 'new_cobol_ui_paths',
            'vector_field': 'path_vector',
            'description': 'UI navigation paths'
        }
    ]
    
    for check in checks:
        print(f"\n📊 {check['description']}")
        print(f"   Index: {check['index']}")
        print(f"   Vector Field: {check['vector_field']}")
        print("-" * 40)
        
        result = check_specific_records(config, check['index'], check['vector_field'], 50)
        
        if result:
            print(f"   ✅ Checked: {result['total_checked']} records")
            print(f"   📈 With vectors: {result['with_vectors']}")
            print(f"   📊 Coverage: {result['coverage_percent']:.1f}%")
            print(f"   📏 Avg vector length: {result['avg_vector_length']:.0f}")
            
            if result['coverage_percent'] > 0:
                print(f"   🎉 SUCCESS: Embeddings detected!")
            else:
                print(f"   ❌ ISSUE: No embeddings found in sample")
        else:
            print(f"   ❌ ERROR: Could not check index")

    print(f"\n🎯 ANALYSIS COMPARISON:")
    print("If verification shows embeddings but analysis shows 0%:")
    print("  → Azure Search may have indexing delays")
    print("  → Try again in 5-10 minutes")
    print("  → Check if vector search is enabled on fields")

if __name__ == '__main__':
    verify_priority_1_results()