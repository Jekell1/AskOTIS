#!/usr/bin/env python3
"""
Verify that embeddings were actually stored in the indexes.
"""

import requests
import os
from secrets_loader import load_secrets

def load_config():
    """Load Azure Search configuration."""
    load_secrets()
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        raise ValueError("Missing Azure Search configuration")
    
    return search_endpoint.rstrip('/'), search_key

def check_specific_embeddings(endpoint: str, key: str):
    """Check specific records to see if embeddings were stored."""
    
    # Check the indexes we just processed
    test_indexes = [
        ('new_cobol_program_meta', 'summary_vector'),
        ('new_cobol_calls', 'snippet_vector'), 
        ('new_cobol_copybook_meta', 'summary_vector'),
        ('new_cobol_program_flows', 'flow_vector'),
        ('new_cobol_program_deps', 'dependency_blob_vector'),
    ]
    
    for index_name, vector_field in test_indexes:
        print(f"\n=== Checking {index_name}.{vector_field} ===")
        
        url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Search for records with vectors
        body = {
            'search': '*',
            'select': f'{vector_field},has_vector',
            'top': 5
        }
        
        try:
            response = requests.post(
                url,
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json=body,
                timeout=30
            )
            
            if response.status_code == 200:
                data = response.json()
                docs = data.get('value', [])
                
                vectors_found = 0
                for doc in docs:
                    vector_data = doc.get(vector_field)
                    has_vector_flag = doc.get('has_vector')
                    
                    if vector_data and len(vector_data) > 0:
                        vectors_found += 1
                        print(f"  ✓ Found vector with {len(vector_data)} dimensions")
                    elif has_vector_flag:
                        print(f"  ⚠ has_vector=true but no vector data")
                    else:
                        print(f"  ✗ No vector data")
                
                print(f"  Summary: {vectors_found}/{len(docs)} records have vectors")
                
            else:
                print(f"  Error: {response.status_code}")
                
        except Exception as e:
            print(f"  Exception: {e}")

def main():
    print("=== EMBEDDINGS VERIFICATION ===")
    
    endpoint, key = load_config()
    print(f"Checking: {endpoint}")
    
    check_specific_embeddings(endpoint, key)

if __name__ == '__main__':
    main()