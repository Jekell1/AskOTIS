"""
Add vector field to the recreated new_cobol_screen_nodes index.
"""
import os
import json
import requests
from secrets_loader import load_secrets

def add_vector_field():
    """Add vector field to the screen nodes index."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    print("🔧 ADDING VECTOR FIELD TO NEW_COBOL_SCREEN_NODES")
    print("=" * 60)
    
    # Get current index definition
    get_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes?api-version=2023-11-01"
    get_response = requests.get(get_url, headers=headers)
    
    if get_response.status_code != 200:
        print(f"❌ Failed to get index: {get_response.status_code}")
        return
    
    index_def = get_response.json()
    
    # Add vector field
    vector_field = {
        "name": "summary_vector",
        "type": "Collection(Edm.Single)",
        "searchable": True,
        "dimensions": 3072,
        "vectorSearchProfile": "vector-profile"
    }
    
    # Add vector search configuration
    index_def["vectorSearch"] = {
        "profiles": [{
            "name": "vector-profile",
            "algorithm": "hnsw"
        }],
        "algorithms": [{
            "name": "hnsw",
            "kind": "hnsw",
            "hnswParameters": {
                "metric": "cosine",
                "m": 4,
                "efConstruction": 400,
                "efSearch": 500
            }
        }]
    }
    
    # Add vector field to fields list
    index_def["fields"].append(vector_field)
    
    # Update index
    update_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes?api-version=2023-11-01"
    update_response = requests.put(update_url, headers=headers, json=index_def)
    
    if update_response.status_code in [200, 201]:
        print("✅ Vector field added successfully!")
        print("📊 Index now ready for embeddings")
    else:
        print(f"❌ Failed to add vector field: {update_response.status_code}")
        print(f"Error: {update_response.text}")

if __name__ == "__main__":
    add_vector_field()