"""
Quick status check for screen nodes embedding progress.
"""
import os
import requests
from secrets_loader import load_secrets

def check_embedding_progress():
    """Check embedding progress for screen nodes."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    search_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    # Count records with vectors
    with_vectors_body = {
        "search": "*",
        "filter": "has_vector eq true",
        "top": 0,
        "count": True
    }
    
    with_vectors_response = requests.post(search_url, headers=headers, json=with_vectors_body)
    
    # Count records without vectors
    without_vectors_body = {
        "search": "*",
        "filter": "has_vector eq false",
        "top": 0,
        "count": True
    }
    
    without_vectors_response = requests.post(search_url, headers=headers, json=without_vectors_body)
    
    # Total count
    total_body = {
        "search": "*",
        "top": 0,
        "count": True
    }
    
    total_response = requests.post(search_url, headers=headers, json=total_body)
    
    if all(r.status_code == 200 for r in [with_vectors_response, without_vectors_response, total_response]):
        with_vectors = with_vectors_response.json().get('@odata.count', 0)
        without_vectors = without_vectors_response.json().get('@odata.count', 0)
        total = total_response.json().get('@odata.count', 0)
        
        progress_pct = (with_vectors / total * 100) if total > 0 else 0
        
        print(f"📊 SCREEN NODES EMBEDDING PROGRESS")
        print(f"   With vectors: {with_vectors:,}")
        print(f"   Without vectors: {without_vectors:,}")
        print(f"   Total: {total:,}")
        print(f"   Progress: {progress_pct:.1f}%")
        
        if with_vectors > 0:
            print("✅ Embedding process is working!")
        else:
            print("⏳ Embeddings not started yet or still in progress...")

if __name__ == "__main__":
    check_embedding_progress()