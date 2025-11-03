"""Count old vs new format screen IDs."""

import os
import json
import requests

# Load settings
try:
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        for key, value in settings.get('Values', {}).items():
            if key not in os.environ:
                os.environ[key] = value
except Exception:
    pass

def analyze_formats():
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        print("❌ Missing search credentials")
        return
    
    search_endpoint = search_endpoint.rstrip('/')
    url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': search_key
    }
    
    # Get a large sample
    payload = {
        "search": "*",
        "select": "screen_id,program_id",
        "top": 100
    }
    
    response = requests.post(url, headers=headers, json=payload)
    if not response.ok:
        print(f"❌ Error: {response.status_code}")
        return
    
    data = response.json()
    docs = data.get('value', [])
    
    old_format = 0
    new_format = 0
    
    for doc in docs:
        screen_id = doc.get('screen_id', '')
        program_id = doc.get('program_id', '')
        
        # New format: program_id is readable (e.g., LONPF2) and matches beginning of screen_id
        if len(program_id) < 20 and screen_id.startswith(program_id + '_'):
            new_format += 1
        else:
            old_format += 1
    
    print(f"\n📊 Sample of {len(docs)} documents:")
    print(f"  Old hash-based format: {old_format} ({old_format/len(docs)*100:.1f}%)")
    print(f"  New readable format:   {new_format} ({new_format/len(docs)*100:.1f}%)")
    print()
    
    if old_format > 0:
        print("⚠️  The index contains a mix of old and new formats")
        print("📋 RECOMMENDATION: Delete old documents or rebuild index completely")
        print()
        print("   To delete all documents and rebuild fresh:")
        print("   1. Delete and recreate index (preserves schema)")
        print("   2. Re-run: python ingest/build_screen_nodes.py --push")
    else:
        print("✅ All documents use the new readable format!")


if __name__ == "__main__":
    analyze_formats()
