#!/usr/bin/env python3
"""
Emergency delete the runaway indexer
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def delete_runaway_indexer():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== EMERGENCY INDEXER DELETION ===")
    
    indexer_name = "cobol-jsonl-indexer"
    
    # Confirm deletion
    print(f"⚠️  About to DELETE indexer: {indexer_name}")
    print("This indexer is running every 4 hours and has processed 400K+ items")
    print("It's causing the abnormally high document count (4.3M+)")
    
    user_input = input("\nConfirm deletion (type 'DELETE' to proceed): ")
    
    if user_input != 'DELETE':
        print("❌ Deletion cancelled")
        return
    
    # Delete the indexer
    delete_url = f'{endpoint}/indexers/{indexer_name}?api-version=2023-07-01-preview'
    
    try:
        print(f"\n🗑️  Deleting {indexer_name}...")
        response = requests.delete(delete_url, headers=headers)
        
        if response.status_code == 204:
            print("✅ Indexer deleted successfully!")
            print("\nThe runaway indexer is now removed.")
            print("This should stop the continuous indexing that was causing")
            print("the abnormally high document count.")
            
        elif response.status_code == 404:
            print("❌ Indexer not found (may already be deleted)")
            
        else:
            print(f"❌ Deletion failed: {response.status_code}")
            print(response.text)
            
    except Exception as e:
        print(f"Error deleting indexer: {e}")
    
    print("\n=== NEXT STEPS ===")
    print("1. Wait a few minutes for the index to stop processing")
    print("2. Check the document count again")
    print("3. The count should stabilize once active indexing stops")
    print("4. If needed, we can rebuild the index cleanly")
    print("\nThe original 'cobol-indexer' (for .CBL files) should remain intact.")

if __name__ == "__main__":
    delete_runaway_indexer()
