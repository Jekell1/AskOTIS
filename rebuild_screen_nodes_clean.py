"""Delete all documents from screen_nodes index and rebuild with readable IDs."""

import os
import json
import requests
import subprocess

# Load settings
try:
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        for key, value in settings.get('Values', {}).items():
            if key not in os.environ:
                os.environ[key] = value
except Exception:
    pass

def delete_all_docs():
    """Delete all documents from the index without deleting the index itself."""
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        print("❌ Missing search credentials")
        return False
    
    search_endpoint = search_endpoint.rstrip('/')
    
    print("\n🗑️  DELETING ALL SCREEN NODE DOCUMENTS...")
    print("=" * 80)
    
    # Fetch all document IDs in batches
    search_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    delete_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': search_key
    }
    
    total_deleted = 0
    batch_size = 1000
    
    while True:
        # Fetch a batch of document IDs
        payload = {
            "search": "*",
            "select": "screen_id",
            "top": batch_size
        }
        
        response = requests.post(search_url, headers=headers, json=payload)
        if not response.ok:
            print(f"❌ Error fetching documents: {response.status_code}")
            return False
        
        data = response.json()
        docs = data.get('value', [])
        
        if not docs:
            break
        
        # Create delete actions
        delete_actions = [
            {
                '@search.action': 'delete',
                'screen_id': doc['screen_id']
            }
            for doc in docs
        ]
        
        # Execute deletions
        delete_response = requests.post(delete_url, headers=headers, json={'value': delete_actions})
        if not delete_response.ok:
            print(f"❌ Error deleting batch: {delete_response.status_code}")
            print(delete_response.text[:200])
            return False
        
        total_deleted += len(docs)
        print(f"  Deleted {total_deleted} documents...")
        
        # If we got less than batch_size, we're done
        if len(docs) < batch_size:
            break
    
    print(f"\n✅ Deleted {total_deleted} old documents")
    return True


def rebuild_index():
    """Rebuild the index with readable IDs from source files."""
    print("\n🔨 REBUILDING INDEX WITH READABLE IDs...")
    print("=" * 80)
    
    # Run the ingest script
    result = subprocess.run(
        ['python', 'ingest/build_screen_nodes.py', '--push'],
        capture_output=True,
        text=True
    )
    
    print(result.stdout)
    if result.stderr:
        print(result.stderr)
    
    if result.returncode == 0:
        print("\n✅ Index rebuilt successfully")
        return True
    else:
        print(f"\n❌ Rebuild failed with exit code {result.returncode}")
        return False


def main():
    print("\n" + "=" * 80)
    print("SCREEN NODES INDEX - COMPLETE REBUILD")
    print("=" * 80)
    print()
    print("This will:")
    print("  1. Delete all existing screen node documents (old hash-based IDs)")
    print("  2. Rebuild from source files with readable program names")
    print()
    
    # Step 1: Delete all old documents
    if not delete_all_docs():
        print("\n❌ Failed to delete old documents")
        return
    
    # Step 2: Rebuild with new format
    if not rebuild_index():
        print("\n❌ Failed to rebuild index")
        return
    
    print("\n" + "=" * 80)
    print("✅ REBUILD COMPLETE")
    print("=" * 80)
    print()
    print("Screen IDs now use readable format:")
    print("  • LONPF2_SCR1, LONPF2_SCR2, ...")
    print("  • APIPAY_SCR1, ...")
    print("  • REGPAY_SCR1, ...")
    print()
    print("Instead of hash-based IDs like:")
    print("  • DD3268D4A57107E6E5918D9EB46C8036F8FDEE0F_SCREEN_1")
    print()


if __name__ == "__main__":
    main()
