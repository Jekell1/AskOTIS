#!/usr/bin/env python3
"""Clear all documents from new_code_chunks index using upload with delete action."""

import requests
import os
import json
from dotenv import load_dotenv

def load_settings():
    """Load settings from local.settings.json if available."""
    try:
        data = json.load(open('local.settings.json', 'r'))
        vals = data.get('Values', {})
        for k in ['SEARCH_ENDPOINT', 'SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_dotenv()
load_settings()

ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

if not ep or not key:
    print("❌ Missing SEARCH_ENDPOINT or SEARCH_KEY environment variables")
    exit(1)

print("🗑️  Clearing new_code_chunks index...")
print("=" * 80)

# Use POST search to get document count
search_url = f'{ep}/indexes/new_code_chunks/docs/search?api-version=2024-07-01'
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': '*',
    'select': 'chunk_id',
    'count': True,
    'top': 0  # Just want the count
}

r = requests.post(search_url, headers=headers, json=payload)
if r.status_code == 200:
    count = r.json().get('@odata.count', 0)
    print(f"Current document count: {count:,}")
else:
    print(f"❌ Failed to get count: {r.status_code}")
    print(r.text[:500])
    exit(1)

if count == 0:
    print("✅ Index is already empty")
    exit(0)

# Confirm deletion
print(f"\n⚠️  This will delete ALL {count:,} documents from new_code_chunks")
print("⚠️  This cannot be undone!")
response = input("\nType 'DELETE' to confirm: ")

if response != 'DELETE':
    print("❌ Aborted")
    exit(0)

# Fetch all document IDs in batches
print("\n📋 Fetching document IDs...")

all_ids = []
skip = 0
batch_size = 1000

while skip < count:
    payload = {
        'search': '*',
        'select': 'chunk_id',
        'top': batch_size,
        'skip': skip
    }
    
    r = requests.post(search_url, headers=headers, json=payload)
    
    if r.status_code != 200:
        print(f"\n❌ Search failed at skip={skip}: {r.status_code}")
        print(r.text[:500])
        break
    
    batch = r.json().get('value', [])
    if not batch:
        break
    
    all_ids.extend([doc['chunk_id'] for doc in batch])
    skip += len(batch)
    
    print(f"  Fetched {len(all_ids):,}/{count:,} IDs...", end='\r')
    
    if len(batch) < batch_size:
        break

print(f"\n✅ Fetched {len(all_ids):,} document IDs")

if not all_ids:
    print("⚠️  No documents found to delete")
    exit(0)

# Delete in batches
print("\n🗑️  Deleting documents...")

delete_url = f'{ep}/indexes/new_code_chunks/docs/index?api-version=2024-07-01'

deleted = 0
delete_batch_size = 1000

for i in range(0, len(all_ids), delete_batch_size):
    batch_ids = all_ids[i:i+delete_batch_size]
    
    actions = [
        {'@search.action': 'delete', 'chunk_id': cid}
        for cid in batch_ids
    ]
    
    r = requests.post(delete_url, headers=headers, json={'value': actions})
    
    if r.status_code in (200, 201):
        deleted += len(batch_ids)
    elif r.status_code == 207:
        result = r.json()
        successful = [item for item in result.get('value', []) if item.get('status')]
        deleted += len(successful)
        print(f"\n  Batch {i//delete_batch_size + 1}: Partial success ({len(successful)}/{len(batch_ids)})")
    else:
        print(f"\n❌ Delete failed at batch {i//delete_batch_size + 1}: {r.status_code}")
        print(r.text[:500])
        break
    
    print(f"  Deleted {deleted:,}/{len(all_ids):,} documents...", end='\r')

print(f"\n\n✅ Deletion complete: {deleted:,} documents deleted")

# Verify
print("\n🔍 Verifying...")
payload = {'search': '*', 'count': True, 'top': 0}
r = requests.post(search_url, headers=headers, json=payload)
if r.status_code == 200:
    remaining = r.json().get('@odata.count', -1)
    print(f"Remaining documents: {remaining:,}")
    
    if remaining == 0:
        print("✅ Index successfully cleared!")
    else:
        print(f"⚠️  {remaining:,} documents remain")
        print("Note: Azure Search updates may take a few moments to reflect")

print("\n" + "=" * 80)
print("✅ Ready for re-ingestion")
print("Next: python ingest_code_chunks.py --include-copybooks --verbose")
