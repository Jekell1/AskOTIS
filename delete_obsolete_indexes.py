"""Delete obsolete Azure AI Search indexes to free up storage space.

This script removes old/empty indexes that have been superseded by newer versions
or are no longer used in the current COBOL RAG system.

SAFE TO DELETE (Total: ~11.4 GB savings):
- cobol-index (0 docs) - Legacy line index
- cobol-routine-aliases (0 docs) - Empty, moved to new_cobol_name_aliases  
- cobol-copybooks-v2 (1,000 docs) - Test version
- cobol-facts-v2 (1,000 docs) - Test version
- cobol-copybooks (115,317 docs, 2.07 GB) - Superseded by new_cobol_copybook_meta
- cobol-facts (474,607 docs, 8.67 GB) - Superseded by cobol-facts-v3l
- cobol-file-chunks-v1 (32,774 docs, 679 MB) - Superseded by new_code_chunks

Usage:
    python delete_obsolete_indexes.py --dry-run    # Preview what would be deleted
    python delete_obsolete_indexes.py --confirm    # Actually delete the indexes
"""

import os
import sys
import json
import requests
import argparse
import time
from typing import List, Dict, Any

API_VERSION = '2023-11-01'

# Indexes safe to delete (superseded or empty)
SAFE_TO_DELETE = [
    'cobol-index',           # Empty (0 docs) - Legacy line index
    'cobol-routine-aliases', # Empty (0 docs) - Moved to new_cobol_name_aliases
    'cobol-copybooks-v2',    # Test version (1,000 docs) - Superseded by new_cobol_copybook_meta
    'cobol-facts-v2',        # Test version (1,000 docs) - Superseded by cobol-facts-v3
    'cobol-copybooks',       # Legacy (115,317 docs, 2.07 GB) - Superseded by new_cobol_copybook_meta
    'cobol-facts',           # Legacy (474,607 docs, 8.67 GB) - Superseded by cobol-facts-v3l
    'cobol-file-chunks-v1',  # Legacy (32,774 docs, 679 MB) - Superseded by new_code_chunks
]

# Indexes to consider for deletion (require manual verification)
CONSIDER_DELETING = [
    'new_cobol_flow_edges',  # V1 (368,454 docs, 13.46 GB) - V2 is current
]

def load_config():
    """Load Azure Search endpoint and key from environment or local.settings.json."""
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json', 'r', encoding='utf-8') as f:
                vals = json.load(f).get('Values', {})
        except Exception:
            pass
    
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    
    ep = first('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT')
    key = first('AZURE_SEARCH_KEY', 'SEARCH_KEY')
    
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    
    return ep.rstrip('/'), key

def get_index_info(ep: str, key: str, index_name: str) -> Dict[str, Any]:
    """Get basic info about an index (existence, doc count, storage)."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    # Check if index exists
    url = f"{ep}/indexes/{index_name}?api-version={API_VERSION}"
    r = requests.get(url, headers=headers)
    if r.status_code == 404:
        return {'exists': False, 'name': index_name}
    elif r.status_code != 200:
        return {'exists': False, 'name': index_name, 'error': f"Status {r.status_code}: {r.text[:200]}"}
    
    # Get document count
    search_url = f"{ep}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    search_body = {'search': '*', 'top': 0, 'count': True}
    r = requests.post(search_url, headers=headers, json=search_body)
    doc_count = 0
    if r.status_code == 200:
        doc_count = r.json().get('@odata.count', 0)
    
    return {
        'exists': True,
        'name': index_name,
        'doc_count': doc_count
    }

def delete_index(ep: str, key: str, index_name: str) -> bool:
    """Delete an index. Returns True if successful."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    url = f"{ep}/indexes/{index_name}?api-version={API_VERSION}"
    
    r = requests.delete(url, headers=headers)
    if r.status_code == 204:
        return True
    elif r.status_code == 404:
        print(f"   ⚠️  Index {index_name} already doesn't exist")
        return True
    else:
        print(f"   ❌ Failed to delete {index_name}: {r.status_code} - {r.text[:200]}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Delete obsolete Azure AI Search indexes')
    parser.add_argument('--dry-run', action='store_true', help='Preview what would be deleted (default)')
    parser.add_argument('--confirm', action='store_true', help='Actually delete the indexes')
    parser.add_argument('--include-flow-edges-v1', action='store_true', 
                       help='Also delete new_cobol_flow_edges (V1) - requires manual verification')
    
    args = parser.parse_args()
    
    if not args.dry_run and not args.confirm:
        print("No action specified. Use --dry-run to preview or --confirm to delete.")
        sys.exit(1)
    
    if args.confirm and args.dry_run:
        print("Cannot use both --dry-run and --confirm. Choose one.")
        sys.exit(1)
    
    ep, key = load_config()
    
    indexes_to_check = SAFE_TO_DELETE.copy()
    if args.include_flow_edges_v1:
        indexes_to_check.extend(CONSIDER_DELETING)
    
    print("🔍 OBSOLETE INDEX DELETION SCRIPT")
    print("=" * 50)
    print()
    
    if args.dry_run:
        print("🔍 DRY RUN MODE - No indexes will be deleted")
    else:
        print("⚠️  DELETION MODE - Indexes will be permanently deleted!")
        print("   Press Ctrl+C within 5 seconds to cancel...")
        for i in range(5, 0, -1):
            print(f"   Proceeding in {i} seconds...")
            time.sleep(1)
    
    print()
    print("📋 Checking indexes:")
    
    total_docs = 0
    existing_indexes = []
    
    for idx in indexes_to_check:
        info = get_index_info(ep, key, idx)
        if info['exists']:
            doc_count = info['doc_count']
            total_docs += doc_count
            existing_indexes.append(idx)
            print(f"   ✅ {idx}: {doc_count:,} documents")
        else:
            print(f"   ⚪ {idx}: Not found (already deleted)")
    
    print()
    print(f"📊 Summary:")
    print(f"   • Total existing indexes: {len(existing_indexes)}")
    print(f"   • Total documents to be deleted: {total_docs:,}")
    print()
    
    if not existing_indexes:
        print("✅ No obsolete indexes found to delete!")
        return
    
    if args.dry_run:
        print("📋 Indexes that would be deleted:")
        for idx in existing_indexes:
            if idx in SAFE_TO_DELETE:
                print(f"   🗑️  {idx} (safe to delete)")
            else:
                print(f"   ⚠️  {idx} (requires verification)")
        print()
        print("💡 To actually delete these indexes, run with --confirm")
        return
    
    # Actual deletion
    print("🗑️  Deleting indexes...")
    deleted_count = 0
    
    for idx in existing_indexes:
        print(f"   Deleting {idx}...")
        if delete_index(ep, key, idx):
            print(f"   ✅ Deleted {idx}")
            deleted_count += 1
        else:
            print(f"   ❌ Failed to delete {idx}")
    
    print()
    print(f"📊 Deletion Summary:")
    print(f"   • Successfully deleted: {deleted_count}/{len(existing_indexes)} indexes")
    print(f"   • Estimated storage freed: ~11.4 GB")
    print()
    
    if deleted_count == len(existing_indexes):
        print("🎉 All obsolete indexes successfully deleted!")
    else:
        print("⚠️  Some indexes could not be deleted. Check errors above.")

if __name__ == '__main__':
    main()