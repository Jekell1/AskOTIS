"""Backup and delete redundant Azure Search indexes.

This script will:
1. Export all documents from each redundant index to a compressed JSON file
2. Save the index schema for future reference
3. Delete the index after successful backup

Indexes to remove (2.4M documents total):
1. code-chunks (216k) - replaced by new_code_chunks
2. new_cobol_calls (137k) - 95% false positives, use flow_edges_v2
3. new_cobol_program_deps (10k) - replaced by flow_edges_v2
4. new_cobol_screens (1.4k) - replaced by screen_nodes
5. new_cobol_symbol_refs (1.9M) - too granular
6. new_cobol_menu_trees (10k) - can rebuild
7. new_cobol_name_aliases (56k) - rarely used
8. program_complexity (1.7k) - analysis data
9. transaction_taxonomy (51) - classification data

Usage:
  python backup_and_delete_redundant_indexes.py --backup-only  # Just backup, don't delete
  python backup_and_delete_redundant_indexes.py --delete       # Backup AND delete
"""
import json
import requests
import gzip
import time
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

ENDPOINT = settings['SEARCH_ENDPOINT'].rstrip('/')
KEY = settings['SEARCH_KEY']
API_VERSION = '2025-08-01-preview'

# Indexes to backup and delete
REDUNDANT_INDEXES = [
    {'name': 'code-chunks', 'docs': 216000, 'reason': 'replaced by new_code_chunks'},
    {'name': 'new_cobol_calls', 'docs': 137000, 'reason': '95% false positives, use flow_edges_v2'},
    {'name': 'new_cobol_program_deps', 'docs': 10000, 'reason': 'replaced by flow_edges_v2'},
    {'name': 'new_cobol_screens', 'docs': 1400, 'reason': 'replaced by screen_nodes'},
    {'name': 'new_cobol_symbol_refs', 'docs': 1925763, 'reason': 'too granular'},
    {'name': 'new_cobol_menu_trees', 'docs': 10000, 'reason': 'can rebuild'},
    {'name': 'new_cobol_name_aliases', 'docs': 56000, 'reason': 'rarely used'},
    {'name': 'program_complexity', 'docs': 1700, 'reason': 'analysis data'},
    {'name': 'transaction_taxonomy', 'docs': 51, 'reason': 'classification data'}
]

BACKUP_DIR = Path('index_backups')
BACKUP_DIR.mkdir(exist_ok=True)

TIMESTAMP = datetime.now().strftime('%Y%m%d_%H%M%S')


def check_index_exists(index_name: str) -> Tuple[bool, int]:
    """Check if index exists and return document count."""
    url = f"{ENDPOINT}/indexes/{index_name}?api-version={API_VERSION}"
    headers = {'api-key': KEY}
    
    response = requests.get(url, headers=headers)
    if response.status_code == 404:
        return False, 0
    elif response.status_code != 200:
        print(f"  ⚠️  Error checking index: {response.status_code}")
        return False, 0
    
    # Get document count
    search_url = f"{ENDPOINT}/indexes/{index_name}/docs/$count?api-version={API_VERSION}"
    count_response = requests.get(search_url, headers=headers)
    
    if count_response.status_code == 200:
        count = int(count_response.text)
        return True, count
    else:
        return True, 0


def backup_index_schema(index_name: str) -> bool:
    """Backup index schema to JSON file."""
    print(f"  📋 Backing up schema...")
    
    url = f"{ENDPOINT}/indexes/{index_name}?api-version={API_VERSION}"
    headers = {'api-key': KEY}
    
    response = requests.get(url, headers=headers)
    if response.status_code != 200:
        print(f"  ❌ Failed to get schema: {response.status_code}")
        return False
    
    schema = response.json()
    schema_file = BACKUP_DIR / f"{index_name}_schema_{TIMESTAMP}.json"
    
    with open(schema_file, 'w', encoding='utf-8') as f:
        json.dump(schema, f, indent=2, ensure_ascii=False)
    
    print(f"  ✅ Schema saved: {schema_file.name}")
    return True


def backup_index_documents(index_name: str, estimated_docs: int) -> Tuple[bool, int]:
    """Backup all documents from index to compressed JSON file."""
    print(f"  💾 Backing up documents...")
    
    search_url = f"{ENDPOINT}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    headers = {'api-key': KEY, 'Content-Type': 'application/json'}
    
    all_docs = []
    batch_size = 1000
    skip = 0
    
    while True:
        search_body = {
            "search": "*",
            "top": batch_size,
            "skip": skip
        }
        
        try:
            response = requests.post(search_url, headers=headers, json=search_body, timeout=60)
            if response.status_code != 200:
                print(f"  ❌ Error fetching batch at skip={skip}: {response.status_code}")
                break
            
            results = response.json()
            batch = results.get('value', [])
            
            if not batch:
                break
            
            all_docs.extend(batch)
            skip += batch_size
            
            if skip % 10000 == 0:
                print(f"  📦 Fetched {len(all_docs):,} documents...")
            
            # Safety check for large indexes
            if skip > 150000 and len(all_docs) < skip * 0.5:
                print(f"  ⚠️  Possible skip limit reached, switching to $skiptoken...")
                # For very large indexes, would need to implement continuation token logic
                break
                
        except Exception as e:
            print(f"  ❌ Error during backup: {e}")
            break
    
    if not all_docs:
        print(f"  ⚠️  No documents retrieved")
        return False, 0
    
    # Save to compressed JSON
    docs_file = BACKUP_DIR / f"{index_name}_docs_{TIMESTAMP}.json.gz"
    
    with gzip.open(docs_file, 'wt', encoding='utf-8') as f:
        json.dump(all_docs, f, indent=2, ensure_ascii=False)
    
    size_mb = docs_file.stat().st_size / (1024 * 1024)
    print(f"  ✅ Documents saved: {docs_file.name}")
    print(f"  📊 Count: {len(all_docs):,} documents")
    print(f"  💿 Size: {size_mb:.2f} MB (compressed)")
    
    return True, len(all_docs)


def delete_index(index_name: str) -> bool:
    """Delete the index."""
    print(f"  🗑️  Deleting index...")
    
    url = f"{ENDPOINT}/indexes/{index_name}?api-version={API_VERSION}"
    headers = {'api-key': KEY}
    
    response = requests.delete(url, headers=headers)
    if response.status_code in [200, 204]:
        print(f"  ✅ Index deleted successfully")
        return True
    else:
        print(f"  ❌ Failed to delete: {response.status_code}")
        print(f"     {response.text[:200]}")
        return False


def process_index(index_info: Dict, delete_after_backup: bool) -> Dict:
    """Process one index: backup and optionally delete."""
    index_name = index_info['name']
    estimated_docs = index_info['docs']
    reason = index_info['reason']
    
    print(f"\n{'='*70}")
    print(f"INDEX: {index_name}")
    print(f"Expected docs: ~{estimated_docs:,}")
    print(f"Reason: {reason}")
    print(f"{'='*70}")
    
    result = {
        'index': index_name,
        'exists': False,
        'schema_backed_up': False,
        'docs_backed_up': False,
        'actual_doc_count': 0,
        'deleted': False,
        'error': None
    }
    
    # Check if index exists
    exists, actual_count = check_index_exists(index_name)
    result['exists'] = exists
    result['actual_doc_count'] = actual_count
    
    if not exists:
        print(f"  ⚠️  Index does not exist (already deleted?)")
        return result
    
    print(f"  📊 Actual document count: {actual_count:,}")
    
    # Backup schema
    schema_ok = backup_index_schema(index_name)
    result['schema_backed_up'] = schema_ok
    
    if not schema_ok:
        result['error'] = 'Schema backup failed'
        return result
    
    # Backup documents
    if actual_count > 0:
        docs_ok, backed_up_count = backup_index_documents(index_name, actual_count)
        result['docs_backed_up'] = docs_ok
        
        if not docs_ok:
            result['error'] = 'Document backup failed'
            return result
        
        # Check if we got most documents (allow for some variance)
        if backed_up_count < actual_count * 0.95:
            print(f"  ⚠️  WARNING: Only backed up {backed_up_count:,} of {actual_count:,} documents")
            result['error'] = f'Incomplete backup: {backed_up_count}/{actual_count}'
    else:
        print(f"  ℹ️  Index is empty")
        result['docs_backed_up'] = True
    
    # Delete if requested
    if delete_after_backup:
        if result['schema_backed_up'] and result['docs_backed_up']:
            print()
            delete_ok = delete_index(index_name)
            result['deleted'] = delete_ok
        else:
            print(f"  ⚠️  Skipping deletion due to backup issues")
    else:
        print(f"  ℹ️  Skipping deletion (--backup-only mode)")
    
    return result


def main():
    parser = argparse.ArgumentParser(description='Backup and delete redundant Azure Search indexes')
    parser.add_argument('--delete', action='store_true', help='Delete indexes after backup')
    parser.add_argument('--backup-only', action='store_true', help='Only backup, do not delete')
    args = parser.parse_args()
    
    delete_after_backup = args.delete and not args.backup_only
    
    print("🗄️  AZURE SEARCH INDEX CLEANUP")
    print("=" * 70)
    print(f"Timestamp: {TIMESTAMP}")
    print(f"Backup directory: {BACKUP_DIR.absolute()}")
    print(f"Mode: {'BACKUP + DELETE' if delete_after_backup else 'BACKUP ONLY'}")
    print(f"Indexes to process: {len(REDUNDANT_INDEXES)}")
    
    if delete_after_backup:
        print()
        print("⚠️  WARNING: Indexes will be DELETED after backup!")
        print("Press Ctrl+C within 5 seconds to cancel...")
        try:
            time.sleep(5)
        except KeyboardInterrupt:
            print("\n❌ Cancelled by user")
            return
    
    results = []
    
    for index_info in REDUNDANT_INDEXES:
        result = process_index(index_info, delete_after_backup)
        results.append(result)
    
    # Summary
    print(f"\n{'='*70}")
    print("📊 SUMMARY")
    print(f"{'='*70}")
    
    total_docs_backed_up = sum(r['actual_doc_count'] for r in results if r['exists'])
    indexes_deleted = sum(1 for r in results if r['deleted'])
    indexes_with_errors = sum(1 for r in results if r['error'])
    
    print(f"\nProcessed: {len(results)} indexes")
    print(f"Existed: {sum(1 for r in results if r['exists'])}")
    print(f"Documents backed up: {total_docs_backed_up:,}")
    print(f"Deleted: {indexes_deleted}")
    
    if indexes_with_errors > 0:
        print(f"\n⚠️  Errors: {indexes_with_errors}")
        for r in results:
            if r['error']:
                print(f"  • {r['index']}: {r['error']}")
    
    print(f"\n✅ Backup location: {BACKUP_DIR.absolute()}")
    
    if delete_after_backup and indexes_deleted > 0:
        print(f"\n🎉 Successfully deleted {indexes_deleted} redundant indexes!")
        print(f"💾 Storage saved: ~{total_docs_backed_up:,} documents")
    
    # Save summary
    summary_file = BACKUP_DIR / f"deletion_summary_{TIMESTAMP}.json"
    with open(summary_file, 'w') as f:
        json.dump({
            'timestamp': TIMESTAMP,
            'mode': 'delete' if delete_after_backup else 'backup_only',
            'results': results,
            'total_docs_backed_up': total_docs_backed_up,
            'indexes_deleted': indexes_deleted
        }, f, indent=2)
    
    print(f"📋 Summary saved: {summary_file.name}")


if __name__ == '__main__':
    main()
