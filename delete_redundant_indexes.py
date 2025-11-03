#!/usr/bin/env python3
"""Delete 9 redundant Azure Search indexes.

Targets (2.4M documents total):
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
    python delete_redundant_indexes.py
"""
import os
import sys
import json
import requests
import time
from datetime import datetime

REDUNDANT_INDEXES = [
    {'name': 'code-chunks', 'reason': 'replaced by new_code_chunks'},
    {'name': 'new_cobol_calls', 'reason': '95% false positives, use flow_edges_v2'},
    {'name': 'new_cobol_program_deps', 'reason': 'replaced by flow_edges_v2'},
    {'name': 'new_cobol_screens', 'reason': 'replaced by screen_nodes'},
    {'name': 'new_cobol_symbol_refs', 'reason': 'too granular'},
    {'name': 'new_cobol_menu_trees', 'reason': 'can rebuild'},
    {'name': 'new_cobol_name_aliases', 'reason': 'rarely used'},
    {'name': 'program_complexity', 'reason': 'analysis data'},
    {'name': 'transaction_taxonomy', 'reason': 'classification data'}
]

def load_config():
    """Load Azure Search config."""
    vals = json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
    for k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY'):
        if k in vals and k not in os.environ:
            os.environ[k] = vals[k]
    
    ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    return ep, key


def check_index_exists(ep, key, index_name):
    """Check if index exists and return document count."""
    try:
        # Check if index exists
        r = requests.get(
            f'{ep}/indexes/{index_name}?api-version=2025-08-01-preview',
            headers={'api-key': key},
            timeout=10
        )
        if r.status_code == 404:
            return False, 0
        
        # Get document count
        count_r = requests.post(
            f'{ep}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview',
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json={'top': 0, 'count': True},
            timeout=10
        )
        count = count_r.json().get('@odata.count', 0)
        return True, count
    except Exception:
        return False, 0


def delete_index(ep, key, index_name):
    """Delete the index."""
    try:
        r = requests.delete(
            f'{ep}/indexes/{index_name}?api-version=2025-08-01-preview',
            headers={'api-key': key},
            timeout=30
        )
        return r.status_code in (200, 204)
    except Exception as e:
        print(f"     Error: {e}")
        return False


def main():
    print("\n🗑️  DELETE REDUNDANT AZURE SEARCH INDEXES")
    print("=" * 70)
    print(f"Indexes to delete: {len(REDUNDANT_INDEXES)}")
    print()
    
    ep, key = load_config()
    
    # Show what will be deleted
    print("⚠️  WARNING: This will permanently delete these indexes!")
    print()
    total_docs = 0
    for idx in REDUNDANT_INDEXES:
        exists, count = check_index_exists(ep, key, idx['name'])
        if exists:
            total_docs += count
            print(f"  • {idx['name']:<30} ({count:,} docs) - {idx['reason']}")
        else:
            print(f"  • {idx['name']:<30} (not found) - {idx['reason']}")
    
    print()
    print(f"📊 Total documents to remove: ~{total_docs:,}")
    print()
    print("Press Ctrl+C within 5 seconds to cancel...")
    
    try:
        time.sleep(5)
    except KeyboardInterrupt:
        print("\n❌ Cancelled by user")
        return 1
    
    print()
    print("Starting deletion...")
    print()
    
    results = {
        'deleted': 0,
        'not_found': 0,
        'errors': 0,
        'total_docs_removed': 0
    }
    
    for idx in REDUNDANT_INDEXES:
        index_name = idx['name']
        reason = idx['reason']
        
        print(f"{'='*70}")
        print(f"INDEX: {index_name}")
        print(f"Reason: {reason}")
        
        # Check if exists
        exists, doc_count = check_index_exists(ep, key, index_name)
        
        if not exists:
            print(f"  ℹ️  Index does not exist (already deleted?)")
            results['not_found'] += 1
            print()
            continue
        
        print(f"  📊 Document count: {doc_count:,}")
        results['total_docs_removed'] += doc_count
        
        # Delete
        print(f"  🗑️  Deleting...")
        success = delete_index(ep, key, index_name)
        
        if success:
            print(f"  ✅ Deleted successfully")
            results['deleted'] += 1
        else:
            print(f"  ❌ Failed to delete")
            results['errors'] += 1
        
        print()
    
    # Summary
    print(f"{'='*70}")
    print("📊 SUMMARY")
    print(f"{'='*70}")
    print(f"Total indexes processed: {len(REDUNDANT_INDEXES)}")
    print(f"  ✅ Deleted: {results['deleted']}")
    print(f"  ℹ️  Already gone: {results['not_found']}")
    print(f"  ❌ Errors: {results['errors']}")
    print()
    print(f"💾 Storage freed: ~{results['total_docs_removed']:,} documents removed")
    print()
    
    if results['deleted'] > 0:
        print(f"🎉 Successfully cleaned up {results['deleted']} redundant indexes!")
    
    return 0


if __name__ == '__main__':
    sys.exit(main())

