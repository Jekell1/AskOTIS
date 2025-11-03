"""Preview what will be deleted without actually deleting.

This is a DRY RUN script - it will NOT delete anything.
Use this to verify what would be deleted before running delete_legacy_indexes.py
"""
import os, json, requests

API = '2025-08-01-preview'

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()
ep = (os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

legacy_indexes = [
    ('cobol-facts-v3', 'new_cobol_* indexes'),
    ('cobol-facts-v3l', 'new_cobol_* indexes'),
    ('cobol-symbols', 'new_cobol_symbol_refs (1.1M docs)'),
    ('cobol-xrefs', 'new_cobol_flow_edges_v2, new_cobol_calls, etc.')
]

print("=" * 80)
print("  LEGACY INDEX DELETION - DRY RUN (NO CHANGES)".center(80))
print("=" * 80)
print()

total_docs = 0
total_with_embeddings = 0

for idx_name, superseded_by in legacy_indexes:
    print(f"📦 {idx_name}")
    print(f"   Superseded by: {superseded_by}")
    
    try:
        # Get count
        r = requests.get(f'{ep}/indexes/{idx_name}/docs/$count?api-version={API}', 
                        headers={'api-key': key}, timeout=30)
        count = int(r.text) if r.status_code == 200 else 0
        total_docs += count
        print(f"   Documents: {count:,}")
        
        # Check for embeddings
        r2 = requests.post(f'{ep}/indexes/{idx_name}/docs/search?api-version={API}', 
                          headers={'api-key': key, 'Content-Type': 'application/json'},
                          json={'search':'*','top':0,'count':True,'filter':'has_vector eq true'}, 
                          timeout=30)
        
        if r2.status_code == 200:
            with_vec = r2.json().get('@odata.count', 0)
            total_with_embeddings += with_vec
            pct = 100 * with_vec / count if count > 0 else 0
            print(f"   Embeddings: {with_vec:,} ({pct:.1f}%)")
        
        # Get schema summary
        r3 = requests.get(f'{ep}/indexes/{idx_name}?api-version={API}', 
                         headers={'api-key': key}, timeout=30)
        if r3.status_code == 200:
            schema = r3.json()
            field_count = len(schema.get('fields', []))
            print(f"   Fields: {field_count}")
        
        print()
        
    except Exception as e:
        print(f"   ⚠️  Error: {e}")
        print()

print("=" * 80)
print("DELETION IMPACT SUMMARY")
print("=" * 80)
print(f"Indexes to delete: {len(legacy_indexes)}")
print(f"Documents to remove: {total_docs:,}")
print(f"Embedded documents: {total_with_embeddings:,}")
print()
print("💾 Storage savings:")
print(f"   ~{total_docs:,} documents")
print(f"   ~{total_with_embeddings * 3072 * 4 / 1024 / 1024:.1f} MB of vector data (estimated)")
print()
print("=" * 80)
print()
print("✅ This is a DRY RUN - no changes were made")
print()
print("To actually delete these indexes:")
print("  python delete_legacy_indexes.py")
print()
print("=" * 80)
