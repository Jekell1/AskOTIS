"""Delete legacy Azure Search indexes that are superseded by new_ versions.

SAFE TO DELETE:
- cobol-facts-v3 (31 docs) → superseded by new_cobol_* indexes
- cobol-facts-v3l (31 docs) → superseded by new_cobol_* indexes  
- cobol-symbols (41,550 docs) → superseded by new_cobol_symbol_refs (1.1M docs)
- cobol-xrefs (63,405 docs) → superseded by multiple new_cobol_* indexes

Total to reclaim: 105,017 documents, 4 indexes

This script will:
1. List the indexes to be deleted
2. Show document counts
3. Ask for confirmation
4. Delete indexes one by one with status updates
"""
import os, json, requests, sys

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

# Indexes to delete
legacy_indexes = [
    ('cobol-facts-v3', 'Superseded by new_cobol_* indexes'),
    ('cobol-facts-v3l', 'Superseded by new_cobol_* indexes'),
    ('cobol-symbols', 'Superseded by new_cobol_symbol_refs (1.1M docs vs 41k)'),
    ('cobol-xrefs', 'Superseded by new_cobol_* indexes (flow_edges, calls, etc.)')
]

print("=" * 80)
print("  LEGACY INDEX DELETION SCRIPT".center(80))
print("=" * 80)
print()
print("The following indexes are SAFE TO DELETE:")
print()

total_docs = 0
for idx_name, reason in legacy_indexes:
    # Get count
    try:
        r = requests.get(f'{ep}/indexes/{idx_name}/docs/$count?api-version={API}', 
                        headers={'api-key': key}, timeout=30)
        count = int(r.text) if r.status_code == 200 else 0
        total_docs += count
        print(f"  📦 {idx_name:<30} {count:>10,} docs")
        print(f"     → {reason}")
        print()
    except Exception as e:
        print(f"  ⚠️  {idx_name:<30} (Error getting count: {e})")
        print()

print("─" * 80)
print(f"Total to reclaim: {total_docs:,} documents across {len(legacy_indexes)} indexes")
print("=" * 80)
print()
print("⚠️  WARNING: This action cannot be undone!")
print("All data in these indexes will be permanently deleted.")
print()

# Confirmation
response = input("Type 'DELETE' to proceed, or anything else to cancel: ")
print()

if response.strip().upper() != 'DELETE':
    print("❌ Deletion cancelled. No changes made.")
    sys.exit(0)

# Delete indexes
print("=" * 80)
print("Starting deletion process...")
print("=" * 80)
print()

deleted_count = 0
failed_count = 0

for idx_name, reason in legacy_indexes:
    print(f"Deleting: {idx_name}...", end=' ')
    sys.stdout.flush()
    
    try:
        r = requests.delete(
            f'{ep}/indexes/{idx_name}?api-version={API}',
            headers={'api-key': key},
            timeout=30
        )
        
        if r.status_code in (200, 204):
            print("✅ SUCCESS")
            deleted_count += 1
        elif r.status_code == 404:
            print("⚠️  NOT FOUND (already deleted?)")
            deleted_count += 1
        else:
            print(f"❌ FAILED: {r.status_code} - {r.text[:100]}")
            failed_count += 1
            
    except Exception as e:
        print(f"❌ ERROR: {e}")
        failed_count += 1

print()
print("=" * 80)
print("DELETION SUMMARY")
print("=" * 80)
print(f"✅ Successfully deleted: {deleted_count}/{len(legacy_indexes)} indexes")
if failed_count > 0:
    print(f"❌ Failed: {failed_count} indexes")
print()

if deleted_count == len(legacy_indexes):
    print("🎉 All legacy indexes successfully deleted!")
    print(f"   Reclaimed ~{total_docs:,} documents")
    print()
    print("Next steps:")
    print("  1. Verify with: python analyze_all_indexes.py")
    print("  2. Update any scripts that reference these indexes")
else:
    print("⚠️  Some deletions failed. Check error messages above.")

print("=" * 80)
