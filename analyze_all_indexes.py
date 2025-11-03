"""List all indexes in Azure Search and analyze their usage."""
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

# Get all indexes
r = requests.get(f'{ep}/indexes?api-version={API}', headers={'api-key': key}, timeout=30)
if r.status_code != 200:
    print(f'Error: {r.status_code}')
    exit(1)

indexes = r.json().get('value', [])

print(f"{'Index Name':<40} {'Doc Count':>12} {'Status'}")
print("=" * 70)

# Known active indexes
active_new_indexes = {
    'new_cobol_copybook_usage',
    'new_cobol_paragraphs', 
    'new_cobol_data_items',
    'new_cobol_flow_edges_v2',
    'new_cobol_screen_nodes',
    'new_cobol_variable_usage',
    'new_cobol_program_flows',
    'new_cobol_ui_paths',
    'new_cobol_program_meta'
}

legacy_indexes = set()
unused_indexes = set()
new_indexes = set()

for idx in sorted(indexes, key=lambda x: x['name']):
    name = idx['name']
    
    # Get document count
    try:
        r2 = requests.get(f'{ep}/indexes/{name}/docs/$count?api-version={API}', 
                         headers={'api-key': key}, timeout=30)
        count = int(r2.text) if r2.status_code == 200 else 0
    except:
        count = 0
    
    # Categorize
    if name in active_new_indexes:
        status = "✅ ACTIVE (new)"
        new_indexes.add(name)
    elif name.startswith('new_cobol_'):
        status = "❓ UNUSED (new_*)"
        unused_indexes.add(name)
    elif name.startswith('cobol-') or name.startswith('cobol_'):
        status = "🔴 LEGACY"
        legacy_indexes.add(name)
    else:
        status = "❓ UNKNOWN"
        unused_indexes.add(name)
    
    print(f"{name:<40} {count:>12,} {status}")

print("\n" + "=" * 70)
print(f"\n📊 Summary:")
print(f"  Active 'new_cobol_*' indexes: {len(new_indexes)}")
print(f"  Unused 'new_cobol_*' indexes: {len([n for n in unused_indexes if n.startswith('new_cobol_')])}")
print(f"  Legacy 'cobol-*' indexes: {len(legacy_indexes)}")
print(f"  Other/Unknown indexes: {len([n for n in unused_indexes if not n.startswith('new_cobol_')])}")
print(f"  Total indexes: {len(indexes)}")

if unused_indexes:
    print(f"\n⚠️  Potentially Unused Indexes ({len(unused_indexes)}):")
    for name in sorted(unused_indexes):
        print(f"  - {name}")

if legacy_indexes:
    print(f"\n🔴 Legacy Indexes (consider archiving):")
    for name in sorted(legacy_indexes):
        # Get count
        try:
            r2 = requests.get(f'{ep}/indexes/{name}/docs/$count?api-version={API}', 
                             headers={'api-key': key}, timeout=30)
            count = int(r2.text) if r2.status_code == 200 else 0
            print(f"  - {name:<35} ({count:,} docs)")
        except:
            print(f"  - {name}")
