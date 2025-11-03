"""Verify all current indexes are integrated into RAG system."""
import json
import requests
from collections import defaultdict
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT'].rstrip('/')
key = settings['SEARCH_KEY']

print("🔍 RAG INDEX INTEGRATION CHECK")
print("=" * 70)

# Get actual indexes from Azure
url = f"{endpoint}/indexes?api-version=2025-08-01-preview"
headers = {'api-key': key}
response = requests.get(url, headers=headers)

actual_indexes = set()
if response.status_code == 200:
    data = response.json()
    actual_indexes = {idx['name'] for idx in data.get('value', [])}
    print(f"\n✅ Found {len(actual_indexes)} indexes in Azure Search\n")
else:
    print(f"❌ Error fetching indexes: {response.status_code}")
    exit(1)

# Indexes configured in RAG (from config.py)
configured_indexes = {
    'code': 'new_code_chunks',
    'files': 'new-cobol-files',
    'programs': 'new_cobol_program_meta',
    'paragraphs': 'new_cobol_paragraphs',
    'data_items': 'new_cobol_data_items',
    'copybooks': 'new_cobol_copybook_meta',
    'variables': 'new_cobol_variable_usage',
    'copybook_usage': 'new_cobol_copybook_usage',
    'flows': 'new_cobol_program_flows',
    'flow_edges': 'new_cobol_flow_edges_v2',
    'ui_paths': 'new_cobol_ui_paths',
    'screen_nodes': 'new_cobol_screen_nodes',
    'help_fields': 'help_fields',
}

# Routing profiles (from router.py)
route_profiles = {
    "program": ["code", "programs", "flows"],
    "data": ["data_items", "variables", "copybooks", "copybook_usage"],
    "ui": ["screen_nodes", "ui_paths", "help_fields"],
    "transaction": ["flows", "flow_edges", "programs"],
    "flow": ["flows", "flow_edges", "paragraphs"],
    "complexity": ["programs", "flows", "paragraphs", "flow_edges"],
    "files": ["files", "code", "programs"]
}

print("=" * 70)
print("CONFIGURED INDEXES (config.py)")
print("=" * 70)

for key_name, index_name in sorted(configured_indexes.items()):
    exists = index_name in actual_indexes
    status = "✅ EXISTS" if exists else "❌ MISSING"
    print(f"  {key_name:<20} -> {index_name:<35} {status}")

print()
print("=" * 70)
print("INDEXES IN ACTUAL USE (router.py profiles)")
print("=" * 70)

# Count how often each index key is referenced in routing
index_usage = defaultdict(int)
for profile_name, index_keys in route_profiles.items():
    for key in index_keys:
        index_usage[key] += 1

print()
for key_name in sorted(index_usage.keys()):
    count = index_usage[key_name]
    index_name = configured_indexes.get(key_name, "UNKNOWN")
    exists = index_name in actual_indexes if index_name != "UNKNOWN" else False
    status = "✅" if exists else "❌"
    print(f"  {status} {key_name:<20} (used in {count} profiles) -> {index_name}")

print()
print("=" * 70)
print("EXISTING INDEXES NOT IN RAG")
print("=" * 70)

configured_index_names = set(configured_indexes.values())
unused = actual_indexes - configured_index_names

if unused:
    print()
    for idx in sorted(unused):
        print(f"  ⚠️  {idx}")
    print(f"\nTotal: {len(unused)} indexes exist but are not configured in RAG")
else:
    print("\n  ✅ All existing indexes are configured!")

print()
print("=" * 70)
print("SUMMARY")
print("=" * 70)
print(f"  Actual indexes in Azure:     {len(actual_indexes)}")
print(f"  Configured in RAG:           {len(configured_indexes)}")
print(f"  Used in routing profiles:    {len(index_usage)}")
print(f"  Unused (exist but not in RAG): {len(unused)}")
print()

# Check for broken references
broken = []
for key_name, index_name in configured_indexes.items():
    if index_name not in actual_indexes:
        broken.append((key_name, index_name))

if broken:
    print("❌ BROKEN REFERENCES (configured but don't exist):")
    for key_name, index_name in broken:
        print(f"  • {key_name} -> {index_name}")
    print()
else:
    print("✅ All configured indexes exist in Azure!")
    print()

print("=" * 70)
print("INTEGRATION STATUS")
print("=" * 70)

all_good = len(broken) == 0 and len(unused) == 0
if all_good:
    print("\n🎉 ALL INDEXES ARE PROPERLY INTEGRATED!")
    print(f"   • {len(actual_indexes)} indexes exist in Azure")
    print(f"   • {len(configured_indexes)} indexes configured in RAG")
    print(f"   • All are accessible through routing profiles")
    print()
else:
    print(f"\n⚠️  ISSUES FOUND:")
    if broken:
        print(f"   • {len(broken)} broken reference(s) in config")
    if unused:
        print(f"   • {len(unused)} unused index(es) in Azure")
    print()
