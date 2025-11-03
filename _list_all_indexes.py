#!/usr/bin/env python3
"""List all indexes and identify which are 'new' vs legacy."""
import requests, json

vals = json.load(open('local.settings.json')).get('Values', {})
ep = vals.get('AZURE_SEARCH_ENDPOINT', '').rstrip('/')
key = vals.get('AZURE_SEARCH_KEY', '')

# Get all indexes
url = f"{ep}/indexes?api-version=2023-11-01"
r = requests.get(url, headers={'api-key': key})

if r.status_code != 200:
    print(f"Error: {r.status_code}")
    exit(1)

indexes = r.json().get('value', [])

print("=" * 80)
print("ALL INDEXES IN SEARCH SERVICE")
print("=" * 80)

new_indexes = []
old_indexes = []

for idx in indexes:
    name = idx.get('name', '')
    fields = idx.get('fields', [])
    doc_count_url = f"{ep}/indexes/{name}/docs/$count?api-version=2023-11-01"
    count_r = requests.get(doc_count_url, headers={'api-key': key})
    doc_count = int(count_r.text) if count_r.status_code == 200 else 0
    
    # Check for vector fields
    vector_fields = [f['name'] for f in fields if f.get('type') == 'Collection(Edm.Single)']
    
    info = {
        'name': name,
        'doc_count': doc_count,
        'field_count': len(fields),
        'vector_fields': len(vector_fields),
        'vector_field_names': vector_fields[:3]  # First 3
    }
    
    if name.startswith('new_cobol') or name.startswith('new-cobol'):
        new_indexes.append(info)
    else:
        old_indexes.append(info)

print("\nNEW INDEXES (new_cobol-* / new-cobol-*):")
print(f"{'Index Name':<45} {'Docs':>10} {'Fields':>8} {'Vectors':>8}")
print("-" * 80)
for idx in sorted(new_indexes, key=lambda x: x['name']):
    print(f"{idx['name']:<45} {idx['doc_count']:>10,} {idx['field_count']:>8} {idx['vector_fields']:>8}")

print("\nLEGACY/OTHER INDEXES:")
if old_indexes:
    print(f"{'Index Name':<45} {'Docs':>10} {'Fields':>8} {'Vectors':>8}")
    print("-" * 80)
    for idx in sorted(old_indexes, key=lambda x: x['name']):
        print(f"{idx['name']:<45} {idx['doc_count']:>10,} {idx['field_count']:>8} {idx['vector_fields']:>8}")
else:
    print("  (none)")

print(f"\n{'=' * 80}")
print("SUMMARY")
print("=" * 80)
print(f"New indexes: {len(new_indexes)}")
print(f"Legacy/Other indexes: {len(old_indexes)}")
print(f"Total indexes: {len(indexes)}")

if old_indexes:
    print(f"\n{'=' * 80}")
    print("LEGACY INDEX DETAILS")
    print("=" * 80)
    for idx in sorted(old_indexes, key=lambda x: x['name']):
        print(f"\n{idx['name']}:")
        print(f"  Documents: {idx['doc_count']:,}")
        print(f"  Total fields: {idx['field_count']}")
        print(f"  Vector fields: {idx['vector_fields']} - {', '.join(idx['vector_field_names']) if idx['vector_field_names'] else 'none'}")
        
        # Check if there's a corresponding "new" version
        potential_new = 'new_cobol_' + idx['name'].replace('cobol-', '').replace('cobol_', '')
        if any(n['name'] == potential_new for n in new_indexes):
            print(f"  ⚠️  Possible duplicate - 'new' version exists: {potential_new}")
