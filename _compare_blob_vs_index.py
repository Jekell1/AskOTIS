#!/usr/bin/env python3
"""
Compare source code files in blob storage vs indexed in new_code_chunks.
"""

import json
import requests
from azure.storage.blob import BlobServiceClient
from collections import defaultdict

with open('local.settings.json') as f:
    settings = json.load(f)['Values']

print("=" * 80)
print("COMPARING SOURCE FILES: Blob Storage vs new_code_chunks Index")
print("=" * 80)

# Get files from blob storage
print("\nStep 1: Getting files from blob storage...")

blob_service = BlobServiceClient(
    account_url=f"https://{settings['STORAGE_ACCOUNT']}.blob.core.windows.net",
    credential=settings['STORAGE_KEY']
)

container_name = 'cobol-source'
container = blob_service.get_container_client(container_name)

blob_files = []
for blob in container.list_blobs():
    if blob.name.upper().endswith(('.CBL', '.CPY')):
        blob_files.append(blob.name)

print(f"Found {len(blob_files)} source files (.CBL/.CPY) in blob storage")

# Categorize blob files
blob_cbl = [f for f in blob_files if f.upper().endswith('.CBL')]
blob_cpy = [f for f in blob_files if f.upper().endswith('.CPY')]

print(f"  - .CBL files: {len(blob_cbl)}")
print(f"  - .CPY files: {len(blob_cpy)}")

# Get files from new_code_chunks index
print("\nStep 2: Getting files from new_code_chunks index...")

url = f"{settings['SEARCH_ENDPOINT']}/indexes/new_code_chunks/docs/search?api-version=2025-08-01-preview"

body = {
    'search': '*',
    'facets': ['path,count:5000'],  # Get all unique paths
    'top': 0
}

r = requests.post(url, headers={'api-key': settings['SEARCH_KEY']}, json=body)
facets = r.json().get('@search.facets', {}).get('path', [])

indexed_files = [f['value'] for f in facets]
indexed_cbl = [f for f in indexed_files if f.upper().endswith('.CBL')]
indexed_cpy = [f for f in indexed_files if f.upper().endswith('.CPY')]

print(f"Found {len(indexed_files)} unique files in new_code_chunks index")
print(f"  - .CBL files: {len(indexed_cbl)}")
print(f"  - .CPY files: {len(indexed_cpy)}")

# Normalize paths for comparison (blob uses / or \, index might differ)
def normalize_path(path):
    """Normalize path separators and case for comparison."""
    return path.replace('/', '\\').upper()

blob_files_normalized = {normalize_path(f): f for f in blob_files}
indexed_files_normalized = {normalize_path(f): f for f in indexed_files}

# Find missing files
missing_from_index = set(blob_files_normalized.keys()) - set(indexed_files_normalized.keys())
only_in_index = set(indexed_files_normalized.keys()) - set(blob_files_normalized.keys())

print("\n" + "=" * 80)
print("COVERAGE ANALYSIS")
print("=" * 80)

coverage_pct = (len(blob_files) - len(missing_from_index)) / len(blob_files) * 100 if blob_files else 0

print(f"""
Source files in blob storage: {len(blob_files)}
Files indexed in new_code_chunks: {len(indexed_files)}

Coverage: {coverage_pct:.1f}% ({len(blob_files) - len(missing_from_index)}/{len(blob_files)} files)

Missing from index: {len(missing_from_index)} files
Only in index (not in blob): {len(only_in_index)} files
""")

# Analyze missing files by type
missing_cbl = [blob_files_normalized[f] for f in missing_from_index if f.endswith('.CBL')]
missing_cpy = [blob_files_normalized[f] for f in missing_from_index if f.endswith('.CPY')]

print("Missing files breakdown:")
print(f"  - .CBL files: {len(missing_cbl)}")
print(f"  - .CPY files: {len(missing_cpy)}")

if missing_from_index:
    print("\n" + "=" * 80)
    print(f"MISSING FILES (showing first 50 of {len(missing_from_index)})")
    print("=" * 80)
    
    for path in sorted(missing_from_index)[:50]:
        original_path = blob_files_normalized[path]
        print(f"  - {original_path}")
    
    if len(missing_from_index) > 50:
        print(f"\n  ... and {len(missing_from_index) - 50} more files")

# Check if LONPF2 is covered
lonpf2_in_blob = any('LONPF2.CBL' in f.upper() for f in blob_files)
lonpf2_in_index = any('LONPF2.CBL' in f.upper() for f in indexed_files)

print("\n" + "=" * 80)
print("LONPF2.CBL STATUS")
print("=" * 80)

print(f"In blob storage: {'✓ YES' if lonpf2_in_blob else '✗ NO'}")
print(f"In index: {'✓ YES' if lonpf2_in_index else '✗ NO'}")

if lonpf2_in_blob:
    lonpf2_blob_path = next(f for f in blob_files if 'LONPF2.CBL' in f.upper())
    print(f"Blob path: {lonpf2_blob_path}")

if lonpf2_in_index:
    lonpf2_index_path = next(f for f in indexed_files if 'LONPF2.CBL' in f.upper())
    print(f"Index path: {lonpf2_index_path}")

# Summary
print("\n" + "=" * 80)
print("SUMMARY")
print("=" * 80)

if coverage_pct >= 99:
    print("✅ EXCELLENT: Near-complete coverage (≥99%)")
elif coverage_pct >= 95:
    print("✅ GOOD: Most files covered (≥95%)")
elif coverage_pct >= 90:
    print("⚠️ FAIR: Some files missing (≥90%)")
else:
    print("❌ POOR: Many files missing (<90%)")

print(f"\nCoverage: {coverage_pct:.1f}%")
print(f"Missing: {len(missing_from_index)} files ({len(missing_from_index)/len(blob_files)*100:.1f}%)")
