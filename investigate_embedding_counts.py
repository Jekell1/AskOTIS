#!/usr/bin/env python3
"""Check what happened with the embedding counts."""

import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_calls',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("🔍 Investigating new_cobol_calls index counts...\n")

# Total records
total = search_client.search('*', include_total_count=True).get_count()
print(f"Total records in index: {total:,}")

# Records marked as enhanced_data=true
enhanced = search_client.search('*', filter='enhanced_data eq true', include_total_count=True).get_count()
print(f"Records with enhanced_data=true: {enhanced:,}")

# Records with has_vector=true
with_vectors = search_client.search('*', filter='has_vector eq true', include_total_count=True).get_count()
print(f"Records with has_vector=true: {with_vectors:,}")

# Records with has_vector=false
without_vectors = search_client.search('*', filter='has_vector eq false', include_total_count=True).get_count()
print(f"Records with has_vector=false: {without_vectors:,}")

# Records with enhanced_data but no vector
enhanced_no_vector = search_client.search('*', filter='enhanced_data eq true and has_vector eq false', include_total_count=True).get_count()
print(f"Records with enhanced_data=true and has_vector=false: {enhanced_no_vector:,}")

# Records without enhanced_data
not_enhanced = search_client.search('*', filter='enhanced_data eq false or enhanced_data eq null', include_total_count=True).get_count()
print(f"Records with enhanced_data=false or null: {not_enhanced:,}")

print(f"\n📊 Analysis:")
print(f"  Total = {total:,}")
print(f"  Enhanced ({enhanced:,}) + Not Enhanced ({not_enhanced:,}) = {enhanced + not_enhanced:,}")
print(f"  With Vectors ({with_vectors:,}) + Without Vectors ({without_vectors:,}) = {with_vectors + without_vectors:,}")
print(f"\n  🎯 Target for embeddings: {enhanced_no_vector:,} (enhanced but no vector)")
