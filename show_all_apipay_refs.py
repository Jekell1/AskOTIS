#!/usr/bin/env python3
"""Direct query for APIPAY calls - bypassing RAG semantic search."""

import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from collections import defaultdict

with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_calls',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("=" * 80)
print("APIPAY - Complete External References")
print("=" * 80)
print()

# Get ALL APIPAY references (not just those with embeddings)
results = search_client.search(
    search_text='APIPAY',  # Keyword search
    select=['caller_program', 'callee_program', 'reference_type', 'category', 'has_vector'],
    top=300  # Get more than the 217 we expect
)

all_refs = list(results)
print(f"Total references from APIPAY: {len(all_refs)}\n")

# Group by type
by_type = defaultdict(list)
for ref in all_refs:
    if ref.get('caller_program') == 'APIPAY':  # Only caller, not callee
        ref_type = ref.get('reference_type', 'Unknown')
        callee = ref.get('callee_program', 'Unknown')
        by_type[ref_type].append(callee)

# Display by type
for ref_type in sorted(by_type.keys(), key=lambda x: (x is None, x or '')):
    callees = by_type[ref_type]
    unique_callees = sorted(set(callees))
    type_label = ref_type if ref_type else '(unknown)'
    
    print(f"{type_label}: {len(unique_callees)} unique")
    for callee in unique_callees:
        print(f"  - {callee}")
    print()

# Summary
total_unique = len(set(ref.get('callee_program') for ref in all_refs if ref.get('caller_program') == 'APIPAY'))
with_embeddings = sum(1 for ref in all_refs if ref.get('caller_program') == 'APIPAY' and ref.get('has_vector'))
print("=" * 80)
print(f"Summary: {total_unique} unique programs/copybooks referenced")
print(f"Embedding coverage: {with_embeddings}/{len([r for r in all_refs if r.get('caller_program') == 'APIPAY'])} ({with_embeddings/len([r for r in all_refs if r.get('caller_program') == 'APIPAY'])*100:.1f}%)")
print("=" * 80)
