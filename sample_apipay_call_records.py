#!/usr/bin/env python3
"""Check what text content is in APIPAY call records."""

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

# Get a few APIPAY records with vectors
results = search_client.search(
    search_text='*',
    filter="caller_program eq 'APIPAY.CBL' and has_vector eq true",
    select=['call_id', 'caller_program', 'callee_program', 'reference_type', 
            'category', 'snippet', 'reference_description', 'has_vector'],
    top=5
)

print("Sample APIPAY call records WITH embeddings:\n")
print("="*80)

for i, record in enumerate(results, 1):
    print(f"\n{i}. Call ID: {record.get('call_id')}")
    print(f"   Caller: {record.get('caller_program')}")
    print(f"   Callee: {record.get('callee_program')}")
    print(f"   Type: {record.get('reference_type')}")
    print(f"   Category: {record.get('category')}")
    print(f"   Has Vector: {record.get('has_vector')}")
    print(f"   Description: {record.get('reference_description', 'N/A')}")
    snippet = record.get('snippet', '')
    if snippet:
        snippet_preview = snippet[:200] + '...' if len(snippet) > 200 else snippet
        print(f"   Snippet: {snippet_preview}")
    print()

print("="*80)
print("\nThis is the text that was embedded for semantic search.")
print("The embedding combines: caller, callee, type, category, description, and snippet.")
