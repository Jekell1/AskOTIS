#!/usr/bin/env python3
"""Search for all LPMENU-related chunks."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load settings
cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(
    cfg['AZURE_SEARCH_ENDPOINT'],
    'new_code_chunks',
    AzureKeyCredential(cfg['AZURE_SEARCH_KEY'])
)

print("=" * 80)
print("SEARCH FOR ALL LPMENU CHUNKS")
print("=" * 80)

# Search by text
results = client.search('LPMENU', select='chunk_id,program_id,start_line,end_line,text', top=20)

docs = list(results)
print(f"\nFound {len(docs)} chunks containing 'LPMENU'")

for i, d in enumerate(docs, 1):
    print(f"\n{i}. Chunk ID: {d['chunk_id'][:16]}...")
    print(f"   Program ID: {d['program_id'][:16]}...")
    print(f"   Lines: {d['start_line']}-{d['end_line']}")
    if 'MASTER' in d['text']:
        print(f"   ✅ Contains 'MASTER'")
        print(f"   Preview: {d['text'][:200]}...")

print("\n" + "=" * 80)
