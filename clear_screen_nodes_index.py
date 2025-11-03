#!/usr/bin/env python3
"""Clear all documents from new_cobol_screen_nodes index."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load settings
cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(
    cfg['AZURE_SEARCH_ENDPOINT'],
    'new_cobol_screen_nodes',
    AzureKeyCredential(cfg['AZURE_SEARCH_KEY'])
)

print("=" * 80)
print("CLEAR SCREEN_NODES INDEX")
print("=" * 80)

# Get total count
total = client.get_document_count()
print(f"\nTotal documents before: {total}")

# Confirm deletion
confirm = input("\nType 'DELETE' to confirm clearing all screen nodes: ")
if confirm != "DELETE":
    print("Aborted.")
    exit(0)

# Delete in rounds (Azure Search has 100K skip limit)
round_num = 1
while True:
    # Fetch documents to delete
    results = client.search("*", select="screen_id", top=1000)
    docs_to_delete = [{"screen_id": doc["screen_id"]} for doc in results]
    
    if not docs_to_delete:
        break
    
    # Delete batch
    result = client.delete_documents(docs_to_delete)
    print(f"Round {round_num}: Deleted {len(docs_to_delete)} documents")
    round_num += 1

# Final count
final = client.get_document_count()
print(f"\nTotal documents after: {final}")
print("\n✅ Screen nodes index cleared")
print("=" * 80)
