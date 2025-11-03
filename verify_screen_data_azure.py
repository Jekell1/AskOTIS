#!/usr/bin/env python3
"""Verify screen_nodes data in Azure Search has the LABEL text."""

import os
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

endpoint = os.getenv("AZURE_SEARCH_ENDPOINT")
key = os.getenv("AZURE_SEARCH_KEY")
index_name = "new_cobol_screen_nodes"

client = SearchClient(
    endpoint=endpoint,
    index_name=index_name,
    credential=AzureKeyCredential(key)
)

# Search for PGMENU screens
print("Searching for PGMENU screens...")
results = client.search(
    search_text="PGMENU menu",
    select=["screen_id", "program_id", "summary_text"],
    top=5
)

found = 0
for result in results:
    found += 1
    print(f"\n{'='*70}")
    print(f"Screen: {result['screen_id']}")
    print(f"Program: {result['program_id']}")
    print(f"\nSummary ({len(result.get('summary_text', ''))} chars):")
    summary = result.get('summary_text', '')
    print(summary[:500])
    
    # Check if LABELS text is present
    has_labels = "LABELS:" in summary
    print(f"\n✓ Has LABELS text: {has_labels}")

print(f"\n{'='*70}")
print(f"Total results: {found}")
