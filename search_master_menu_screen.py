#!/usr/bin/env python3
"""Search for MASTER MENU in screen_nodes."""

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
print("SEARCHING FOR MASTER MENU IN SCREEN_NODES")
print("=" * 80)

# Search for MASTER MENU
results = client.search(
    'MASTER MENU',
    select='screen_id,program_id,summary_text',
    top=10
)

found = False
for i, r in enumerate(results, 1):
    found = True
    print(f"\n{i}. Screen ID: {r['screen_id']}")
    print(f"   Program ID: {r['program_id']}")
    print(f"   Summary: {r['summary_text'][:300]}...")

if not found:
    print("\n❌ No screens found containing 'MASTER MENU'")
    print("\nSearching for LPMENU screens...")
    
    # Search for LPMENU
    results = client.search(
        'LPMENU',
        select='screen_id,program_id,summary_text',
        top=10
    )
    
    for i, r in enumerate(results, 1):
        print(f"\n{i}. Screen ID: {r['screen_id']}")
        print(f"   Program ID: {r['program_id']}")
        print(f"   Summary: {r['summary_text'][:300]}...")

print("\n" + "=" * 80)
