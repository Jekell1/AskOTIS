#!/usr/bin/env python3
"""Find PGMENU screen node."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import os, json

vals = json.load(open('local.settings.json','r')).get('Values',{})
for k,v in vals.items():
    os.environ[k] = str(v)

ep = os.getenv('AZURE_SEARCH_ENDPOINT').rstrip('/')
key = os.getenv('AZURE_SEARCH_KEY')

client = SearchClient(
    endpoint=ep,
    index_name='new_cobol_screen_nodes',
    credential=AzureKeyCredential(key)
)

# Search for PGMENU
results = list(client.search(
    search_text='PGMENU',
    select=['screen_id','program_id','summary_text'],
    top=10
))

print(f"Found {len(results)} screens with 'PGMENU'")
for r in results:
    print(f"\n{'='*70}")
    print(f"Screen: {r['screen_id']}")
    print(f"Program: {r.get('program_id', 'N/A')}")
    summary = r.get('summary_text', '')
    print(f"Summary ({len(summary)} chars):")
    print(summary[:500])
