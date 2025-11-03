from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

with open('local.settings.json', 'r', encoding='utf-8') as f:
    settings = json.load(f)
    search_key = settings['Values']['AZURE_SEARCH_KEY']
    search_endpoint = settings['Values']['AZURE_SEARCH_ENDPOINT']

client = SearchClient(
    endpoint=search_endpoint,
    index_name='new_cobol_copybook_meta',
    credential=AzureKeyCredential(search_key)
)

print("="*80)
print("AVAILABLE COPYBOOKS")
print("="*80)

results = list(client.search(
    search_text='*',
    select=['copybook_name', 'include_count', 'summary'],
    top=30
))

print(f"\nTotal copybooks found: {len(results)}\n")

for i, r in enumerate(results[:20], 1):
    name = r.get('copybook_name') or 'Unknown'
    count = r.get('include_count') or 0
    summary = r.get('summary') or ''
    print(f"{i:2}. {name:30} (used {count:3}x)")
    if summary and len(summary) < 100:
        print(f"    {summary}")
