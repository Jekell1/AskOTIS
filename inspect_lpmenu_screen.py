from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    search_key = settings['Values']['AZURE_SEARCH_KEY']
    search_endpoint = settings['Values']['AZURE_SEARCH_ENDPOINT']

client = SearchClient(
    endpoint=search_endpoint,
    index_name='new_code_chunks',
    credential=AzureKeyCredential(search_key)
)

print("="*80)
print("SEARCHING FOR LPMENU SCREEN DEFINITION")
print("="*80)

# Search for screen copybook
for search_term in ['LPMENU_SCN', 'LPMENU_DEF', 'LPMENU_EVA']:
    print(f"\n\nSearching for: {search_term}")
    print("-"*80)
    results = client.search(
        search_text=search_term,
        select=['text', 'name', 'chunk_id'],
        top=3
    )
    
    found = False
    for result in results:
        name = result.get('name', '')
        if search_term in name:
            found = True
            print(f"\n✓ Found: {name}")
            code = result.get('text', '')
            print(code[:2000])
            print("\n" + "."*80)
            break
    
    if not found:
        print(f"  ⚠️ No chunks found for {search_term}")
