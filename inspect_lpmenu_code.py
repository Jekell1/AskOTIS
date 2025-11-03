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

# Get LPMENU code
print("="*80)
print("LPMENU CODE SAMPLE")
print("="*80)

results = client.search(
    search_text='LPMENU.CBL',
    select=['text', 'name', 'chunk_id'],
    top=5
)

for i, result in enumerate(results):
    name = result.get('name', '')
    if 'LPMENU.CBL' in name:
        print(f"\n--- Chunk {i+1}: {name} ---")
        code = result.get('text', '')
        # Show first 2000 characters
        print(code[:2000])
        print("\n" + "."*80)
        if len(code) > 2000:
            print("(showing first 2000 chars)")
