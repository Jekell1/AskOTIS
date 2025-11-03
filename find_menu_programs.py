from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    search_key = settings['Values']['AZURE_SEARCH_KEY']
    search_endpoint = settings['Values']['AZURE_SEARCH_ENDPOINT']

meta_client = SearchClient(
    endpoint=search_endpoint,
    index_name='new_cobol_program_meta',
    credential=AzureKeyCredential(search_key)
)

print("="*80)
print("SEARCHING FOR MENU PROGRAMS")
print("="*80)

# Search for programs with "MENU" in name or description
results = meta_client.search(
    search_text='MENU',
    select=['program_id', 'program_summary', 'outgoing_count'],
    top=30
)

print("\nMenu-related programs:")
for r in results:
    pid = r.get('program_id', '')
    summary = r.get('program_summary', '')
    calls = r.get('outgoing_count', 0)
    
    # Only show if program_id contains MENU
    if 'MENU' in pid.upper():
        print(f"\n  {pid}")
        if summary:
            print(f"    Summary: {summary[:200]}")
        print(f"    Calls: {calls} programs")
