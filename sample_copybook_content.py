from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

with open('local.settings.json', 'r', encoding='utf-8') as f:
    settings = json.load(f)
    search_key = settings['Values']['AZURE_SEARCH_KEY']
    search_endpoint = settings['Values']['AZURE_SEARCH_ENDPOINT']

code_client = SearchClient(
    endpoint=search_endpoint,
    index_name='new_code_chunks',
    credential=AzureKeyCredential(search_key)
)

print("="*80)
print("SAMPLE COPYBOOK CONTENT FROM CODE_CHUNKS")
print("="*80)

# Try a few copybooks
test_names = ['LNSCN3-SCN', 'CLMAIN-SCN', 'LNAPBD-DEF', 'LNCOST-DEF']

for cpyname in test_names:
    results = list(code_client.search(
        search_text=cpyname,
        select=['name', 'text'],
        top=3
    ))
    
    for r in results:
        name = r.get('name', '')
        if cpyname.upper() in name.upper() and '.CPY' in name.upper():
            print(f"\n\n📄 {name}")
            print("-"*80)
            content = r.get('text', '')
            
            # Show first 2000 chars
            print(content[:2000])
            if len(content) > 2000:
                print(f"\n... [Total length: {len(content)} chars]")
            
            # Look for field definitions (level numbers)
            lines = content.split('\n')
            field_lines = [l for l in lines if l.strip() and l.strip()[0:2].strip().isdigit()]
            print(f"\n📊 Field definition lines: {len(field_lines)}")
            break
    break  # Just show one example
