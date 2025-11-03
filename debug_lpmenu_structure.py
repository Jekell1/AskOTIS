from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json
import re

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
results = client.search(
    search_text='LPMENU.CBL',
    select=['text', 'name'],
    top=50
)

code_parts = []
for result in results:
    name = result.get('name', '')
    if 'LPMENU' in name.upper():
        content = result.get('text', '')
        code_parts.append(content)

full_code = '\n'.join(code_parts)

print("="*80)
print("LPMENU CODE - Looking for menu-related sections")
print("="*80)

# Look for lines that might contain menu options
lines = full_code.split('\n')
menu_sections = []

for i, line in enumerate(lines):
    # Look for patterns that might indicate menu options
    if any(keyword in line.upper() for keyword in ['DISPLAY', 'MENU', 'OPTION', 'SELECT', 'CHOICE', 'VALUE']):
        # Get context (3 lines before and after)
        start = max(0, i-3)
        end = min(len(lines), i+4)
        context = '\n'.join(lines[start:end])
        menu_sections.append((i, context))

print(f"\nFound {len(menu_sections)} potential menu sections\n")

# Show first 10
for line_num, context in menu_sections[:10]:
    print(f"\n--- Line {line_num} ---")
    print(context)
    print("-" * 40)
