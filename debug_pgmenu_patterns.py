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

# Get PGMENU code
results = client.search(
    search_text='PGMENU.CBL',
    select=['text', 'name'],
    top=50
)

code_parts = []
for result in results:
    name = result.get('name', '')
    if 'PGMENU' in name.upper():
        content = result.get('text', '')
        code_parts.append(content)

full_code = '\n'.join(code_parts)

# Look for menu patterns
print("="*80)
print("PGMENU CODE SAMPLE - Looking for menu patterns")
print("="*80)

# Pattern 1: Look for VALUE clauses with numbers
value_pattern = r'VALUE\s+["\'](\d+)[.\s\-]+([^"\']+)["\']'
matches = list(re.finditer(value_pattern, full_code, re.IGNORECASE))
print(f"\n✓ Found {len(matches)} VALUE clause matches")
for i, match in enumerate(matches[:10]):
    print(f"\n  Match {i+1}:")
    print(f"    Full match: {match.group(0)}")
    print(f"    Number: {match.group(1)}")
    print(f"    Text: {match.group(2)}")

# Pattern 2: Look for DISPLAY statements
display_pattern = r'DISPLAY\s+["\'](\d+)[.\s\-]+([^"\']+)["\']'
matches = list(re.finditer(display_pattern, full_code, re.IGNORECASE))
print(f"\n\n✓ Found {len(matches)} DISPLAY statement matches")
for i, match in enumerate(matches[:10]):
    print(f"\n  Match {i+1}:")
    print(f"    Full match: {match.group(0)}")
    print(f"    Number: {match.group(1)}")
    print(f"    Text: {match.group(2)}")

# Show sample of code to understand structure
print("\n\n" + "="*80)
print("CODE SAMPLE (first 3000 chars)")
print("="*80)
print(full_code[:3000])
