from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json') as f:
    creds = json.load(f)['Values']

client = SearchClient(
    creds['AZURE_SEARCH_ENDPOINT'],
    'new_code_chunks',
    AzureKeyCredential(creds['AZURE_SEARCH_KEY'])
)

# Test a few copybooks that had "No field definitions found"
test_cases = [
    'LN1099-EVA',  # First one that failed
    'LNLIST-EVA',  # Another EVA file
    'LPINFO-EVA',  # Another EVA file
    'LPFSSM',      # One without suffix
    'LPLSL1RN',    # RN file
]

def get_copybook_content(copybook_name: str) -> str:
    """Retrieve copybook content."""
    search_variants = [
        copybook_name,
        copybook_name.replace('-', '_'),
        copybook_name.replace('_', '-'),
    ]
    
    all_chunks = []
    for variant in search_variants:
        results = client.search(
            search_text=variant,
            select=['name', 'text', 'start_line', 'end_line'],
            top=50
        )
        
        for result in results:
            name = result.get('name', '')
            if name and '.CPY' in name.upper():
                name_normalized = name.upper().replace('_', '').replace('-', '').replace('.CPY', '')
                search_normalized = variant.upper().replace('_', '').replace('-', '')
                
                if search_normalized in name_normalized:
                    all_chunks.append({
                        'text': result.get('text', ''),
                        'start_line': result.get('start_line', 0),
                        'end_line': result.get('end_line', 0)
                    })
        
        if all_chunks:
            break
    
    if not all_chunks:
        return None
    
    all_chunks.sort(key=lambda x: x['start_line'])
    return '\n'.join([c['text'] for c in all_chunks])

for copybook_name in test_cases:
    print(f"\n{'='*80}")
    print(f"Analyzing: {copybook_name}")
    print('='*80)
    
    content = get_copybook_content(copybook_name)
    
    if not content:
        print("  ⚠️ No content found in index")
        continue
    
    print(f"\nContent length: {len(content)} chars")
    print(f"\nFirst 500 chars:")
    print(content[:500])
    
    # Check for field definitions
    lines = content.split('\n')
    field_lines = []
    for i, line in enumerate(lines[:50], 1):  # Check first 50 lines
        # Look for COBOL field definitions (level numbers)
        stripped = line.strip()
        if stripped and len(stripped) > 2:
            # Check if line starts with 01-49 or 66, 77, 88
            first_word = stripped.split()[0] if stripped.split() else ''
            if first_word.isdigit() and len(first_word) == 2:
                level = int(first_word)
                if 1 <= level <= 49 or level in [66, 77, 88]:
                    field_lines.append(f"  Line {i}: {line[:70]}")
    
    if field_lines:
        print(f"\nFound {len(field_lines)} potential field definition lines:")
        for line in field_lines[:5]:  # Show first 5
            print(line)
    else:
        print("\n  ⚠️ No field definition lines found (no level numbers 01-49)")
        print("\nContent appears to be:")
        # Analyze what type of content it is
        if 'WHEN VDU-' in content or 'MOVE' in content and 'TO VDU-' in content:
            print("    → Screen/form evaluation logic (WHEN/MOVE statements)")
        elif 'WORKING-STORAGE' in content:
            print("    → Working storage section")
        elif 'EXEC SQL' in content:
            print("    → SQL statements")
        elif 'COPY' in content:
            print("    → COPY statements only")
        else:
            print("    → Unknown/mixed content")
