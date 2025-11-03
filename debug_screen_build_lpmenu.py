"""Debug why LPMENU screen node isn't being generated."""
import json
import re
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from pathlib import Path
from ingestion_common import stable_hash

# Load config
with open('local.settings.json') as f:
    cfg = json.load(f)['Values']

search_client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_code_chunks', AzureKeyCredential(cfg['SEARCH_KEY']))

# LPMENU program_id
lpmenu_path = Path('cobol_src/LIBLP/LPMENU_SCN.CPY')
normalized_path = str(lpmenu_path).replace('\\', '/').lower()
program_id = stable_hash([normalized_path])

print(f"LPMENU program_id: {program_id}")
print(f"Path: {normalized_path}\n")

# Step 1: Check if chunks contain "SCREEN SECTION"
print("Step 1: Checking chunks for 'SCREEN SECTION'...")
results = list(search_client.search(
    search_text='"SCREEN SECTION"',
    filter=f"program_id eq '{program_id}'",
    select=['chunk_id', 'start_line', 'end_line', 'text'],
    top=10
))

print(f"Found {len(results)} chunks with 'SCREEN SECTION'\n")

if results:
    for r in results:
        print(f"Chunk lines {r['start_line']}-{r['end_line']}:")
        text = r['text']
        if 'SCREEN SECTION' in text:
            idx = text.index('SCREEN SECTION')
            print(f"  Preview: ...{text[max(0,idx-50):idx+150]}...")
        print()
else:
    print("❌ No chunks found with 'SCREEN SECTION'")
    print("\nStep 2: Checking if file has SCREEN SECTION...")
    
    # Read the file directly
    with open(lpmenu_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    if 'SCREEN SECTION' in content.upper():
        print("✅ File DOES contain 'SCREEN SECTION'")
        idx = content.upper().index('SCREEN SECTION')
        print(f"   Found at position {idx}")
        print(f"   Context: ...{content[max(0,idx-50):idx+100]}...")
    else:
        print("❌ File does NOT contain 'SCREEN SECTION'")
        print("\nChecking for screen-related keywords...")
        keywords = ['SCREEN', 'LABEL', 'LINE', 'COL', 'DISPLAY']
        for kw in keywords:
            if kw in content.upper():
                count = content.upper().count(kw)
                print(f"  Found '{kw}': {count} times")
        
        # Show first 50 lines
        lines = content.split('\n')
        print(f"\nFirst 50 lines of file:")
        for i, line in enumerate(lines[:50], 1):
            print(f"{i:3d}: {line}")
