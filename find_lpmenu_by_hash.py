"""
Find the actual LPMENU_SCN screen in screen_nodes by searching the hash ID from code chunks
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()

# First get the hash ID for LPMENU_SCN from code chunks
chunk_client = SearchClient(
    config.search_endpoint, 
    'new_code_chunks',
    AzureKeyCredential(config.search_key)
)

print("Step 1: Finding LPMENU_SCN copybook hash ID in code chunks...")
print("="*80)

results = list(chunk_client.search(
    "LPMENU_SCN",
    top=20,
    select=['program_id', 'path', 'name', 'text']
))

lpmenu_hash = None
for doc in results:
    path = doc.get('path', '')
    name = doc.get('name', '')
    if 'LPMENU_SCN' in path or 'LPMENU_SCN' in name:
        lpmenu_hash = doc.get('program_id')
        print(f"\n✅ Found LPMENU_SCN copybook!")
        print(f"   Hash ID: {lpmenu_hash}")
        print(f"   Path: {path}")
        print(f"   Name: {name}")
        
        # Check if it has the menu content
        text = doc.get('text', '')
        if 'DAILY PROCESSING' in text and 'MASTER MENU' in text:
            print(f"   ✅ Contains MASTER MENU content!")
        break

if not lpmenu_hash:
    print("\n❌ LPMENU_SCN not found in code chunks!")
    exit(1)

# Now search for that hash in screen_nodes
print("\n\nStep 2: Searching for this hash ID in screen_nodes...")
print("="*80)

screen_client = SearchClient(
    config.search_endpoint, 
    'new_cobol_screen_nodes',
    AzureKeyCredential(config.search_key)
)

results = list(screen_client.search(
    "*",
    filter=f"program_id eq '{lpmenu_hash}'",
    top=10
))

if results:
    print(f"\n✅ Found {len(results)} screen(s) for LPMENU_SCN hash!")
    for doc in results:
        print(f"\n   Screen ID: {doc.get('screen_id')}")
        print(f"   Program ID: {doc.get('program_id')}")
        summary = doc.get('summary_text', '')
        print(f"\n   Summary:\n{summary}")
        print("="*80)
else:
    print(f"\n❌ NO SCREENS found for hash {lpmenu_hash}")
    print("   This means build_screen_nodes.py didn't extract it!")
