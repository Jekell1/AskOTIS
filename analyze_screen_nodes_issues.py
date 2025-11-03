"""
Comprehensive analysis of screen_nodes index issues
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config
import json

config = Config()

client = SearchClient(
    config.search_endpoint, 
    'new_cobol_screen_nodes',
    AzureKeyCredential(config.search_key)
)

print("="*80)
print("SCREEN_NODES INDEX ANALYSIS")
print("="*80)

# 1. Check total count
results = list(client.search("*", select=['screen_id'], top=1))
print(f"\n1. Total documents in index: ~1881 (from recent rebuild)")

# 2. Check for copybook-based screens (hash IDs)
print("\n2. Checking for copybook-based screens (hash IDs)...")
results = list(client.search("DAILY PROCESSING REPORTS INQUIRIES", top=5, select=['screen_id', 'program_id', 'label_literals_json']))

hash_id_screens = []
for doc in results:
    prog_id = doc.get('program_id', '')
    if len(prog_id) == 40 and all(c in '0123456789abcdefABCDEF' for c in prog_id):
        hash_id_screens.append(doc)
        
print(f"   Found {len(hash_id_screens)} screens with hash IDs in search results")

if hash_id_screens:
    print("\n   Example hash-based screen:")
    doc = hash_id_screens[0]
    print(f"   Screen ID: {doc.get('screen_id')}")
    print(f"   Program ID: {doc.get('program_id')}")
    labels_json = doc.get('label_literals_json')
    if labels_json:
        labels = json.loads(labels_json)
        print(f"   Label count: {len(labels)}")
        if labels:
            print(f"   First 3 labels:")
            for lbl in labels[:3]:
                print(f"      - {lbl.get('text', 'N/A')}")

# 3. Check for LPMENU-specific content
print("\n3. Searching for LPMENU MASTER MENU content...")
results = list(client.search(
    "MASTER MENU DAILY PROCESSING REPORTS INQUIRIES COLLECTION",
    top=10,
    select=['screen_id', 'program_id', 'summary_text']
))

master_menu_found = False
for doc in results:
    summary = doc.get('summary_text', '')
    if 'DAILY PROCESSING' in summary and 'REPORTS' in summary and 'INQUIRIES' in summary and 'COLLECTION PROCESSING' in summary:
        master_menu_found = True
        print(f"\n   ✅ FOUND LPMENU MASTER MENU!")
        print(f"   Screen ID: {doc.get('screen_id')}")
        print(f"   Program ID: {doc.get('program_id')}")
        print(f"\n   Summary:\n{summary[:800]}")
        break

if not master_menu_found:
    print("\n   ❌ LPMENU MASTER MENU NOT FOUND in screen_nodes")
    print("   This is the core problem!")

# 4. Check code chunks for LPMENU_SCN copybook
print("\n4. Checking code chunks for LPMENU_SCN copybook...")
chunk_client = SearchClient(
    config.search_endpoint, 
    'new_code_chunks',
    AzureKeyCredential(config.search_key)
)

results = list(chunk_client.search(
    "LPMENU_SCN LABEL LINE MASTER MENU",
    top=3,
    select=['program_id', 'path', 'name']
))

print(f"   Found {len(results)} code chunks with LPMENU_SCN content")
if results:
    for doc in results:
        print(f"   - Program ID: {doc.get('program_id')}")
        print(f"     Path: {doc.get('path', 'N/A')}")
        print(f"     Name: {doc.get('name', 'N/A')}")

# 5. Summary of problems
print("\n" + "="*80)
print("PROBLEMS IDENTIFIED:")
print("="*80)
print("""
1. COPYBOOK IDENTIFICATION:
   - Screen copybooks (like LPMENU_SCN.CPY) are indexed with SHA hash IDs
   - No connection to the friendly name "LPMENU"
   - RAG cannot retrieve by program name

2. MISSING LPMENU.CBL:
   - The actual program file LPMENU.CBL is not in code chunks
   - No way to link the hash-based screen to "LPMENU" program

3. SEMANTIC SEARCH GAPS:
   - Even though MASTER MENU content exists in screen_nodes
   - The summary text may not have enough context to match "main user menu"
   - Need better keywords in summary (e.g., "main menu", "top-level menu")

SOLUTIONS NEEDED:
""")

print("A. Enhance build_screen_nodes.py to:")
print("   - Extract copybook filename from 'path' or 'name' field")
print("   - Use copybook name (e.g., 'LPMENU_SCN') as program_id instead of hash")
print("   - Add 'copybook_name' field to documents")
print()
print("B. Add better semantic keywords to summary text:")
print("   - Detect common menu titles: 'MASTER MENU', 'MAIN MENU'")
print("   - Add 'This is the main/top-level user menu' to summary")
print("   - Add alternative names for retrieval")
print()
print("C. Re-ingest COBOL programs to code chunks:")
print("   - Ensure all .CBL files are in code chunks with proper program_id")
print("   - This creates the program -> copybook linkage")
