"""Search broadly for menu trees to find the main menu."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config
import json

config = Config()
client = SearchClient(config.search_endpoint, 'new_cobol_menu_trees', AzureKeyCredential(config.search_key))

print("Searching for common main menu program names...")
print("="*80)

# Try various possible main menu names
possible_names = [
    'PGMENU', 'MAINMENU', 'MAIN', 'UPMENU', 'MASTER', 'MMENU',
    'OTOS', 'OTIS', 'MENU', 'PRIMARY', 'START', 'ENTRY'
]

for name in possible_names:
    results = list(client.search(name, top=3))
    if results:
        print(f"\n✓ Found {len(results)} results for '{name}':")
        for r in results:
            root_id = r.get('root_program_id', 'UNKNOWN')
            nodes = r.get('total_nodes', 0)
            ui_nodes = r.get('total_ui_nodes', 0)
            print(f"  - {root_id}: {nodes} nodes, {ui_nodes} UI screens")

print("\n" + "="*80)
print("Getting sample of menu trees (first 20)...")
print("="*80 + "\n")

# Get a sample of all menu trees
results = list(client.search('*', top=20))
print(f"Total results returned: {len(results)}\n")

# Group by naming patterns
by_pattern = {}
for r in results:
    root_id = r.get('root_program_id', 'UNKNOWN')
    # Categorize by pattern
    if 'MENU' in root_id:
        pattern = 'MENU'
    elif 'LP' in root_id or 'DL' in root_id or 'AD' in root_id:
        pattern = 'Module prefixed (LP/DL/AD)'
    else:
        pattern = 'Other'
    
    if pattern not in by_pattern:
        by_pattern[pattern] = []
    by_pattern[pattern].append(root_id)

print("Menu trees by pattern:")
for pattern, programs in sorted(by_pattern.items()):
    print(f"\n{pattern} ({len(programs)}):")
    for prog in sorted(programs)[:10]:
        print(f"  - {prog}")
    if len(programs) > 10:
        print(f"  ... and {len(programs) - 10} more")
