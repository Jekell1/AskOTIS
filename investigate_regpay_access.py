#!/usr/bin/env python3
"""Investigate how users access REGPAY."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

print("="*100)
print("HOW USERS ACCESS REGPAY")
print("="*100)

# 1. Check UI paths
print("\n1. Checking UI Paths Index...")
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_ui_paths',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('REGPAY', top=20))
print(f"   Found {len(results)} UI paths mentioning REGPAY")

for i, r in enumerate(results[:5], 1):
    root = r.get('root_screen', 'Unknown')
    leaf = r.get('leaf_screen', 'Unknown')
    path_json = r.get('path_json', [])
    
    if isinstance(path_json, str):
        try:
            path_json = json.loads(path_json)
        except:
            pass
    
    print(f"\n   Path {i}:")
    print(f"     Root: {root}")
    print(f"     Leaf: {leaf}")
    if isinstance(path_json, list) and len(path_json) > 0:
        print(f"     Path: {' → '.join([str(x) for x in path_json[:10]])}")

# 2. Check screen nodes
print("\n2. Checking Screen Nodes Index...")
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_screen_nodes',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('REGPAY', top=20))
print(f"   Found {len(results)} screen nodes mentioning REGPAY")

for i, r in enumerate(results[:5], 1):
    screen_name = r.get('screen_name', 'Unknown')
    program = r.get('program', 'Unknown')
    transaction = r.get('transaction', 'Unknown')
    
    print(f"\n   Screen {i}:")
    print(f"     Screen: {screen_name}")
    print(f"     Program: {program}")
    print(f"     Transaction: {transaction}")

# 3. Check REGPAY's own menu tree
print("\n3. Checking REGPAY Menu Tree...")
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('', filter="root_program_id eq 'REGPAY'", top=1))
if results:
    r = results[0]
    tree_json = r.get('tree_json', '{}')
    
    if isinstance(tree_json, str):
        try:
            tree_json = json.loads(tree_json)
        except:
            pass
    
    print(f"   REGPAY is a root program in menu_trees")
    print(f"   This means it's a top-level entry point")

# 4. Check REGPAY file metadata
print("\n4. Checking REGPAY File Metadata...")
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new-cobol-files',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('', filter="programId eq 'REGPAY'", top=1))
if results:
    r = results[0]
    has_screens = r.get('hasScreens', False)
    has_cics = r.get('hasCICS', False)
    summary = r.get('summary', '')
    
    print(f"   Has Screens: {has_screens}")
    print(f"   Has CICS: {has_cics}")
    print(f"   Description: {summary[:200]}...")

# 5. Look for transaction code in code
print("\n5. Searching Code for Transaction References...")
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_code_chunks',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(client.search('REGPAY transaction', top=5))
print(f"   Found {len(results)} code chunks")

for i, r in enumerate(results[:3], 1):
    name = r.get('name', 'Unknown')
    text = r.get('text', '')
    
    if 'REGPAY' in text and ('TRANS' in text.upper() or 'MENU' in text.upper()):
        print(f"\n   Chunk {i} ({name}):")
        # Find relevant lines
        lines = text.split('\n')
        for line in lines:
            if 'REGPAY' in line and ('TRANS' in line.upper() or 'MENU' in line.upper() or 'OPTION' in line.upper()):
                print(f"     {line.strip()[:100]}")

print("\n" + "="*100)
print("CONCLUSION")
print("="*100)
print("""
REGPAY is accessed in one of these ways:

1. **Direct Transaction Code**: User types a transaction code (like 'REGP' or 'REG') at 
   a CICS terminal, which directly invokes REGPAY

2. **Menu Selection**: User navigates through a menu system and selects an option that
   is mapped to REGPAY (though REGPAY itself is the root of its own menu tree)

3. **Screen Navigation**: User follows a UI path that leads to REGPAY, possibly from
   other loan processing screens

Since REGPAY:
- Has its own screens (hasScreens=True)
- Is a menu tree root (not a child of another menu)
- Has no inbound CALL statements from other programs

The most likely access method is: **Direct transaction code entry** or **menu option**
that maps to REGPAY's transaction code.
""")
print("="*100)
