"""
Find the OTIS startup/initial screen
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv
import json

load_dotenv()

from otis_rag.config import Config
config = Config()

print("="*80)
print("FINDING OTIS STARTUP SCREEN")
print("="*80)

# Common names for startup/login screens in mainframe apps
startup_candidates = [
    'OTIS', 'OTOS', 'LOGON', 'LOGIN', 'SIGNON', 'SIGNIN',
    'MAIN', 'START', 'INIT', 'MENU', 'WELCOME', 'ENTRY',
    'LOGUID', 'USERID', 'GTFORM'
]

print("\n1. Checking for startup screen programs:")
print("-" * 80)

# Check files index for likely startup screens
files_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new-cobol-files',
    credential=AzureKeyCredential(config.search_key)
)

for candidate in ['OTIS', 'OTOS', 'LOGON', 'LOGUID', 'GTFORM', 'MENU', 'START']:
    results = list(files_client.search(
        search_text=candidate,
        filter=f"program_id eq '{candidate}'",
        top=1,
        select=['program_id', 'program_name', 'description', 'file_path']
    ))
    
    if results:
        r = results[0]
        print(f"\n✓ {candidate}:")
        print(f"  Name: {r.get('program_name', 'N/A')}")
        print(f"  Description: {r.get('description', 'N/A')[:100]}")
        print(f"  Path: {r.get('file_path', 'N/A')}")

print("\n" + "="*80)
print("2. Looking for LOGUID (login/user ID screen):")
print("="*80)

# LOGUID appeared multiple times in the menu trees as a security check
menu_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config.search_key)
)

try:
    loguid_doc = files_client.search(
        search_text='LOGUID',
        filter="program_id eq 'LOGUID'",
        top=1,
        select=['program_id', 'program_name', 'description', 'file_path']
    )
    
    for doc in loguid_doc:
        print(f"\nProgram: {doc.get('program_id')}")
        print(f"Name: {doc.get('program_name', 'N/A')}")
        print(f"Description: {doc.get('description', 'N/A')}")
        print(f"Path: {doc.get('file_path', 'N/A')}")
except:
    pass

print("\n" + "="*80)
print("3. Looking for GTFORM (Get Form - likely the entry point):")
print("="*80)

try:
    gtform_results = list(files_client.search(
        search_text='GTFORM',
        filter="program_id eq 'GTFORM'",
        top=1,
        select=['program_id', 'program_name', 'description', 'file_path']
    ))
    
    if gtform_results:
        doc = gtform_results[0]
        print(f"\nProgram: {doc.get('program_id')}")
        print(f"Name: {doc.get('program_name', 'N/A')}")
        print(f"Description: {doc.get('description', 'N/A')}")
        print(f"Path: {doc.get('file_path', 'N/A')}")
except:
    pass

print("\n" + "="*80)
print("4. Searching screen_nodes for 'main menu' or 'welcome':")
print("="*80)

screen_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_screen_nodes',
    credential=AzureKeyCredential(config.search_key)
)

results = list(screen_client.search(
    search_text='main menu welcome startup login',
    top=10,
    select=['program_id', 'screen_name', 'context']
))

if results:
    for i, r in enumerate(results[:5], 1):
        print(f"\n{i}. {r.get('program_id', 'N/A')} - {r.get('screen_name', 'N/A')}")
        context = r.get('context', '')[:150]
        print(f"   {context}...")

print("\n" + "="*80)
print("5. Checking UI paths for entry points:")
print("="*80)

ui_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_ui_paths',
    credential=AzureKeyCredential(config.search_key)
)

# Look for paths that start with likely entry programs
results = list(ui_client.search(
    search_text='*',
    top=10,
    select=['path_id', 'root_program', 'path_length', 'full_path']
))

print("\nSample UI path roots (entry points):")
root_programs = set()
for r in results[:20]:
    root = r.get('root_program', 'N/A')
    root_programs.add(root)

for root in sorted(root_programs)[:10]:
    print(f"  - {root}")

print("\n" + "="*80)
print("💡 HYPOTHESIS:")
print("="*80)
print("""
Most likely scenario:

1. GTFORM or LOGUID is the initial screen
   - GTFORM = "Get Form" (presents the initial menu selection screen)
   - LOGUID = "Login User ID" (authentication screen)

2. User either:
   - Sees a master menu screen listing transaction codes
   - Directly types a transaction code (WI, SP, GL, etc.)
   
3. Transaction code invokes the corresponding *MENU program

4. From there, the user navigates the menu tree for that functional area

Let me search the code to confirm...
""")
