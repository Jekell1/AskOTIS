"""
Find the OTIS startup screen - simpler approach
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

from otis_rag.config import Config
config = Config()

print("="*80)
print("FINDING OTIS STARTUP/ENTRY SCREEN")
print("="*80)

# Search in code chunks for startup/login logic
code_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_code_chunks',
    credential=AzureKeyCredential(config.search_key)
)

print("\n1. Searching for 'welcome' or 'main menu' screens:")
print("-" * 80)

results = list(code_client.search(
    search_text='OTIS welcome main menu startup',
    top=5,
    select=['file_name', 'content']
))

for i, r in enumerate(results, 1):
    print(f"\n{i}. File: {r.get('file_name', 'N/A')}")
    content = r.get('content', '')[:300]
    print(f"   {content}...")

print("\n" + "="*80)
print("2. Searching for 'GTFORM' (Get Form - likely entry point):")
print("="*80)

results = list(code_client.search(
    search_text='GTFORM',
    top=3,
    select=['file_name', 'content']
))

for i, r in enumerate(results, 1):
    print(f"\n{i}. File: {r.get('file_name', 'N/A')}")
    content = r.get('content', '')[:400]
    print(f"   {content}...")

print("\n" + "="*80)
print("3. Searching for 'LOGUID' (Login User ID):")
print("="*80)

results = list(code_client.search(
    search_text='LOGUID IDENTIFICATION DIVISION PROGRAM-ID',
    top=3,
    select=['file_name', 'content']
))

for i, r in enumerate(results, 1):
    print(f"\n{i}. File: {r.get('file_name', 'N/A')}")
    content = r.get('content', '')[:400]
    print(f"   {content}...")

print("\n" + "="*80)
print("4. Looking at what the RAG knows about OTIS entry:")
print("="*80)

print("""
Let me check what calls these common entry programs...
GTFORM appeared in many menu trees - it seems to be the form handler
LOGUID appeared as a security check - might be the login screen
""")
