"""
Search for MASTER MENU in screen_nodes
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()

client = SearchClient(
    config.search_endpoint, 
    'new_cobol_screen_nodes',
    AzureKeyCredential(config.search_key)
)

# Search for MASTER MENU
print("Searching for MASTER MENU...\n")
print("=" * 80)

results = list(client.search(
    "MASTER MENU DAILY PROCESSING",
    top=5
))

if results:
    print(f"Found {len(results)} results:\n")
    for doc in results:
        screen_id = doc.get('screen_id', 'N/A')
        program_id = doc.get('program_id', 'N/A')
        summary = doc.get('summary_text', '')
        
        print(f"\nScreen: {screen_id}")
        print(f"Program: {program_id}")
        print(f"\nSummary:\n{summary}")
        print("=" * 80)
else:
    print("❌ No MASTER MENU found!")
