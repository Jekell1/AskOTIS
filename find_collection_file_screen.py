"""
Find the actual screen name for Collection File Maintenance Menu
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()

# Search screen_nodes
client = SearchClient(
    config.search_endpoint, 
    'new_cobol_screen_nodes',
    AzureKeyCredential(config.search_key)
)

# Search for collection file maintenance screens
search_terms = [
    "collection file",
    "CFMENU",
    "file maintenance"
]

print("Searching for Collection File Maintenance screens...\n")

for term in search_terms:
    print(f"\n🔍 Searching for: '{term}'")
    print("=" * 80)
    
    results = list(client.search(
        term,
        top=5,
        select=['screen_id', 'program_id', 'summary_text']
    ))
    
    if results:
        for doc in results:
            print(f"\nScreen: {doc.get('screen_id')}")
            print(f"Program: {doc.get('program_id')}")
            summary = doc.get('summary_text', '')
            if len(summary) > 200:
                summary = summary[:200] + "..."
            print(f"Summary: {summary}")
    else:
        print("No results found")
