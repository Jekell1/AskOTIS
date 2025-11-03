"""
Find menu containing "Result Code file maint" option
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

# Search for result code
search_terms = [
    "result code",
    "file maint",
    "7. Result"
]

print("Searching for 'Result Code file maint' menu option...\n")

for term in search_terms:
    print(f"\n🔍 Searching for: '{term}'")
    print("=" * 80)
    
    results = list(client.search(
        term,
        top=5,
        select=['screen_id', 'program_id', 'summary_text', 'display_literals_json']
    ))
    
    if results:
        for doc in results:
            print(f"\nScreen: {doc.get('screen_id')}")
            print(f"Program: {doc.get('program_id')}")
            
            # Show full summary
            summary = doc.get('summary_text', '')
            print(f"Summary:\n{summary}")
            
            # Check if display_literals has the menu
            display_lit = doc.get('display_literals_json')
            if display_lit and ('result' in display_lit.lower() or 'file maint' in display_lit.lower()):
                print(f"\n📋 Display Literals (excerpt):")
                if len(display_lit) > 500:
                    print(display_lit[:500] + "...")
                else:
                    print(display_lit)
            
            print("-" * 40)
    else:
        print("No results found")
