"""
Search for PGMENU DISPLAY statements to find menu text
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()

client = SearchClient(
    config.search_endpoint, 
    'new_code_chunks',
    AzureKeyCredential(config.search_key)
)

# Search for PGMENU with DISPLAY statements
print("Searching for PGMENU code with DISPLAY statements...\n")
print("=" * 80)

results = list(client.search(
    "PGMENU DISPLAY",
    filter="program_id eq 'PGMENU'",
    top=10
))

print(f"Found {len(results)} PGMENU code chunks\n")

for i, doc in enumerate(results, 1):
    print(f"\n{'='*80}")
    print(f"Chunk {i}:")
    print(f"{'='*80}")
    
    # Get all text-like fields
    for key in doc.keys():
        if 'text' in key.lower() or 'code' in key.lower() or 'span' in key.lower():
            content = str(doc.get(key, ''))
            if len(content) > 100:  # Only show substantial content
                print(f"\n📄 Field: {key}")
                # Show more context for DISPLAY statements
                if 'DISPLAY' in content.upper():
                    print(content[:2000])
                    break
