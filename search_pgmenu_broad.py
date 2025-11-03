"""
Search for PGMENU in any index to find the source
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()

# Try code_new index
print("Searching new_code_chunks for PGMENU...\n")
client = SearchClient(
    config.search_endpoint, 
    'new_code_chunks',
    AzureKeyCredential(config.search_key)
)

results = list(client.search("PGMENU", top=5))
print(f"Found {len(results)} results in new_code_chunks")

if results:
    doc = results[0]
    print(f"\nFirst result keys: {list(doc.keys())}")
    print(f"Program ID: {doc.get('program_id', 'N/A')}")
    
    # Show a snippet of text content
    for key in ['text', 'raw_span_text', 'span_text']:
        if key in doc:
            content = doc.get(key, '')
            if content:
                print(f"\n📄 {key} (first 1000 chars):")
                print(content[:1000])
                break

# Also try the legacy code index
print("\n" + "="*80)
print("\nSearching code-chunks for PGMENU...\n")
client2 = SearchClient(
    config.search_endpoint, 
    'code-chunks',
    AzureKeyCredential(config.search_key)
)

results2 = list(client2.search("PGMENU", top=5))
print(f"Found {len(results2)} results in code-chunks")

if results2:
    doc = results2[0]
    print(f"\nFirst result keys: {list(doc.keys())}")
    for key in ['text', 'content', 'code']:
        if key in doc:
            content = doc.get(key, '')
            if content and len(content) > 100:
                print(f"\n📄 {key} (first 1500 chars):")
                print(content[:1500])
                break
