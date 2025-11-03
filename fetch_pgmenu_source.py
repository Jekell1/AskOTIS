"""
Fetch PGMENU source code from code chunks to see screen format
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

# Search for PGMENU code chunks
print("Searching for PGMENU code chunks...\n")
print("=" * 80)

results = list(client.search(
    "PGMENU SCREEN SECTION",
    filter="program_id eq 'PGMENU'",
    top=2
))

if results:
    print(f"Found {len(results)} PGMENU code chunks:\n")
    for doc in results:
        print(f"\nChunk ID: {doc.get('chunk_id', 'N/A')}")
        print(f"Available keys: {list(doc.keys())[:10]}")
        
        # Try to get content with various possible field names
        for key in ['text', 'code', 'source', 'span_text', 'content']:
            if key in doc:
                content = doc.get(key, '')
                if content and 'SCREEN' in str(content).upper():
                    print(f"\n📄 Found content in field '{key}' (first 1000 chars):")
                    print(str(content)[:1000])
                    print("\n" + "=" * 80)
                    break
else:
    print("❌ No PGMENU code chunks found!")
