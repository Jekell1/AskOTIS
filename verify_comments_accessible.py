"""
Check comment accessibility by reading actual indexed content
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

from otis_rag.config import Config
config = Config()

print("="*80)
print("VERIFYING COMMENT CONTENT IN INDEXES")
print("="*80)

code_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_code_chunks',
    credential=AzureKeyCredential(config.search_key)
)

# Get any chunk from REGPAY
print("\n1. Get actual REGPAY chunks:")
print("-" * 80)

results = list(code_client.search(
    search_text='REGPAY',
    top=3
))

print(f"Found {len(results)} results")

if results:
    # Show all fields available
    first_result = results[0]
    print(f"\nAvailable fields in result:")
    for key in first_result.keys():
        print(f"  - {key}")
    
    print(f"\n" + "="*80)
    print("First chunk content:")
    print("="*80)
    
    # Try different field names
    possible_fields = ['content', 'chunk_text', 'text', 'code', 'source']
    
    for field in possible_fields:
        if field in first_result:
            content = first_result.get(field, '')
            print(f"\nField '{field}' found:")
            print(content[:800])
            break
    else:
        print("\nNo standard content field found. Showing all data:")
        for key, value in first_result.items():
            if isinstance(value, str) and len(value) > 50:
                print(f"\n{key}:")
                print(value[:800])

print("\n" + "="*80)
print("2. Ask RAG directly about comments:")
print("="*80)

from otis_rag import OTISRAG

rag = OTISRAG()

question = """
In the REGPAY program, what do the comments say about the ELEMENT SPECIFICATIONS 
and the SPEC-TABLE? Show me the exact comment text from the code.
"""

print(f"Question: {question}")
print("\nAsking RAG...")

response = rag.ask(question, verbose=False)

print("\nRAG Response:")
print("-" * 80)
print(response)

print("\n" + "="*80)
print("3. Test another program with known comments:")
print("="*80)

question2 = """
What does the IDENTIFICATION DIVISION say in the REGPAY program? 
Show me the AUTHOR, DATE-WRITTEN, and REMARKS if present.
"""

print(f"Question: {question2}")
print("\nAsking RAG...")

response2 = rag.ask(question2, verbose=False)

print("\nRAG Response:")
print("-" * 80)
print(response2)
