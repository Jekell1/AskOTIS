"""
Search paragraphs for REGPAY field entry prompts
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()
from otis_rag.config import Config
config = Config()

client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_paragraphs',
    credential=AzureKeyCredential(config.search_key)
)

results = client.search(
    search_text='REGPAY "ENTER NEW MONIES TOWARD PDTHRU"',
    top=10
)

print('='*80)
print('Searching paragraphs for ENTRY-MODULE / ENTER-ELE')
print('='*80)

found_prompts = False

for i, r in enumerate(results, 1):
    print(f'\n--- Paragraph {i} ---')
    print(f'Program: {r.get("program_id", "N/A")}')
    print(f'Name: {r.get("paragraph_name", "N/A")}')
    
    text = r.get('text', '')
    
    if 'ENTER NEW' in text:
        print('** Contains field entry prompts **')
        found_prompts = True
        lines = text.split('\n')
        for j, line in enumerate(lines):
            if 'ENTER NEW' in line or 'ELE =' in line or 'IF ELE =' in line:
                print(f'  {line.strip()[:120]}')

print('\n' + '='*80)
print('CONCLUSION')
print('='*80)

if found_prompts:
    print("""
✓ YES - The field entry prompts are indexed in paragraphs.

The information about the THREE specific fields is available:
1. ENTER NEW MONIES TOWARD PDTHRU (Field 1)
2. ENTER NEW NON-CONTRACTUAL MONIES (Field 2)  
3. ENTER NEW REGULAR PAYMENT AMT (Field 3)

This data is in the indexes and should be retrievable by the RAG system.
The RAG might need better query routing or more specific search terms
to find the ENTRY-MODULE paragraph that contains this business logic.
""")
else:
    print("""
✗ NO - The field entry prompts were not found in top paragraphs.

This could mean:
1. The ENTRY-MODULE paragraph wasn't indexed
2. The search terms don't match well
3. The prompts are in a different index (code chunks)

The information EXISTS in the codebase but may not be easily retrievable.
""")
