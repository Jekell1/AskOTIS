"""
Search for REGPAY business logic in code chunks
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv
import os

load_dotenv()

from otis_rag.config import Config
config = Config()

client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_code_chunks',
    credential=AzureKeyCredential(config.search_key)
)

# Search for the ENTRY-MODULE or MAX=3 logic
results = client.search(
    search_text='REGPAY MAX ENTER NEW MONIES TOTPAYMNTD TOTEXCPAYMNTD REGPYAMT',
    top=10,
    select=['program_id', 'name', 'scope', 'text']
)

print('='*80)
print('Searching for REGPAY field entry logic in code chunks')
print('='*80)

for i, r in enumerate(results, 1):
    print(f'\n--- Result {i} ---')
    print(f'Program: {r["program_id"]}')
    print(f'Name: {r["name"]}')
    print(f'Scope: {r["scope"]}')
    
    text = r.get('text', '')
    
    # Check for key phrases
    has_max = 'MAX' in text and '3' in text
    has_prompts = 'ENTER NEW MONIES' in text
    has_fields = any(field in text for field in ['TOTPAYMNTD', 'TOTEXCPAYMNTD', 'REGPYAMT'])
    
    if has_max:
        print('** Contains MAX=3 definition **')
    if has_prompts:
        print('** Contains field entry prompts **')
    if has_fields:
        print('** Contains actual data field names **')
    
    # Show snippet
    if has_max or has_prompts or has_fields:
        lines = text.split('\n')
        relevant_lines = []
        for j, line in enumerate(lines):
            if any(keyword in line for keyword in ['MAX', 'ENTER NEW', 'TOTPAYMNTD', 'TOTEXCPAYMNTD', 'REGPYAMT', 'SPEC-TABLE', 'CHFG', 'ELEMENT SPECIFICATIONS']):
                relevant_lines.append((j+1, line.strip()[:120]))
        
        if relevant_lines:
            print('  Relevant lines:')
            for line_num, line_text in relevant_lines[:15]:
                print(f'    Line {line_num}: {line_text}')

print('\n' + '='*80)
print('SUMMARY')
print('='*80)
print("""
If the code chunks contain:
- MAX = 3 definition
- ENTER NEW MONIES TOWARD PDTHRU
- ENTER NEW NON-CONTRACTUAL MONIES  
- ENTER NEW REGULAR PAYMENT AMT

Then the RAG system should be able to answer questions about the three specific
fields (not just showing all screen fields).

The key distinction is:
- Screen copybooks show ALL fields on the screen
- Business logic in ENTRY-MODULE shows the 3 MODIFIABLE fields (MAX=3)
""")
