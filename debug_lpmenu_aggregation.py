"""Debug which chunks are aggregated for LPMENU."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json
from collections import defaultdict

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_code_chunks', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for LPMENU_SCN.CPY chunks
results = list(client.search(
    'LPMENU_SCN.CPY', 
    search_mode='all', 
    select='name,program_id,start_line,end_line,text',
    top=15
))

# Group by program_id
prog_chunks = defaultdict(list)
for r in results:
    prog_id = (r.get('program_id') or '').replace('\\', '/').upper()
    if prog_id:
        prog_chunks[prog_id].append(r)

print(f'Found {len(results)} chunks grouped into {len(prog_chunks)} programs\n')

for prog_id, chunks in prog_chunks.items():
    print(f'Program: {prog_id}')
    print(f'Chunks: {len(chunks)}')
    
    # Sort by start_line
    chunks.sort(key=lambda x: x.get('start_line', 0))
    
    for ch in chunks:
        name = ch.get('name') or 'None'
        text = ch.get('text', '')
        has_label = 'LABEL LINE' in text.upper()
        num_labels = text.upper().count('LABEL LINE')
        print(f'  {name:20} lines {ch["start_line"]:3}-{ch["end_line"]:3}  has_label={has_label}  labels={num_labels}')
    
    print()
