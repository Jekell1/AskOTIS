"""Find all LPCAMU_SCN.CPY chunks including lines 1-25."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_code_chunks', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for chunks with program_id matching LPCAMU_SCN
prog_id = '3fd0260097dc80b59888b12e76ae834b21206c3a'

results = list(client.search(
    f'program_id:{prog_id}',
    filter=f"program_id eq '{prog_id}'",
    select='name,start_line,end_line,text',
    top=10
))

print(f'Found {len(results)} chunks for program_id {prog_id}\n')

# Sort by start_line
results.sort(key=lambda x: x.get('start_line', 999))

for r in results:
    name = r.get('name') or 'None'
    text = r.get('text', '')
    
    # Count menu options 1-7
    menu_opts = []
    for i in range(1, 8):
        if f'"{i}.' in text:
            menu_opts.append(i)
    
    print(f'{name:20} lines {r["start_line"]:3}-{r["end_line"]:3}  options: {menu_opts if menu_opts else "none"}')
