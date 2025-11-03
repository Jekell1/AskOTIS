"""Direct search for all LPCAMU_SCN.CPY chunks."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_code_chunks', AzureKeyCredential(cfg['SEARCH_KEY']))

results = list(client.search(
    '*',
    filter="name eq 'LPCAMU_SCN.CPY'",
    select='name,start_line,end_line,path',
    top=20
))

print(f'Found {len(results)} chunks with name=LPCAMU_SCN.CPY\n')

if results:
    results.sort(key=lambda x: x['start_line'])
    for r in results:
        print(f'lines {r["start_line"]:3}-{r["end_line"]:3}  path: {r.get("path", "N/A")}')
else:
    print('No chunks found! File may not have been ingested as a standalone copybook.')
    print('\nThe chunks we found earlier were from programs that COPY this file.')
