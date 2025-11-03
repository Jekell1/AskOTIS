"""Get full details of Daily Processing menu screen."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

screen_id = '3FD0260097DC80B59888B12E76AE834B21206C3A_1'
r = client.get_document(screen_id)

print(f'Screen: {r["screen_id"]}')
print(f'Program: {r["program_id"]}\n')

print('Label Literals:')
labels = json.loads(r['label_literals_json'])
for lbl in labels:
    print(f'  Line {lbl["line"]:2} Col {lbl["col"]:2}: {lbl["text"]}')

print(f'\n\nRaw COBOL (first 2000 chars):')
print(r['raw_span_text'][:2000])
