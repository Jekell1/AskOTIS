"""Verify Daily Processing menu screen has all options."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

screen_id = '3FD0260097DC80B59888B12E76AE834B21206C3A_1'
r = client.get_document(screen_id)

labels = json.loads(r['label_literals_json'])

print(f'Screen: {screen_id}')
print(f'Program: {r["program_id"]}')
print(f'Total label literals: {len(labels)}\n')

print('Label Literals:')
for i, lbl in enumerate(labels, 1):
    print(f'{i:2}. Line {lbl["line"]:2} Col {lbl["col"]:2}: {lbl["text"]}')
