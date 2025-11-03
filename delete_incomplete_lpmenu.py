"""Delete incomplete LPMENU screen _2."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

# Delete EA66581C142E6BA08A83D2DC773BC990DB403C3C_2 (incomplete screen with only 4 options)
screen_id_to_delete = 'EA66581C142E6BA08A83D2DC773BC990DB403C3C_2'

print(f'Deleting screen: {screen_id_to_delete}')
result = client.delete_documents(documents=[{'screen_id': screen_id_to_delete}])
print(f'Deleted: {len(result)} documents')
print('Done!')
