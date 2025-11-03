from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

config = json.load(open('local.settings.json')).get('Values', {})
client = SearchClient(
    config['AZURE_SEARCH_ENDPOINT'], 
    'new_cobol_screen_nodes', 
    AzureKeyCredential(config['AZURE_SEARCH_KEY'])
)

# Get a sample screen node document
results = client.search(
    search_text='*',
    top=1
)

doc = next(results, None)
if doc:
    print("SCREEN NODE SAMPLE:")
    print("="*80)
    print(json.dumps(doc, indent=2))
else:
    print('No screen nodes found')
