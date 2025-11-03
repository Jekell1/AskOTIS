from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

config = json.load(open('local.settings.json')).get('Values', {})
client = SearchClient(
    config['AZURE_SEARCH_ENDPOINT'], 
    'new_cobol_menu_trees', 
    AzureKeyCredential(config['AZURE_SEARCH_KEY'])
)

# Get documents with UI nodes (actual menus)
results = client.search(
    search_text='*', 
    filter='total_ui_nodes gt 0',
    order_by=['total_nodes desc'],
    top=1
)

doc = next(results, None)
if doc:
    print(json.dumps(doc, indent=2))
    
    # Pretty print the tree structure
    print("\n" + "="*80)
    print("TREE STRUCTURE DETAIL:")
    print("="*80)
    tree_data = json.loads(doc.get('tree_json', '{}'))
    print(json.dumps(tree_data, indent=2)[:2000])  # First 2000 chars
else:
    print('No menu trees with nodes found')
