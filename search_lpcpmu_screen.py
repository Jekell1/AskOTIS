from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

config = json.load(open('local.settings.json')).get('Values', {})
client = SearchClient(
    config['AZURE_SEARCH_ENDPOINT'], 
    'new_cobol_screen_nodes', 
    AzureKeyCredential(config['AZURE_SEARCH_KEY'])
)

# Search for LPCPMU screen
print("Searching for LPCPMU in screen_nodes...")
results = client.search(
    search_text='LPCPMU',
    top=5
)

count = 0
for doc in results:
    count += 1
    print(f"\n{'='*80}")
    print(f"Result {count}:")
    print(f"{'='*80}")
    print(f"screen_name: {doc.get('screen_name')}")
    print(f"program_id: {doc.get('program_id')}")
    print(f"field_count: {doc.get('field_count')}")
    print(f"\nLabel literals:")
    labels = json.loads(doc.get('label_literals_json', '[]'))
    for label in labels[:10]:  # First 10
        print(f"  Line {label.get('line')}, Col {label.get('col')}: {label.get('text')}")
    
    print(f"\nSummary text (first 500 chars):")
    print(doc.get('summary_text', '')[:500])

if count == 0:
    print("No LPCPMU screens found in new_cobol_screen_nodes!")
    print("\nLet's check what screens ARE in the index...")
    
    # Get a sample of screen names
    all_results = client.search(search_text='*', top=10)
    print("\nSample screen names in index:")
    for i, doc in enumerate(all_results, 1):
        print(f"  {i}. {doc.get('screen_name')} (program: {doc.get('program_id')})")
