"""Find Daily Processing submenu screens."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for Daily Processing menu - broader search
results = list(client.search(
    'DAILY PROCESSING POSTING TRANSACTIONS',
    select='screen_id,summary_text,label_literals_json',
    top=10
))

print(f'Found {len(results)} Daily Processing screens\n')
print('='*80)

for i, r in enumerate(results, 1):
    screen_id = r['screen_id']
    summary = r.get('summary_text', '')
    label_json = r.get('label_literals_json', '[]')
    
    print(f'\n{i}. Screen: {screen_id}')
    print(f'   @search.score: {r.get("@search.score", 0):.2f}')
    
    # Parse labels
    import json as js
    try:
        labels = js.loads(label_json)
        print(f'   Labels ({len(labels)} items):')
        
        # Filter out duplicates and sort
        seen = set()
        unique_labels = []
        for lbl in labels:
            text = lbl.get('text', '').strip()
            if text and text not in seen:
                seen.add(text)
                unique_labels.append(text)
        
        for text in sorted(unique_labels):
            print(f'     • {text}')
    except:
        print(f'   Labels: {label_json[:200]}...')
    
    print('='*80)
