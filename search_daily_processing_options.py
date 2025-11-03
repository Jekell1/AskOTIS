"""Search for all screens with Daily Processing options."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for screens with numbered daily processing options
search_terms = [
    'DAILY PROCESSING POSTING TRANSACTIONS TELLER',
    'DAILY PROCESSING 1. 2. 3. 4.',
    'DAILY PROCESSING TELLER POSTING LOAN',
]

all_screens = {}

for term in search_terms:
    results = list(client.search(
        term,
        select='screen_id,program_id,summary_text,label_literals_json',
        top=15
    ))
    
    for r in results:
        screen_id = r['screen_id']
        if screen_id not in all_screens:
            all_screens[screen_id] = r

print(f'Found {len(all_screens)} unique screens related to Daily Processing\n')
print('='*80)

# Parse and display each screen's menu options
for screen_id, r in sorted(all_screens.items())[:20]:
    label_json = r.get('label_literals_json', '[]')
    
    try:
        labels = json.loads(label_json)
        
        # Filter for menu-like items (numbered options, F-keys, menu titles)
        menu_items = []
        for lbl in labels:
            text = lbl.get('text', '').strip()
            # Look for numbered options or menu navigation
            if any(x in text.upper() for x in ['1.', '2.', '3.', '4.', '5.', '6.', '7.', '8.', '9.', 'F7', 'MENU', 'DAILY', 'PROCESSING', 'TELLER', 'POSTING']):
                if text not in menu_items:
                    menu_items.append(text)
        
        if menu_items and len(menu_items) >= 2:
            print(f'\nScreen: {screen_id[:40]}')
            print(f'Program: {r["program_id"][:40]}')
            print(f'Menu items ({len(menu_items)}):')
            for item in sorted(menu_items)[:20]:
                print(f'  • {item}')
            print('-'*80)
    except:
        pass
