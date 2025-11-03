"""Show details of the 10 screens with menu-related LABEL text"""
import os, json, requests

with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('SEARCH_ENDPOINT', os.getenv('AZURE_SEARCH_ENDPOINT')).rstrip('/')
KEY = os.getenv('SEARCH_KEY', os.getenv('AZURE_SEARCH_KEY'))
API_VERSION = '2024-07-01'

HEADERS = {'Content-Type': 'application/json', 'api-key': KEY}

print('=' * 80)
print('SCREENS WITH MENU-RELATED LABEL TEXT')
print('=' * 80)
print()

# Search for screens with 'MENU' or 'OPTION' in LABEL text
url = f'{ENDPOINT}/indexes/new_cobol_screen_nodes/docs/search?api-version={API_VERSION}'
body = {
    'search': 'MENU OPTION',
    'searchFields': 'label_literals_json',
    'top': 15,
    'select': 'screen_id,program_id,screen_name,label_literals_json,field_count'
}

r = requests.post(url, headers=HEADERS, json=body)

if r.status_code != 200:
    print(f'ERROR: {r.status_code} - {r.text[:500]}')
else:
    results = r.json().get('value', [])
    
    print(f'Found {len(results)} screens with MENU/OPTION in LABEL text\n')
    
    for idx, result in enumerate(results, 1):
        print(f'{idx}. Screen: {result["screen_name"]}')
        print(f'   Program ID: {result["program_id"]}')
        print(f'   Fields: {result["field_count"]}')
        
        labels = json.loads(result.get('label_literals_json', '[]'))
        menu_labels = [l for l in labels if 'MENU' in l.get('text', '').upper() or 'OPTION' in l.get('text', '').upper()]
        
        if menu_labels:
            print(f'   Menu-related LABEL statements:')
            for label in menu_labels[:10]:
                print(f'     • "{label["text"]}"')
            if len(menu_labels) > 10:
                print(f'     ... and {len(menu_labels) - 10} more')
        print()

print('=' * 80)

# Now search specifically for OPMENU, PGMENU, etc. by their content
print('\nSearching for specific menu screens by content...\n')

menu_searches = [
    ('OPMENU', 'ADVERTISING SOLICITATION'),
    ('PGMENU', 'INTERRUPT OPTIONS'),
    ('SPMENU', 'SPECIAL PROCEDURES'),
    ('LPMENU', 'LOAN PROCESSING'),
    ('BPMENU', 'BANK PROCESSING')
]

for menu_name, search_term in menu_searches:
    body = {
        'search': search_term,
        'searchFields': 'label_literals_json',
        'top': 1,
        'select': 'screen_name,label_literals_json'
    }
    
    r = requests.post(url, headers=HEADERS, json=body)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        if results:
            labels = json.loads(results[0].get('label_literals_json', '[]'))
            print(f'✓ {menu_name}: Found! ({len(labels)} LABEL statements)')
            # Show first few options
            for label in labels[:3]:
                text = label.get('text', '')
                if text and text[0].isdigit():
                    print(f'    - {text}')
        else:
            print(f'✗ {menu_name}: Not found with search "{search_term}"')
    print()
