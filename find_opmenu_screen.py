"""Find OPMENU in screen nodes by checking all screens with menu text"""
import os, json, requests

with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('SEARCH_ENDPOINT', os.getenv('AZURE_SEARCH_ENDPOINT')).rstrip('/')
KEY = os.getenv('SEARCH_KEY', os.getenv('AZURE_SEARCH_KEY'))
API_VERSION = '2024-07-01'

HEADERS = {'Content-Type': 'application/json', 'api-key': KEY}

print('Searching for OPMENU in screen_nodes...\n')

# Search for screens with "ADVERTISING" (which is in OPMENU_SCN.CPY)
url = f'{ENDPOINT}/indexes/new_cobol_screen_nodes/docs/search?api-version={API_VERSION}'
body = {
    'search': 'ADVERTISING SOLICITATION',
    'top': 10,
    'select': 'screen_id,program_id,screen_name,label_literals_json'
}

r = requests.post(url, headers=HEADERS, json=body)

if r.status_code != 200:
    print(f'ERROR: {r.status_code} - {r.text[:500]}')
else:
    results = r.json().get('value', [])
    print(f'Found {len(results)} screens with "ADVERTISING SOLICITATION"')
    print()
    
    for result in results:
        print(f'Screen: {result["screen_name"]}')
        print(f'  Program ID: {result["program_id"]}')
        print(f'  Screen ID: {result["screen_id"]}')
        
        labels = json.loads(result.get('label_literals_json', '[]'))
        if labels:
            print(f'  LABEL statements ({len(labels)} total):')
            for label in labels[:5]:
                print(f'    - Line {label["line"]}, Col {label["col"]}: "{label["text"]}"')
            if len(labels) > 5:
                print(f'    ... and {len(labels) - 5} more')
        print()
