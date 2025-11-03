"""Check if copybooks are actually in new_code_chunks"""
import os, json, requests

with open('local.settings.json', 'r') as f:
    settings = json.load(f)
    for key, value in settings.get('Values', {}).items():
        os.environ[key] = value

ENDPOINT = os.getenv('SEARCH_ENDPOINT', os.getenv('AZURE_SEARCH_ENDPOINT')).rstrip('/')
KEY = os.getenv('SEARCH_KEY', os.getenv('AZURE_SEARCH_KEY'))
API_VERSION = '2024-07-01'
INDEX_NAME = 'new_code_chunks'

HEADERS = {'Content-Type': 'application/json', 'api-key': KEY}

print('Checking copybook count in new_code_chunks...\n')

# Try different filter patterns
tests = [
    ("*.CPY exact match", "search.ismatch('*.CPY', 'name')"),
    ("CPY substring", "search.in(name, '*.CPY', ',')"),
    ("name endswith .CPY", "endswith(name, '.CPY')"),
]

for label, filter_expr in tests:
    url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
    body = {'search': '*', 'filter': filter_expr, 'top': 0, 'count': True}
    
    r = requests.post(url, headers=HEADERS, json=body)
    
    if r.status_code == 200:
        count = r.json().get('@odata.count', 0)
        print(f'{label:30s}: {count:,} chunks')
    else:
        print(f'{label:30s}: ERROR {r.status_code}')

# Try simple text search for .CPY
print()
print('Simple search for ".CPY"...')
url = f'{ENDPOINT}/indexes/{INDEX_NAME}/docs/search?api-version={API_VERSION}'
body = {'search': '.CPY', 'searchFields': 'name', 'top': 20, 'select': 'name'}
r = requests.post(url, headers=HEADERS, json=body)
if r.status_code == 200:
    results = r.json().get('value', [])
    print(f'Found {len(results)} results')
    unique_names = set([r['name'] for r in results])
    for name in sorted(unique_names)[:15]:
        print(f'  - {name}')
print()
