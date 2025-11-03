"""Compare summary quality of LPMENU screens."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

results = list(client.search(
    'main menu options DAILY PROCESSING REPORTS INQUIRIES COLLECTION BATCH',
    select='screen_id,summary_text',
    top=10
))

print(f'Found {len(results)} screens\n')
print('='*80)

for i, r in enumerate(results, 1):
    screen_id = r['screen_id']
    summary = r.get('summary_text', '')
    
    print(f'\n{i}. Screen: {screen_id}')
    print(f'   @search.score: {r.get("@search.score", 0)}')
    print(f'   Summary length: {len(summary)} chars')
    print(f'   Summary preview:')
    print('   ' + '\n   '.join(summary[:600].split('\n')))
    print('='*80)
