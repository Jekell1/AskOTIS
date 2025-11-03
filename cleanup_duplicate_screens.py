"""Delete incomplete duplicate screens (keep only _1 screens for programs with duplicates)."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json
from collections import defaultdict

cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

# Get all screen IDs
results = list(client.search('*', select='screen_id,program_id,label_literals_json', top=2000))

print(f'Total screens: {len(results)}\n')

# Group by program_id
by_program = defaultdict(list)
for r in results:
    prog_id = r.get('program_id', '')
    screen_id = r['screen_id']
    
    # Extract sequence number
    if '_' in screen_id:
        seq = int(screen_id.split('_')[-1])
    else:
        seq = 0
    
    label_json = r.get('label_literals_json', '[]')
    num_labels = label_json.count('"text":')
    
    by_program[prog_id].append({
        'screen_id': screen_id,
        'seq': seq,
        'num_labels': num_labels
    })

# Find programs with multiple screens where _1 has significantly more labels
to_delete = []
for prog_id, screens in by_program.items():
    if len(screens) <= 1:
        continue
    
    screens.sort(key=lambda x: x['seq'])
    
    # Get _1 screen
    screen_1 = next((s for s in screens if s['seq'] == 1), None)
    if not screen_1:
        continue
    
    # Find other screens with significantly fewer labels
    for screen in screens:
        if screen['seq'] != 1 and screen_1['num_labels'] > screen['num_labels'] * 1.5:
            to_delete.append(screen['screen_id'])

print(f'Screens to delete: {len(to_delete)}')
print(f'Sample (first 10):')
for screen_id in to_delete[:10]:
    print(f'  - {screen_id}')

if to_delete:
    print(f'\n{"="*80}')
    confirm = input(f'Delete {len(to_delete)} incomplete screens? (yes/no): ')
    
    if confirm.lower() == 'yes':
        print('Deleting in batches of 100...')
        for i in range(0, len(to_delete), 100):
            batch = to_delete[i:i+100]
            docs = [{'screen_id': sid} for sid in batch]
            result = client.delete_documents(documents=docs)
            print(f'  Deleted batch {i//100 + 1}: {len(result)} documents')
        
        print(f'\n✓ Deleted {len(to_delete)} incomplete screens')
        print(f'Remaining screens should now be complete (aggregated from all chunks)')
    else:
        print('Cancelled.')
else:
    print('No screens to delete.')
