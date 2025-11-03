"""Check for duplicate screens (programs with multiple _1, _2, etc.)"""
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
    # Extract sequence number from screen_id (e.g., EA66581C..._2 -> 2)
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

# Find programs with multiple screens
duplicates = {prog_id: screens for prog_id, screens in by_program.items() if len(screens) > 1}

print(f'Programs with multiple screens: {len(duplicates)}\n')
print('='*80)

# Show programs where different sequences have different label counts (potential incomplete screens)
suspicious = []
for prog_id, screens in duplicates.items():
    screens.sort(key=lambda x: x['seq'])
    label_counts = [s['num_labels'] for s in screens]
    
    # If label counts vary significantly, it's suspicious
    if len(set(label_counts)) > 1 and max(label_counts) > min(label_counts) * 1.5:
        suspicious.append((prog_id, screens))

print(f'Suspicious programs (varying label counts): {len(suspicious)}\n')

for prog_id, screens in suspicious[:20]:  # Show first 20
    print(f'\nProgram: {prog_id[:40]}...')
    for s in screens:
        print(f"  {s['screen_id']:50} labels={s['num_labels']}")
    
print(f'\n{"="*80}')
print(f'Total suspicious programs: {len(suspicious)}')
print(f'These may have incomplete screens that should be deleted.')
