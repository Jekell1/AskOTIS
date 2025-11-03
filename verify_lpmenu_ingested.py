"""Verify LPMENU_SCN.CPY was ingested into new_code_chunks."""
import os, json, requests

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

url = f"{endpoint}/indexes/new_code_chunks/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Try different search approaches
searches = [
    {'search': 'LPMENU_SCN', 'searchFields': 'path,name'},
    {'search': '*', 'filter': "name eq 'LPMENU_SCN.CPY'"},
    {'search': '*', 'filter': "search.ismatch('LPMENU_SCN', 'path')"}
]

print("="*80)
print(f"Searching new_code_chunks for LPMENU_SCN.CPY...")
print("="*80)

found = False
for i, payload in enumerate(searches, 1):
    payload['select'] = 'path,name,chunk_id,start_line,end_line,text'
    payload['top'] = 5
    
    print(f"\nAttempt {i}: {payload.get('search')} {payload.get('filter', '')}")
    r = requests.post(url, headers=headers, json=payload)
    results = r.json()
    
    if results.get('value'):
        print(f"✅ FOUND {len(results['value'])} chunks!\n")
        found = True
        for doc in results['value']:
            print(f"  {doc.get('name')} (lines {doc.get('start_line')}-{doc.get('end_line')})")
            print(f"  Path: {doc.get('path')}")
            text = doc.get('text', '')
            if 'MASTER' in text.upper() and 'MENU' in text.upper():
                print(f"  ✅ Contains MASTER MENU content!")
        break
    else:
        print("  Not found with this method")

if not found:
    print("\n" + "="*80)
    print("❌ LPMENU_SCN.CPY NOT FOUND in any search")
    print("\nLet's check what LIBLP files ARE in the index...")
    
    payload = {
        'search': '*',
        'filter': "search.ismatch('LIBLP', 'path')",
        'select': 'path',
        'top': 20
    }
    r = requests.post(url, headers=headers, json=payload)
    results = r.json()
    liblp_files = set(d['path'] for d in results.get('value', []))
    print(f"Found {len(liblp_files)} LIBLP files:")
    for path in sorted(liblp_files):
        print(f"  {path}")
