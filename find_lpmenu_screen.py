"""Find LPMENU screen by its hash ID."""
import os, json, requests

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

lpmenu_hash = '9e62bc4c1d02b643a48147d509e3406dd916a8ef'

url = f"{endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': '*',
    'filter': f"program_id eq '{lpmenu_hash}'",
    'select': 'program_id,screen_id,summary,display_literals_json',
    'top': 5
}

r = requests.post(url, headers=headers, json=payload)
results = r.json()

print(f"Searching for LPMENU_SCN screens (hash: {lpmenu_hash[:12]}...)...")
print("="*80)

if results.get('value'):
    print(f"\n✅ Found {len(results['value'])} screen(s) from LPMENU_SCN.CPY!\n")
    for doc in results['value']:
        print(f"Screen ID: {doc.get('screen_id')}")
        print(f"Program ID: {doc.get('program_id')}")
        print(f"\nSummary:\n{doc.get('summary')}\n")
        print(f"Display Literals:\n{doc.get('display_literals_json')[:500]}\n")
        print("="*80)
else:
    print("\n❌ No screens found for LPMENU_SCN.CPY")
    print("\nThis means build_screen_nodes.py didn't extract it.")
    print("Let me check if LPMENU_SCN.CPY has LABEL LINE statements...")
