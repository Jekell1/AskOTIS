"""Search for MASTER MENU by its content."""
import os, json, requests

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

url = f"{endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Search for the unique combination of menu items
payload = {
    'search': 'DAILY PROCESSING REPORTS INQUIRIES COLLECTION BATCH',
    'select': 'screen_id,program_id,summary,display_literals_json',
    'top': 10
}

r = requests.post(url, headers=headers, json=payload)
results = r.json()

print("Searching for MASTER MENU by content...")
print("="*80)
print(f"Found {len(results.get('value', []))} screens\n")

for doc in results.get('value', []):
    screen_id = doc.get('screen_id', 'N/A')
    summary = doc.get('summary', '')
    
    # Check if this looks like the MASTER MENU
    if 'DAILY PROCESSING' in summary and 'REPORTS' in summary and 'INQUIRIES' in summary:
        print(f"✅ FOUND MASTER MENU!")
        print(f"Screen ID: {screen_id}")
        print(f"Program ID: {doc.get('program_id')}")
        print(f"\nSummary:\n{summary}\n")
        print("="*80)
        break
else:
    print("❌ MASTER MENU not found in results")
