"""Search for screens with different LPMENU hash IDs."""
import os, json, requests

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

url = f"{endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Try all the hash variations
hashes = [
    '1778ece8d33afbb3a4814d8a992a8dd54632dc92',
    '7d8c4378a816f43b483af36f4f9e86efe03e9b87',
    '9e62bc4c1d02b643a48147d509e3406dd916a8ef'
]

print("Searching for LPMENU screens with different hash IDs...")
print("="*80)

found_any = False
for h in hashes:
    payload = {
        'search': '*',
        'filter': f"program_id eq '{h}'",
        'select': 'screen_id,program_id,summary',
        'top': 5
    }
    
    r = requests.post(url, headers=headers, json=payload)
    results = r.json().get('value', [])
    
    if results:
        print(f"\n✅ FOUND {len(results)} screen(s) for hash {h[:16]}...")
        found_any = True
        for doc in results:
            summary = doc.get('summary', '')
            if 'MASTER' in summary.upper() or 'DAILY PROCESSING' in summary:
                print(f"\n🎉 THIS IS THE MASTER MENU!")
                print(f"Screen ID: {doc.get('screen_id')}")
                print(f"\nSummary:\n{summary}\n")
    else:
        print(f"❌ No screens for hash {h[:16]}...")

if not found_any:
    print("\n❌ No screens found with any hash variation")
