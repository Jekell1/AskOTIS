"""View LPMENU_SCN chunks from code_chunks."""
import os, json, requests

settings = json.load(open('local.settings.json'))
endpoint = settings['Values']['SEARCH_ENDPOINT']
key = settings['Values']['SEARCH_KEY']

url = f"{endpoint}/indexes/new_code_chunks/docs/search?api-version=2024-07-01"
headers = {'api-key': key, 'Content-Type': 'application/json'}

payload = {
    'search': '*',
    'filter': "name eq 'LPMENU_SCN.CPY'",
    'select': 'text,start_line,end_line',
    'top': 5
}

r = requests.post(url, headers=headers, json=payload)
results = r.json()

print(f"LPMENU_SCN.CPY chunks in code_chunks:")
print("="*80)

for i, doc in enumerate(results.get('value', []), 1):
    print(f"\nChunk {i}: Lines {doc.get('start_line')}-{doc.get('end_line')}")
    text = doc.get('text', '')
    # Show first 800 chars
    print(text[:800])
    if 'MASTER' in text.upper() or 'MENU' in text.upper():
        print("\n>>> Contains MENU content! <<<")
    print("-"*80)
