"""Find the COLLECTION MAINTENANCE MENU screen"""
import requests
import json

with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

ep = settings['AZURE_SEARCH_ENDPOINT']
key = settings['AZURE_SEARCH_KEY']

url = f"{ep}/indexes/new_cobol_screen_nodes/docs/search?api-version=2025-08-01-preview"

# Try different searches
searches = [
    'COLLECTION MAINTENANCE MENU',
    'COLLECTION MAINTENANCE',
    '"COLLECTION MAINTENANCE MENU"'
]

for search_term in searches:
    print(f"\n{'='*60}")
    print(f"Searching for: {search_term}")
    print('='*60)
    
    body = {
        'search': search_term,
        'searchFields': 'label_literals_json,summary_text',
        'select': 'screen_id,screen_name,label_literals_json',
        'top': 5
    }
    
    r = requests.post(url, 
                     headers={'api-key': key, 'Content-Type': 'application/json'}, 
                     json=body)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"Found {len(results)} results\n")
        
        for result in results:
            labels = json.loads(result.get('label_literals_json', '[]'))
            label_texts = [l['text'] if isinstance(l, dict) else l for l in labels]
            
            # Check if this screen has COLLECTION MAINTENANCE MENU
            has_collection_menu = any('COLLECTION MAINTENANCE MENU' in text for text in label_texts)
            
            print(f"Screen: {result['screen_id']}")
            print(f"Name: {result.get('screen_name', 'N/A')}")
            print(f"Label count: {len(labels)}")
            print(f"Has COLLECTION MAINTENANCE MENU: {has_collection_menu}")
            
            if has_collection_menu:
                print("\n*** THIS IS THE RIGHT SCREEN! ***")
                print("\nAll labels:")
                for i, text in enumerate(label_texts, 1):
                    print(f"  {i}. {text}")
            
            print()
