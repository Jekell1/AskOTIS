"""Search for SCREENS.CPY or SCREEN.CPY copybook."""
import os, json, requests

vals=json.load(open('local.settings.json','r')).get('Values',{})
for k,v in vals.items():
    if k not in os.environ:
        os.environ[k]=str(v)

ep=os.getenv('AZURE_SEARCH_ENDPOINT')
key=os.getenv('AZURE_SEARCH_API_KEY') or os.getenv('AZURE_SEARCH_KEY')

# Search in copybooks index
def search_copybooks():
    url = f"{ep}/indexes/new_cobol_copybooks/docs/search?api-version=2024-07-01"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': 'SCREEN',
        'searchMode': 'any',
        'top': 20,
        'select': 'copybook_name,file_path,description'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"\n=== Found {len(results)} copybooks matching 'SCREEN' ===\n")
        for doc in results:
            print(f"Copybook: {doc.get('copybook_name')}")
            print(f"Path: {doc.get('file_path')}")
            print(f"Description: {doc.get('description', 'N/A')[:100]}")
            print("-" * 80)
    else:
        print(f"Copybooks search failed: {r.status_code}")
        print(r.text[:500])

# Search in code chunks for SCREENS.CPY content
def search_screens_cpy_content():
    url = f"{ep}/indexes/new_code_chunks/docs/search?api-version=2024-07-01"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': '"SCREENS.CPY" OR "SCREEN.CPY"',
        'searchMode': 'any',
        'top': 10,
        'select': 'text,program_id,chunk_id'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"\n=== Found {len(results)} code chunks referencing SCREENS.CPY ===\n")
        for doc in results:
            print(f"Chunk ID: {doc.get('chunk_id')}")
            print(f"Program: {doc.get('program_id')}")
            text = doc.get('text', '')
            if 'SCREEN' in text.upper():
                print("Content snippet:")
                print(text[:600])
            print("\n" + "="*80 + "\n")
    else:
        print(f"Code chunks search failed: {r.status_code}")
        print(r.text[:500])

# Search data items from SCREEN copybook
def search_screen_data_items():
    url = f"{ep}/indexes/new_cobol_data_items/docs/search?api-version=2024-07-01"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': 'SCREEN menu',
        'searchMode': 'any',
        'top': 30,
        'select': 'item_name,value_clause,program_id,file_path,full_clause'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"\n=== Found {len(results)} data items from SCREEN-related files ===\n")
        
        # Filter for items with VALUE clauses
        with_values = [d for d in results if d.get('value_clause') and 'SCREEN' in d.get('file_path', '').upper()]
        
        print(f"Items with VALUE clauses from SCREEN files: {len(with_values)}\n")
        
        for doc in with_values[:15]:
            print(f"File: {doc.get('file_path', '').split('/')[-1]}")
            print(f"Program: {doc.get('program_id')}")
            print(f"Field: {doc.get('item_name')}")
            print(f"VALUE: '{doc.get('value_clause')}'")
            print(f"Full: {doc.get('full_clause', '')[:100]}")
            print()
    else:
        print(f"Data items search failed: {r.status_code}")
        print(r.text[:500])

if __name__ == '__main__':
    print("Searching for SCREENS.CPY copybook and content...\n")
    search_copybooks()
    search_screens_cpy_content()
    search_screen_data_items()
