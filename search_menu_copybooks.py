"""Search for PGMENU copybooks in the data."""
import os, json, requests

vals=json.load(open('local.settings.json','r')).get('Values',{})
for k,v in vals.items():
    if k not in os.environ:
        os.environ[k]=str(v)

ep=os.getenv('AZURE_SEARCH_ENDPOINT')
key=os.getenv('AZURE_SEARCH_API_KEY') or os.getenv('AZURE_SEARCH_KEY')

def search_pgmenu_copybooks():
    """Search for PGMENU copybook definitions."""
    url = f"{ep}/indexes/new_cobol_data_items/docs/search?api-version=2024-07-01"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': 'PGMENU',
        'searchMode': 'any',
        'top': 50,
        'select': 'item_name,value_clause,program_id,file_path,level,full_clause'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"=== Found {len(results)} PGMENU-related data items ===\n")
        
        # Group by file
        by_file = {}
        for doc in results:
            file = doc.get('file_path', 'unknown')
            if file not in by_file:
                by_file[file] = []
            by_file[file].append(doc)
        
        for file, items in by_file.items():
            print(f"\nFile: {file}")
            print(f"Items: {len(items)}")
            
            # Show items with VALUE clauses
            with_values = [i for i in items if i.get('value_clause')]
            if with_values:
                print(f"With VALUES: {len(with_values)}")
                for item in with_values[:5]:
                    print(f"  {item.get('level'):02d} {item.get('item_name')} = '{item.get('value_clause')}'")
            print()
    else:
        print(f"Failed: {r.status_code}")

def search_menu_option_codes():
    """Search for menu option codes like '010.'"""
    url = f"{ep}/indexes/new_cobol_data_items/docs/search?api-version=2024-07-01"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': '"010." OR "020." OR "030."',
        'searchMode': 'any',
        'top': 20,
        'select': 'item_name,value_clause,program_id,file_path'
    }
    
    r = requests.post(url, headers=headers, json=payload)
    
    if r.status_code == 200:
        results = r.json().get('value', [])
        print(f"=== Found {len(results)} menu option code patterns ===\n")
        
        for doc in results:
            print(f"{doc.get('file_path')} -> {doc.get('item_name')} = '{doc.get('value_clause')}'")
    else:
        print(f"Failed: {r.status_code}")

if __name__ == '__main__':
    search_pgmenu_copybooks()
    print("\n" + "="*80 + "\n")
    search_menu_option_codes()
