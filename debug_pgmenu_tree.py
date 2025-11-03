"""Debug PGMENU tree_json to see why names are Unknown."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config
import json

config = Config()
client = SearchClient(config.search_endpoint, 'new_cobol_menu_trees', AzureKeyCredential(config.search_key))

results = list(client.search('*', filter="root_program_id eq 'PGMENU'", top=1))

if results:
    r = results[0]
    print("PGMENU Document Fields:")
    print("="*80)
    
    for key in sorted(r.keys()):
        if key.startswith('@'):
            continue
        value = r[key]
        if isinstance(value, str) and len(value) > 200:
            print(f"{key}: {value[:200]}...")
        elif isinstance(value, list) and len(value) > 10:
            print(f"{key}: [{len(value)} items] {value[:5]}...")
        else:
            print(f"{key}: {value}")
    
    print("\n" + "="*80)
    print("Raw tree_json (first 1000 chars):")
    print("="*80)
    tree_json = r.get('tree_json', '')
    print(tree_json[:1000] if tree_json else "EMPTY")
    
    print("\n" + "="*80)
    print("Parsed tree_json:")
    print("="*80)
    if tree_json:
        try:
            tree = json.loads(tree_json) if isinstance(tree_json, str) else tree_json
            print(json.dumps(tree, indent=2)[:2000])
        except Exception as e:
            print(f"Error parsing: {e}")
