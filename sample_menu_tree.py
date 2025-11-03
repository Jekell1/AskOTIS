import os, json, requests

vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
api = '2025-08-01-preview'

url = f'{ep}/indexes/new_cobol_menu_trees/docs/search?api-version={api}'
r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                  json={'search': '*', 'top': 5})

docs = r.json().get('value', [])
print(f'Found {len(docs)} menu trees')

for doc in docs:
    print(f"\nRoot: {doc.get('root_program_id')}")
    print(f"Total nodes: {doc.get('total_nodes')}, UI nodes: {doc.get('total_ui_nodes')}, Max depth: {doc.get('max_depth')}")
    tree_json_str = doc.get('tree_json', '[]')
    
    # Parse the JSON string
    try:
        tree = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
        if tree and isinstance(tree, list):
            print(f"Tree structure sample (first 5 nodes):")
            for i, node in enumerate(tree[:5]):
                print(f"  {json.dumps(node, indent=2)}")
    except Exception as e:
        print(f"  Error parsing tree: {e}")
