import os, json, requests

vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
ep = (vals.get('AZURE_SEARCH_ENDPOINT') or vals.get('SEARCH_ENDPOINT')).rstrip('/')
key = vals.get('AZURE_SEARCH_KEY') or vals.get('SEARCH_KEY')
api = '2025-08-01-preview'

url = f'{ep}/indexes/new_cobol_menu_trees/docs/search?api-version={api}'
r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                  json={'search': '*', 'top': 3})

docs = r.json().get('value', [])
print(f'Found {len(docs)} documents')

if docs:
    doc = docs[0]
    print(f'Keys: {list(doc.keys())}')
    print(f'Has content_vector: {"content_vector" in doc}')
    if 'content_vector' in doc:
        vec = doc['content_vector']
        print(f'Vector type: {type(vec)}, len: {len(vec) if vec else 0}')
