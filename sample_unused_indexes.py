"""Sample data from unused indexes to evaluate chatbot value."""
import os, json, requests

vals=json.load(open('local.settings.json','r',encoding='utf-8')).get('Values',{})
[os.environ.update({k:v}) for k,v in vals.items() if k in ('AZURE_SEARCH_ENDPOINT','SEARCH_ENDPOINT','AZURE_SEARCH_KEY','SEARCH_KEY')]
ep=(os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')).rstrip('/')
key=os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')

# Sample calls index
r = requests.post(f'{ep}/indexes/new_cobol_calls/docs/search?api-version=2025-08-01-preview',
    headers={'api-key':key,'Content-Type':'application/json'},
    json={'top':3,'select':'caller_program,callee_program,snippet,has_vector'})
print('=== new_cobol_calls (sample) ===')
for doc in r.json().get('value',[]):
    print(f"  {doc.get('caller_program')} → {doc.get('callee_program')}")
    snippet = doc.get('snippet', '')[:80]
    print(f"  Snippet: {snippet}...")
    print(f"  Has vector: {doc.get('has_vector')}")
print()

# Sample name_aliases
r = requests.post(f'{ep}/indexes/new_cobol_name_aliases/docs/search?api-version=2025-08-01-preview',
    headers={'api-key':key,'Content-Type':'application/json'},
    json={'top':5,'select':'canonical_name,alias,variant_type,kind'})
print('=== new_cobol_name_aliases (sample) ===')
for doc in r.json().get('value',[]):
    print(f"  {doc.get('canonical_name')} ← {doc.get('alias')} ({doc.get('variant_type')}, {doc.get('kind')})")
print()

# Sample menu_trees
r = requests.post(f'{ep}/indexes/new_cobol_menu_trees/docs/search?api-version=2025-08-01-preview',
    headers={'api-key':key,'Content-Type':'application/json'},
    json={'top':2,'select':'root_program_id,total_nodes,total_ui_nodes,max_depth'})
print('=== new_cobol_menu_trees (sample) ===')
for doc in r.json().get('value',[]):
    print(f"  Root: {doc.get('root_program_id')}, Nodes: {doc.get('total_nodes')}, UI: {doc.get('total_ui_nodes')}, Depth: {doc.get('max_depth')}")
print()

# Sample program_copybook_edges
r = requests.post(f'{ep}/indexes/new_cobol_program_copybook_edges/docs/search?api-version=2025-08-01-preview',
    headers={'api-key':key,'Content-Type':'application/json'},
    json={'top':5,'select':'program_id,copybook_name_plain,occurrence_count'})
print('=== new_cobol_program_copybook_edges (sample) ===')
for doc in r.json().get('value',[]):
    print(f"  {doc.get('program_id')} uses {doc.get('copybook_name_plain')} ({doc.get('occurrence_count')}x)")
print()

# Sample program_inventory
r = requests.post(f'{ep}/indexes/new_cobol_program_inventory/docs/search?api-version=2025-08-01-preview',
    headers={'api-key':key,'Content-Type':'application/json'},
    json={'top':3,'select':'program_id,copybook_count,paragraph_count,usage_rows'})
print('=== new_cobol_program_inventory (sample) ===')
for doc in r.json().get('value',[]):
    print(f"  {doc.get('program_id')}: {doc.get('copybook_count')} copybooks, {doc.get('paragraph_count')} paragraphs, {doc.get('usage_rows')} usage rows")
