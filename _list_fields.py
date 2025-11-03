import os, json, requests
from secrets_loader import load_secrets

load_secrets()

ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
ep = ep.rstrip('/')

r = requests.get(
    f'{ep}/indexes/new_cobol_variable_usage?api-version=2025-08-01-preview',
    headers={'api-key': key}
)

fields = r.json().get('fields', [])
print("All fields in new_cobol_variable_usage:")
for f in fields:
    print(f"  {f['name']} ({f.get('type')})")
