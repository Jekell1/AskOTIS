"""
Updates summary_vector field dimension from 1536 to 3072 to match other indexes.
"""
import os, sys, json
from azure.search.documents.indexes import SearchIndexClient
from azure.core.credentials import AzureKeyCredential

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ['AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY']:
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

load_settings()

EP = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
KEY = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
INDEX_NAME = 'new_cobol_screen_nodes'
VECTOR_FIELD = 'summary_vector'
NEW_DIM = 3072

if not EP or not KEY:
    print("ERROR: Missing SEARCH_ENDPOINT/SEARCH_KEY or AZURE_SEARCH_ENDPOINT/AZURE_SEARCH_KEY")
    exit(1)

client = SearchIndexClient(endpoint=EP, credential=AzureKeyCredential(KEY))

print(f"Fetching current index definition for {INDEX_NAME}...")
index = client.get_index(INDEX_NAME)

# Find and update the vector field dimension
found = False
for field in index.fields:
    if field.name == VECTOR_FIELD:
        old_dim = field.vector_search_dimensions
        field.vector_search_dimensions = NEW_DIM
        found = True
        print(f"Updating {VECTOR_FIELD}: {old_dim} → {NEW_DIM} dimensions")
        break

if not found:
    print(f"ERROR: Field {VECTOR_FIELD} not found in index!")
    exit(1)

print("Updating index...")
client.create_or_update_index(index)
print(f"Done. {VECTOR_FIELD} now has {NEW_DIM} dimensions.")
print("Run backfill_screen_node_embeddings.py next to generate 3072-dim embeddings.")
