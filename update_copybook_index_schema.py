"""
Update Copybook Index Schema to Include Field Definitions

Adds fields to store parsed copybook field information.
"""

from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    SearchField, SearchFieldDataType, SimpleField, SearchableField
)
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json', 'r', encoding='utf-8') as f:
    settings = json.load(f)['Values']
    SEARCH_KEY = settings['AZURE_SEARCH_KEY']
    SEARCH_ENDPOINT = settings['AZURE_SEARCH_ENDPOINT']

index_client = SearchIndexClient(
    endpoint=SEARCH_ENDPOINT,
    credential=AzureKeyCredential(SEARCH_KEY)
)

print("="*80)
print("UPDATE COPYBOOK INDEX SCHEMA")
print("="*80)

# Get current index
index = index_client.get_index('new_cobol_copybook_meta')

print(f"\nCurrent index: {index.name}")
print(f"Current fields: {len(index.fields)}")

# Define new fields to add
new_fields = [
    SearchableField(
        name="fields_json",
        type=SearchFieldDataType.String,
        searchable=True,
        filterable=False,
        sortable=False
    ),
    SimpleField(
        name="field_count",
        type=SearchFieldDataType.Int32,
        filterable=True,
        sortable=True
    ),
    SearchableField(
        name="record_names_json",
        type=SearchFieldDataType.String,
        searchable=True,
        filterable=False,
        sortable=False
    ),
    SimpleField(
        name="has_field_definitions",
        type=SearchFieldDataType.Boolean,
        filterable=True,
        sortable=False
    )
]

# Check which fields already exist
existing_field_names = [f.name for f in index.fields]
fields_to_add = [f for f in new_fields if f.name not in existing_field_names]

if not fields_to_add:
    print("\n✅ All fields already exist in index!")
    print("\nExisting fields:")
    for fname in ['fields_json', 'field_count', 'record_names_json', 'has_field_definitions']:
        if fname in existing_field_names:
            print(f"  ✓ {fname}")
else:
    print(f"\n📝 Need to add {len(fields_to_add)} new fields:")
    for f in fields_to_add:
        print(f"  + {f.name} ({f.type})")
    
    # Add new fields to index
    for new_field in fields_to_add:
        index.fields.append(new_field)
    
    print("\n🔄 Updating index...")
    try:
        index_client.create_or_update_index(index)
        print("✅ Index schema updated successfully!")
        
        print("\n📋 New schema:")
        print(f"Total fields: {len(index.fields)}")
        for f in index.fields:
            print(f"  - {f.name}")
    except Exception as e:
        print(f"❌ Error updating index: {e}")
        print("\nThis might require recreating the index or using the Azure Portal")
