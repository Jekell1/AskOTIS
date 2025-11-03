"""Query help_fields index for AULNRG screen to see actual help content."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json
from otis_rag.config import Config

config = Config()

# Create search client for help_fields
search_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='help_fields',
    credential=AzureKeyCredential(config.search_key)
)

print("=" * 80)
print("HELP_FIELDS CONTENT FOR AULNRG SCREEN")
print("=" * 80)
print()

# Search for AULNRG help fields
results = search_client.search(
    search_text="AULNRG",
    select=["screen_id", "field_id", "module", "help_text", "valid_values_json", "function_keys"],
    top=50  # Get many results to see full coverage
)

count = 0
for doc in results:
    count += 1
    print(f"\n{'=' * 80}")
    print(f"FIELD #{count}")
    print('=' * 80)
    print(f"Screen ID:   {doc.get('screen_id', 'N/A')}")
    print(f"Field ID:    {doc.get('field_id', 'N/A')}")
    print(f"Module:      {doc.get('module', 'N/A')}")
    print(f"\nHelp Text:")
    print("-" * 80)
    help_text = doc.get('help_text', 'N/A')
    print(help_text[:500] + ('...' if len(help_text) > 500 else ''))
    
    if doc.get('function_keys'):
        print(f"\nFunction Keys: {doc.get('function_keys')}")
    
    if doc.get('valid_values_json'):
        print(f"\nValid Values: {doc.get('valid_values_json')}")
    print()

if count == 0:
    print("⚠️  NO HELP FIELDS FOUND FOR AULNRG")
    print()
    print("Possible reasons:")
    print("  1. Screen ID doesn't match 'AULNRG' exactly")
    print("  2. Help fields use different naming convention")
    print("  3. No help documentation exists for this screen")
    print()
    print("Let's search by module and see what we have...")
    print()
    
    # Try broader search
    results = search_client.search(
        search_text="audit register loan",
        select=["screen_id", "field_id", "module"],
        top=20
    )
    
    print("\nRelated screens found:")
    print("-" * 80)
    for doc in results:
        print(f"  {doc.get('screen_id', 'N/A'):40s} | Field: {doc.get('field_id', 'N/A'):15s} | Module: {doc.get('module', 'N/A')}")
else:
    print("=" * 80)
    print(f"TOTAL: Found {count} help fields for AULNRG screen")
    print("=" * 80)
