"""Get sample help_fields documents to understand the data structure."""
import json
import requests
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT'].rstrip('/')
key = settings['SEARCH_KEY']
index_name = 'help_fields'

print("📊 HELP_FIELDS SAMPLE DOCUMENTS")
print("=" * 70)

search_url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
headers = {'api-key': key, 'Content-Type': 'application/json'}

# Get sample documents
search_body = {
    "search": "*",
    "top": 5
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    docs = results.get('value', [])
    
    print(f"\nShowing 5 sample documents:\n")
    
    for i, doc in enumerate(docs, 1):
        print(f"\n{'=' * 70}")
        print(f"DOCUMENT {i}")
        print(f"{'=' * 70}")
        
        # Show key identifying fields first
        if 'screen_id' in doc:
            print(f"Screen ID:      {doc['screen_id']}")
        if 'field_id' in doc:
            print(f"Field ID:       {doc['field_id']}")
        if 'module' in doc:
            print(f"Module:         {doc['module']}")
        if 'field_type' in doc:
            print(f"Field Type:     {doc['field_type']}")
        
        print()
        
        # Show help text
        if 'help_text' in doc:
            help_text = doc['help_text']
            if len(help_text) > 300:
                print(f"Help Text:      {help_text[:300]}...")
                print(f"                (Total length: {len(help_text)} chars)")
            else:
                print(f"Help Text:      {help_text}")
        
        print()
        
        # Show field properties
        properties = []
        if doc.get('required'):
            properties.append("Required")
        if doc.get('optional'):
            properties.append("Optional")
        if doc.get('numeric'):
            properties.append("Numeric")
        if doc.get('date_field'):
            properties.append("Date")
        if doc.get('has_scan_window'):
            properties.append("Has Scan Window")
        
        if properties:
            print(f"Properties:     {', '.join(properties)}")
        
        if 'char_length' in doc and doc['char_length']:
            print(f"Length:         {doc['char_length']} characters")
        
        # Show function keys if present
        if 'function_keys' in doc and doc['function_keys']:
            print(f"Function Keys:  {', '.join(doc['function_keys'])}")
        
        # Show valid values if present
        if 'valid_values_json' in doc and doc['valid_values_json']:
            try:
                valid_vals = json.loads(doc['valid_values_json'])
                if valid_vals and len(valid_vals) <= 10:
                    print(f"Valid Values:   {valid_vals}")
                elif valid_vals:
                    print(f"Valid Values:   {valid_vals[:10]}... ({len(valid_vals)} total)")
            except:
                pass
        
        # Vector status
        has_vector = doc.get('has_vector', False)
        print(f"Has Vector:     {'✅ Yes' if has_vector else '❌ No'}")

else:
    print(f"❌ Error: {response.status_code}")
    print(response.text)
    exit(1)

print()
print("=" * 70)
print("📈 STATISTICS")
print("=" * 70)

# Get count
search_body = {
    "search": "*",
    "count": True,
    "top": 0
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    total = results.get('@odata.count', 0)
    print(f"\nTotal help field documents: {total:,}")

# Get module distribution
search_body = {
    "search": "*",
    "facets": ["module,count:20"],
    "top": 0
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    facets = results.get('@search.facets', {})
    module_facets = facets.get('module', [])
    
    if module_facets:
        print(f"\nTop modules with help fields:")
        for facet in module_facets[:15]:
            module = facet.get('value', 'N/A')
            count = facet.get('count', 0)
            print(f"  • {module:<20} ({count:,} fields)")

# Get field type distribution
search_body = {
    "search": "*",
    "facets": ["field_type,count:20"],
    "top": 0
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    facets = results.get('@search.facets', {})
    type_facets = facets.get('field_type', [])
    
    if type_facets:
        print(f"\nField types:")
        for facet in type_facets:
            ftype = facet.get('value', 'N/A')
            count = facet.get('count', 0)
            print(f"  • {ftype:<30} ({count:,} fields)")

print()
print("=" * 70)
print("💡 KEY INSIGHTS")
print("=" * 70)
print("""
✅ help_fields provides SCREEN-LEVEL HELP:
  - Field-by-field documentation
  - Usage instructions for each field
  - Valid values and constraints
  - Field properties (required, optional, numeric, date)
  - Function key mappings

✅ Rich metadata available:
  - Screen ID and Field ID for linking
  - Module for grouping
  - Field type classification
  - Character length limits
  - Scan window availability
  - 3072-dim vectors for semantic search

✅ Integration is ACTIVE:
  - Part of "ui" routing profile
  - Searched for screen/menu/UI questions
  - Complements screen_nodes with usage context

🎯 VALUE FOR RAG:
  - Explains HOW fields are used (not just WHAT they are)
  - Provides user-facing documentation
  - Helps answer "how do I use this screen?" questions
  - Adds business context to technical structure
""")
