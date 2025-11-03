"""Investigate the help_fields index structure and integration."""
import json
import requests
import env_autoload

# Load settings
with open('local.settings.json', 'r') as f:
    settings = json.load(f)['Values']

endpoint = settings['SEARCH_ENDPOINT'].rstrip('/')
key = settings['SEARCH_KEY']
index_name = 'help_fields'

print("🔍 HELP_FIELDS INDEX ANALYSIS")
print("=" * 70)

# Get schema
schema_url = f"{endpoint}/indexes/{index_name}?api-version=2025-08-01-preview"
headers = {'api-key': key, 'Content-Type': 'application/json'}

response = requests.get(schema_url, headers=headers)

if response.status_code == 200:
    schema = response.json()
    print(f"\n✅ Index: {schema['name']}")
    print()
    print("📋 SCHEMA FIELDS:")
    print("-" * 70)
    
    fields = schema.get('fields', [])
    for field in fields:
        searchable = '🔍' if field.get('searchable') else '  '
        filterable = '🔎' if field.get('filterable') else '  '
        facetable = '📊' if field.get('facetable') else '  '
        sortable = '📈' if field.get('sortable') else '  '
        vector = '🎯' if field.get('dimensions') else '  '
        key_marker = '🔑' if field.get('key') else '  '
        
        print(f'{key_marker}{searchable}{filterable}{facetable}{sortable}{vector} {field["name"]:<30} ({field["type"]})')
        
        if field.get('dimensions'):
            print(f'      Vector dimensions: {field["dimensions"]}')
else:
    print(f"❌ Error: {response.status_code}")
    print(response.text)
    exit(1)

print()
print("=" * 70)
print("📊 SAMPLE DOCUMENTS:")
print("=" * 70)

# Get sample documents
search_url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
search_body = {
    "search": "*",
    "top": 5,
    "orderby": "help_text_length desc"
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    docs = results.get('value', [])
    total = results.get('@odata.count', 0)
    
    print(f"\nTotal documents: {total:,}")
    print(f"\nShowing top 5 documents (by help text length):\n")
    
    for i, doc in enumerate(docs, 1):
        print(f"\n{'-' * 70}")
        print(f"Document {i}:")
        print(f"{'-' * 70}")
        
        # Show all fields except vectors
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            if isinstance(value, list) and len(value) > 100:  # Skip large vectors
                print(f"  {key}: [Vector with {len(value)} dimensions]")
            elif isinstance(value, str) and len(value) > 200:
                print(f"  {key}: {value[:200]}...")
            else:
                print(f"  {key}: {value}")
else:
    print(f"❌ Error: {response.status_code}")
    print(response.text)

print()
print("=" * 70)
print("🔗 RELATIONSHIP TO SCREENS:")
print("=" * 70)

# Check if there are program/screen associations
search_body = {
    "search": "*",
    "facets": ["program_id,count:100"],
    "top": 0
}

response = requests.post(search_url, headers=headers, json=search_body)

if response.status_code == 200:
    results = response.json()
    facets = results.get('@search.facets', {})
    program_facets = facets.get('program_id', [])
    
    if program_facets:
        print(f"\n📌 Programs with help fields: {len(program_facets)}")
        print(f"\nTop 10 programs by help field count:")
        for facet in program_facets[:10]:
            prog = facet.get('value', 'N/A')
            count = facet.get('count', 0)
            print(f"  • {prog:<20} ({count:,} help fields)")
    else:
        print("\n⚠️  No program_id facets found")

print()
print("=" * 70)
print("🎯 USAGE IN RAG:")
print("=" * 70)

print("""
The help_fields index is integrated in the RAG system:

CONFIG (otis_rag/config.py):
  'help_fields': 'help_fields'

ROUTER (otis_rag/router.py):
  Used in "ui" profile: ["screen_nodes", "ui_paths", "help_fields"]

ROUTING LOGIC:
  - Questions about menus, screens, UI → includes help_fields
  - Questions about user interface → includes help_fields
  - Questions about what users see → includes help_fields

PURPOSE:
  - Provides field-level help documentation
  - Explains how forms/screens are used
  - Describes user interaction patterns
  - Complements screen_nodes (structure) with usage context

EXAMPLE QUERIES THAT WOULD USE help_fields:
  ✅ "What does the user see on the GLMENU screen?"
  ✅ "What are the menu options in GLMENU?"
  ✅ "How is the LPBW1IN form used?"
  ✅ "What fields are available on screen XYZ?"
""")

print()
print("=" * 70)
print("💡 RECOMMENDATIONS:")
print("=" * 70)
print("""
1. Help fields provide VALUABLE USAGE CONTEXT:
   - Screen definitions (screen_nodes) show STRUCTURE
   - Help fields (help_fields) show USAGE & PURPOSE
   - Together they give complete screen understanding

2. Current integration is GOOD:
   - Already in "ui" routing profile
   - Will be searched for screen/menu questions
   - Vector embeddings enable semantic search

3. Consider ENHANCING:
   - Link help_fields explicitly to screen_nodes (by program/screen name)
   - Create composite documents combining structure + help text
   - Add help text snippets to screen_nodes summary field
""")
