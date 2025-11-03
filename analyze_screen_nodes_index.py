"""
Analyze the new_cobol_screen_nodes index to understand what it contains.

This script examines the structure, content, and purpose of the screen nodes index.
"""
import os
import json
import requests
from secrets_loader import load_secrets

def analyze_screen_nodes():
    """Analyze the screen nodes index structure and content."""
    load_secrets()  # Load into environment variables
    
    print("🔍 ANALYZING NEW_COBOL_SCREEN_NODES INDEX")
    print("=" * 60)
    
    # Get configuration from environment
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        print("❌ Failed to load search configuration")
        return
    
    # Get index schema
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    # 1. Get index definition
    print("\n📋 INDEX SCHEMA:")
    schema_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes?api-version=2023-11-01"
    schema_response = requests.get(schema_url, headers=headers)
    
    if schema_response.status_code == 200:
        schema = schema_response.json()
        print(f"   Index Name: {schema['name']}")
        print(f"   Fields: {len(schema['fields'])}")
        
        print("\n   Key Fields:")
        for field in schema['fields']:
            field_info = f"   - {field['name']} ({field['type']})"
            if field.get('key'):
                field_info += " [KEY]"
            if field.get('searchable'):
                field_info += " [SEARCHABLE]"
            if field.get('filterable'):
                field_info += " [FILTERABLE]"
            print(field_info)
    
    # 2. Get record count and sample data
    print("\n📊 INDEX CONTENT:")
    search_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    # Get total count
    count_body = {"search": "*", "top": 0, "count": True}
    count_response = requests.post(search_url, headers=headers, json=count_body)
    
    if count_response.status_code == 200:
        count_data = count_response.json()
        total_records = count_data.get('@odata.count', 0)
        print(f"   Total Records: {total_records:,}")
    
    # Get sample records
    sample_body = {"search": "*", "top": 3}
    sample_response = requests.post(search_url, headers=headers, json=sample_body)
    
    if sample_response.status_code == 200:
        sample_data = sample_response.json()
        records = sample_data.get('value', [])
        
        if records:
            print(f"\n📄 SAMPLE RECORDS ({len(records)} shown):")
            for i, record in enumerate(records, 1):
                print(f"\n   Record {i}:")
                print(f"   - Screen ID: {record.get('screen_id', 'N/A')}")
                print(f"   - Program ID: {record.get('program_id', 'N/A')}")
                print(f"   - Screen Name: {record.get('screen_name', 'N/A')}")
                print(f"   - Field Count: {record.get('field_count', 'N/A')}")
                print(f"   - Action Count: {record.get('action_count', 'N/A')}")
                print(f"   - Transition Count: {record.get('transition_count', 'N/A')}")
                
                # Show fields sample
                if record.get('fields_json'):
                    try:
                        fields = json.loads(record['fields_json'])
                        print(f"   - Sample Fields: {len(fields)} total")
                        if fields:
                            first_field = fields[0]
                            print(f"     First field: {first_field}")
                    except:
                        print("   - Fields JSON: [parsing error]")
                
                # Show actions sample
                if record.get('actions_json'):
                    try:
                        actions = json.loads(record['actions_json'])
                        print(f"   - Sample Actions: {len(actions)} total")
                        if actions:
                            print(f"     Actions: {actions[:3]}")
                    except:
                        print("   - Actions JSON: [parsing error]")
                
                # Show summary
                if record.get('summary_text'):
                    summary = record['summary_text'][:150]
                    print(f"   - Summary: {summary}...")
    
    # 3. Analyze field distributions
    print("\n📈 FIELD DISTRIBUTIONS:")
    
    # Field count distribution
    facet_body = {
        "search": "*",
        "top": 0,
        "facets": ["field_count,count:20", "action_count,count:20", "program_id,count:10"]
    }
    facet_response = requests.post(search_url, headers=headers, json=facet_body)
    
    if facet_response.status_code == 200:
        facet_data = facet_response.json()
        facets = facet_data.get('@search.facets', {})
        
        if 'field_count' in facets:
            print("   Field Count Distribution:")
            for item in facets['field_count'][:10]:
                print(f"     {item['value']} fields: {item['count']} screens")
        
        if 'action_count' in facets:
            print("\n   Action Count Distribution:")
            for item in facets['action_count'][:10]:
                print(f"     {item['value']} actions: {item['count']} screens")
        
        if 'program_id' in facets:
            print("\n   Top Programs with Screens:")
            for item in facets['program_id'][:10]:
                print(f"     {item['value']}: {item['count']} screens")
    
    # 4. Purpose and Usage
    print("\n🎯 PURPOSE & USAGE:")
    print("   The new_cobol_screen_nodes index captures:")
    print("   1. COBOL SCREEN SECTION definitions (UI forms)")
    print("   2. Field definitions with PIC clauses and attributes")
    print("   3. Actionable elements (function keys, buttons, verbs)")
    print("   4. Program-to-program transitions from screen logic")
    print("   5. Searchable summaries for RAG/semantic search")
    print("\n   Use Cases:")
    print("   - Understanding COBOL application UI structure")
    print("   - Mapping user interaction flows")
    print("   - Documenting legacy screen layouts")
    print("   - Supporting modernization efforts")
    print("   - Semantic search for screen functionality")

if __name__ == "__main__":
    analyze_screen_nodes()