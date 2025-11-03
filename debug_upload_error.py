"""
Debug the 400 error when uploading enhanced dependencies.
Test with a small sample first to identify the schema issue.
"""

import json
import requests
from datetime import datetime

def test_enhanced_dependencies_upload():
    """Test upload with a small sample to debug 400 error."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    # First, check if index exists and get its schema
    index_name = "enhanced_cobol_dependencies"
    schema_url = f"{endpoint}/indexes/{index_name}?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print("🔍 Checking index schema...")
    response = requests.get(schema_url, headers=headers)
    
    if response.status_code == 404:
        print("❌ Index does not exist. Creating it first...")
        create_test_index()
        return
    elif response.status_code == 200:
        schema = response.json()
        print("✅ Index exists. Fields:")
        for field in schema.get('fields', []):
            print(f"   {field['name']} ({field['type']})")
    else:
        print(f"❌ Error getting schema: {response.status_code}")
        print(response.text)
        return
    
    # Test with minimal document
    print("\n📤 Testing minimal document upload...")
    
    test_doc = {
        "@search.action": "mergeOrUpload",
        "dependency_id": "TEST_001",
        "caller_program": "APIPAY", 
        "called_resource": "TEST_PROGRAM",
        "reference_type": "CALL_STATIC",
        "reference_description": "Test call",
        "line_number": 100,
        "statement": "CALL TEST_PROGRAM",
        "file_path": "test/TEST.CBL",
        "file_size": 1000,
        "analysis_timestamp": datetime.now().isoformat(),
        "category": "PROGRAM_CALL",
        "is_program_call": True,
        "is_copybook": False,
        "is_system_call": False
    }
    
    upload_url = f"{endpoint}/indexes/{index_name}/docs/index?api-version=2024-07-01"
    
    response = requests.post(upload_url, headers=headers, json={"value": [test_doc]})
    
    if response.status_code in [200, 201]:
        print("✅ Test upload successful!")
        result = response.json()
        print(f"Result: {result}")
    else:
        print(f"❌ Test upload failed: {response.status_code}")
        print(f"Response: {response.text}")
        
        # Try to parse error details
        try:
            error_data = response.json()
            if 'value' in error_data:
                for item in error_data['value']:
                    if 'errorMessage' in item:
                        print(f"Error: {item['errorMessage']}")
                    if 'key' in item:
                        print(f"Key: {item['key']}")
        except:
            pass

def create_test_index():
    """Create the enhanced dependencies index."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "enhanced_cobol_dependencies"
    
    # Simplified index schema
    index_schema = {
        "name": index_name,
        "fields": [
            {"name": "dependency_id", "type": "Edm.String", "key": True, "searchable": False, "filterable": True},
            {"name": "caller_program", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "called_resource", "type": "Edm.String", "searchable": True, "filterable": True, "sortable": True},
            {"name": "reference_type", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},
            {"name": "reference_description", "type": "Edm.String", "searchable": True},
            {"name": "line_number", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "statement", "type": "Edm.String", "searchable": True},
            {"name": "file_path", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "file_size", "type": "Edm.Int64", "filterable": True, "sortable": True},
            {"name": "analysis_timestamp", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            {"name": "category", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},
            {"name": "is_program_call", "type": "Edm.Boolean", "filterable": True, "facetable": True},
            {"name": "is_copybook", "type": "Edm.Boolean", "filterable": True, "facetable": True},
            {"name": "is_system_call", "type": "Edm.Boolean", "filterable": True, "facetable": True}
        ]
    }
    
    # Create index
    create_url = f"{endpoint}/indexes?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print("🔧 Creating index...")
    response = requests.post(create_url, headers=headers, json=index_schema)
    
    if response.status_code in [200, 201]:
        print(f"✅ Created index: {index_name}")
        # Now test upload
        test_enhanced_dependencies_upload()
    else:
        print(f"❌ Failed to create index: {response.status_code}")
        print(f"Response: {response.text}")

if __name__ == "__main__":
    test_enhanced_dependencies_upload()