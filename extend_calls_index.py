"""
Extend the existing new_cobol_calls index with enhanced dependency data.
Add missing fields and upload comprehensive external reference data.
"""

import json
import requests
from datetime import datetime, timezone
from pathlib import Path
from comprehensive_external_references import extract_all_external_references

def add_fields_to_calls_index():
    """Add new fields to the existing new_cobol_calls index."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "new_cobol_calls"
    
    # Get current schema
    schema_url = f"{endpoint}/indexes/{index_name}?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print("🔧 EXTENDING NEW_COBOL_CALLS INDEX")
    print("=" * 50)
    
    response = requests.get(schema_url, headers=headers)
    if response.status_code != 200:
        print(f"❌ Error getting current schema: {response.status_code}")
        return False
    
    current_schema = response.json()
    
    # Add new fields to the schema
    new_fields = [
        {"name": "reference_type", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},
        {"name": "reference_description", "type": "Edm.String", "searchable": True},
        {"name": "category", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},
        {"name": "is_program_call", "type": "Edm.Boolean", "filterable": True, "facetable": True},
        {"name": "is_copybook", "type": "Edm.Boolean", "filterable": True, "facetable": True},
        {"name": "is_system_call", "type": "Edm.Boolean", "filterable": True, "facetable": True},
        {"name": "enhanced_data", "type": "Edm.Boolean", "filterable": True, "facetable": True}  # Flag for enhanced records
    ]
    
    # Check which fields need to be added
    existing_field_names = [f['name'] for f in current_schema['fields']]
    fields_to_add = [f for f in new_fields if f['name'] not in existing_field_names]
    
    if not fields_to_add:
        print("✅ All required fields already exist")
        return True
    
    print(f"📋 Adding {len(fields_to_add)} new fields:")
    for field in fields_to_add:
        print(f"   • {field['name']} ({field['type']})")
    
    # Update schema
    updated_schema = current_schema.copy()
    updated_schema['fields'].extend(fields_to_add)
    
    # Update the index
    update_response = requests.put(schema_url, headers=headers, json=updated_schema)
    
    if update_response.status_code in [200, 201, 204]:
        print("✅ Successfully extended index schema")
        return True
    else:
        print(f"❌ Failed to update schema: {update_response.status_code}")
        print(f"Response: {update_response.text}")
        return False

def categorize_reference_for_calls_index(ref_type: str, referenced_name: str):
    """Categorize a reference for the calls index."""
    
    is_program_call = False
    is_copybook = False
    is_system_call = False
    category = "OTHER"
    
    if ref_type in ['CALL_STATIC', 'CALL_DYNAMIC', 'EXEC_CICS_XCTL', 'EXEC_CICS_LINK', 'EXEC_CICS_LOAD']:
        is_program_call = True
        category = "PROGRAM_CALL"
    elif ref_type in ['COPY_SIMPLE', 'COPY_LIBRARY']:
        is_copybook = True
        category = "COPYBOOK"
    elif ref_type in ['EXEC_SQL', 'EXEC_SQL_CALL', 'FUNCTION_CALL']:
        is_system_call = True
        category = "SYSTEM_CALL"
    elif ref_type in ['CANCEL', 'CANCEL_DYNAMIC']:
        is_program_call = True
        category = "PROGRAM_CLEANUP"
    elif ref_type in ['SORT_INPUT_PROC', 'SORT_OUTPUT_PROC']:
        is_program_call = True
        category = "SORT_PROCEDURE"
    elif ref_type in ['EXIT_PROGRAM', 'STOP_RUN']:
        category = "CONTROL_FLOW"
    else:
        category = "OTHER"
    
    return category, is_program_call, is_copybook, is_system_call

def upload_enhanced_apipay_data():
    """Upload enhanced APIPAY dependency data to the extended calls index."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    # Read APIPAY.CBL
    apipay_path = Path('cobol_src/SP/APIPAY.CBL')
    if not apipay_path.exists():
        print("❌ APIPAY.CBL not found")
        return
    
    with open(apipay_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Extract all external references
    references = extract_all_external_references(content, 'APIPAY')
    
    print(f"\n📊 Processing APIPAY external references:")
    print(f"   Total references: {references['total_references']}")
    print(f"   Unique references: {references['unique_references']}")
    
    # Prepare documents for upload
    documents = []
    timestamp = datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
    
    for ref in references.get('all_references', []):
        # Create unique call_id for enhanced entries
        call_id = f"ENHANCED_APIPAY_{ref['line']:05d}_{ref['type']}"
        
        category, is_program_call, is_copybook, is_system_call = categorize_reference_for_calls_index(
            ref['type'], ref['referenced_name']
        )
        
        # Map to calls index structure
        doc = {
            "@search.action": "mergeOrUpload",
            "call_id": call_id,
            "caller_program": "APIPAY",
            "callee_program": ref['referenced_name'],
            "file_id": "cobol_src/SP/APIPAY.CBL",
            "file_path": "cobol_src/SP/APIPAY.CBL",
            "line": ref['line'],
            "col": 1,  # Default column
            "occurrence": 0,
            "call_type": "enhanced",  # New call type for enhanced data
            "is_dynamic": ref['type'] in ['CALL_DYNAMIC', 'CANCEL_DYNAMIC'],
            "snippet": ref['statement'][:200],  # Truncate long statements
            "has_vector": False,  # No vector for now
            # New enhanced fields
            "reference_type": ref['type'],
            "reference_description": ref['description'],
            "category": category,
            "is_program_call": is_program_call,
            "is_copybook": is_copybook,
            "is_system_call": is_system_call,
            "enhanced_data": True,
            "ingested_at": timestamp
        }
        
        documents.append(doc)
    
    # Upload to calls index
    upload_url = f"{endpoint}/indexes/new_cobol_calls/docs/index?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print(f"\n📤 Uploading {len(documents)} enhanced records...")
    
    # Upload in batches
    batch_size = 50
    uploaded_count = 0
    
    for i in range(0, len(documents), batch_size):
        batch = documents[i:i + batch_size]
        
        response = requests.post(upload_url, headers=headers, json={"value": batch})
        
        if response.status_code in [200, 201]:
            uploaded_count += len(batch)
            print(f"✅ Uploaded batch {i // batch_size + 1}: {len(batch)} documents (total: {uploaded_count})")
        else:
            print(f"❌ Failed to upload batch {i // batch_size + 1}: {response.status_code}")
            print(f"Error: {response.text[:200]}")
    
    print(f"\n📊 Summary:")
    print(f"   Successfully uploaded: {uploaded_count}/{len(documents)} enhanced records")
    print(f"   Enhanced data now available in new_cobol_calls index")
    
    return uploaded_count

def main():
    """Main function to extend the calls index with enhanced data."""
    
    print("🚀 EXTENDING NEW_COBOL_CALLS WITH ENHANCED DEPENDENCIES")
    print("=" * 70)
    
    # Step 1: Extend the schema
    if not add_fields_to_calls_index():
        print("❌ Failed to extend index schema")
        return
    
    # Step 2: Upload enhanced APIPAY data
    uploaded = upload_enhanced_apipay_data()
    
    if uploaded > 0:
        print(f"\n✅ SUCCESS: Enhanced {uploaded} dependency records in new_cobol_calls index")
        print("   RAG system can now access comprehensive APIPAY dependencies!")
    else:
        print("\n❌ No data was uploaded")

if __name__ == "__main__":
    main()