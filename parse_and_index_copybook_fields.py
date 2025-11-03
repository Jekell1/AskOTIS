"""
Parse COBOL Copybooks and Add Field Definitions to Index

This script:
1. Reads copybook content from code_chunks index
2. Parses COBOL field definitions (level numbers, names, PIC clauses)
3. Updates copybook_meta index with extracted field information

Usage:
    python parse_and_index_copybook_fields.py [--batch-size 10] [--push]
"""

from azure.search.documents import SearchClient
from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    SearchField, SearchFieldDataType, SimpleField, SearchableField
)
from azure.core.credentials import AzureKeyCredential
import json
import re
from datetime import datetime
from typing import List, Dict, Optional

# Load Azure credentials
with open('local.settings.json', 'r', encoding='utf-8') as f:
    settings = json.load(f)['Values']
    SEARCH_KEY = settings['AZURE_SEARCH_KEY']
    SEARCH_ENDPOINT = settings['AZURE_SEARCH_ENDPOINT']

credential = AzureKeyCredential(SEARCH_KEY)

def parse_cobol_field_definition(line: str) -> Optional[Dict]:
    """
    Parse a COBOL field definition line.
    
    Example inputs:
        01  CUSTOMER-RECORD.
        05  CUST-ID            PIC 9(8).
        05  CUST-NAME          PIC X(30).
        10  ADDR-LINE-1        PIC X(40).
        88  VALID-STATE        VALUE 'NY', 'CA', 'TX'.
    
    Returns dict with: level, name, pic_clause, value_clause
    """
    line = line.strip()
    if not line or line.startswith('*'):
        return None
    
    # Pattern: <level> <name> [PIC <type>] [VALUE <value>]
    # Level number: 01-49, 66, 77, 88
    pattern = r'^(\d{2})\s+([A-Z0-9\-]+)(?:\s+PIC\s+([A-Z0-9\(\)]+))?(?:\s+VALUE\s+(.+?))?[.\s]*$'
    match = re.match(pattern, line, re.IGNORECASE)
    
    if match:
        level, name, pic, value = match.groups()
        return {
            'level': level,
            'name': name,
            'pic_clause': pic,
            'value_clause': value
        }
    
    return None

def extract_copybook_fields(copybook_content: str) -> Dict:
    """
    Extract all field definitions from copybook content.
    
    Returns:
        {
            'fields': [list of field dicts],
            'field_count': int,
            'has_01_level': bool,
            'has_88_level': bool,
            'record_names': [list of 01-level names]
        }
    """
    lines = copybook_content.split('\n')
    fields = []
    record_names = []
    has_88 = False
    
    for line in lines:
        field = parse_cobol_field_definition(line)
        if field:
            fields.append(field)
            if field['level'] == '01':
                record_names.append(field['name'])
            if field['level'] == '88':
                has_88 = True
    
    return {
        'fields': fields,
        'field_count': len(fields),
        'has_01_level': any(f['level'] == '01' for f in fields),
        'has_88_level': has_88,
        'record_names': record_names
    }

def get_copybook_content(copybook_name: str, code_client: SearchClient) -> Optional[str]:
    """Retrieve copybook source code from code_chunks index."""
    try:
        # Convert copybook name: LN1099-EVA -> LN1099_EVA or LN1099-EVA
        # Try both formats (with hyphen and underscore)
        search_variants = [
            copybook_name,  # Original
            copybook_name.replace('-', '_'),  # Replace hyphens with underscores
            copybook_name.replace('_', '-'),  # Replace underscores with hyphens
        ]
        
        all_chunks = []
        for variant in search_variants:
            results = code_client.search(
                search_text=variant,
                select=['name', 'text', 'start_line', 'end_line'],
                top=50  # Get more chunks in case file is large
            )
            
            # Collect all matching chunks
            for result in results:
                name = result.get('name', '')
                if name and '.CPY' in name.upper():
                    # Check if name matches any variant (ignoring case and hyphen/underscore)
                    name_normalized = name.upper().replace('_', '').replace('-', '').replace('.CPY', '')
                    search_normalized = variant.upper().replace('_', '').replace('-', '')
                    
                    if search_normalized in name_normalized:
                        all_chunks.append({
                            'text': result.get('text', ''),
                            'start_line': result.get('start_line', 0),
                            'end_line': result.get('end_line', 0)
                        })
            
            if all_chunks:
                break  # Found matches, no need to try other variants
        
        if not all_chunks:
            return None
        
        # Sort by start_line and combine all chunks
        all_chunks.sort(key=lambda x: x['start_line'])
        full_text = '\n'.join([c['text'] for c in all_chunks])
        
        return full_text
    except Exception as e:
        print(f"  ⚠️ Error retrieving {copybook_name}: {e}")
        return None

def add_fields_to_copybook_index():
    """
    Main function: Add field definitions to existing copybooks.
    """
    # Check if we need to add new fields to the index
    index_client = SearchIndexClient(
        endpoint=SEARCH_ENDPOINT,
        credential=credential
    )
    
    copybook_index = index_client.get_index('new_cobol_copybook_meta')
    existing_fields = [f.name for f in copybook_index.fields]
    
    # Add new fields if needed
    new_fields_needed = [
        'fields_json',  # JSON array of field definitions
        'field_count',  # Total number of fields
        'record_names_json',  # JSON array of 01-level record names
        'has_field_definitions'  # Boolean flag
    ]
    
    fields_to_add = [f for f in new_fields_needed if f not in existing_fields]
    
    if fields_to_add:
        print(f"⚠️  Index schema needs updating. Missing fields: {fields_to_add}")
        print("    Run update_copybook_index_schema.py first!")
        return
    
    # Get clients
    copybook_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name='new_cobol_copybook_meta',
        credential=credential
    )
    
    code_client = SearchClient(
        endpoint=SEARCH_ENDPOINT,
        index_name='new_code_chunks',
        credential=credential
    )
    
    print("="*80)
    print("COPYBOOK FIELD EXTRACTION")
    print("="*80)
    
    # Get all copybooks
    copybooks = list(copybook_client.search(
        search_text='*',
        select=['copybook_name'],
        top=100
    ))
    
    print(f"\nFound {len(copybooks)} copybooks to process\n")
    
    processed = 0
    updated = 0
    failed = 0
    
    updates = []
    
    for cpybook in copybooks:
        cpyname = cpybook.get('copybook_name')
        if not cpyname:
            continue
        
        print(f"📋 Processing: {cpyname}")
        processed += 1
        
        # Get content
        content = get_copybook_content(cpyname, code_client)
        
        if not content:
            print(f"  ⚠️ No content found in code_chunks")
            failed += 1
            continue
        
        # Parse fields
        field_data = extract_copybook_fields(content)
        
        if field_data['field_count'] == 0:
            print(f"  ⚠️ No field definitions found")
            failed += 1
            continue
        
        print(f"  ✓ Extracted {field_data['field_count']} fields")
        if field_data['record_names']:
            print(f"    Records: {', '.join(field_data['record_names'][:3])}")
        
        # Prepare update document
        update_doc = {
            'copybook_name': cpyname,
            'fields_json': json.dumps(field_data['fields'][:100]),  # Limit to first 100
            'field_count': field_data['field_count'],
            'record_names_json': json.dumps(field_data['record_names']),
            'has_field_definitions': True
        }
        
        updates.append(update_doc)
        updated += 1
    
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"Processed: {processed}")
    print(f"Updated: {updated}")
    print(f"Failed: {failed}")
    
    if updates:
        print(f"\n💾 Ready to upload {len(updates)} copybook updates")
        print("\nPreview of first update:")
        print(json.dumps(updates[0], indent=2)[:500])
        
        response = input("\n\nUpload updates to Azure Search? (y/n): ")
        if response.lower() == 'y':
            print("\n📤 Uploading...")
            result = copybook_client.merge_or_upload_documents(documents=updates)
            print(f"✅ Upload complete! Processed {len(result)} documents")
        else:
            print("❌ Upload cancelled")
            print("💾 Saving to copybook_fields_update.json")
            with open('copybook_fields_update.json', 'w') as f:
                json.dump(updates, f, indent=2)

if __name__ == '__main__':
    add_fields_to_copybook_index()
