"""
Extract field definitions from ALL copybooks in code_chunks index
and create/update entries in copybook_meta index.
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from pathlib import Path
import json
import re
from typing import Optional, Dict, List
from datetime import datetime

# Load credentials
with open('local.settings.json') as f:
    creds = json.load(f)['Values']

SEARCH_ENDPOINT = creds['AZURE_SEARCH_ENDPOINT']
SEARCH_KEY = creds['AZURE_SEARCH_KEY']

code_client = SearchClient(SEARCH_ENDPOINT, 'new_code_chunks', AzureKeyCredential(SEARCH_KEY))
meta_client = SearchClient(SEARCH_ENDPOINT, 'new_cobol_copybook_meta', AzureKeyCredential(SEARCH_KEY))

def parse_cobol_field_definition(line: str) -> Optional[Dict]:
    """Parse a COBOL field definition line."""
    # Pattern: level-number field-name [PIC clause] [VALUE clause]
    pattern = r'^(\d{2})\s+([A-Z0-9\-]+)(?:\s+PIC\s+([A-Z0-9\(\)]+))?(?:\s+VALUE\s+(.+?))?[.\s]*$'
    
    line = line.strip()
    if not line or line.startswith('*'):
        return None
    
    match = re.match(pattern, line, re.IGNORECASE)
    if match:
        level, name, pic, value = match.groups()
        return {
            'level': level,
            'name': name,
            'pic_clause': pic,
            'value_clause': value.strip() if value else None
        }
    return None

def extract_copybook_fields(copybook_content: str) -> Dict:
    """Extract field definitions from copybook content."""
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

def get_all_copybook_files() -> List[Dict]:
    """Get list of all unique copybook files from code_chunks using pagination."""
    print("Fetching all copybook files from code_chunks index (using pagination)...")
    
    # Collect unique copybook names using pagination
    seen_names = set()
    copybook_files = []
    skip = 0
    batch_size = 1000
    
    while True:
        try:
            results = code_client.search(
                search_text='*',
                select='name,path',
                top=batch_size,
                skip=skip
            )
            
            count = 0
            for result in results:
                count += 1
                name = result.get('name', '')
                if name and name.upper().endswith('.CPY') and name not in seen_names:
                    seen_names.add(name)
                    # Extract base name without extension
                    base_name = name.replace('.CPY', '').replace('.cpy', '')
                    copybook_files.append({
                        'file_name': name,
                        'base_name': base_name,
                        'copybook_name': base_name.replace('_', '-'),  # Convert to metadata format
                        'path': result.get('path', '')
                    })
            
            if count < batch_size:
                # No more results
                break
            
            skip += batch_size
            
            if skip % 10000 == 0:
                print(f"  Scanned {skip} chunks, found {len(copybook_files)} unique copybooks so far...")
            
        except Exception as e:
            print(f"  ⚠️ Error at skip={skip}: {e}")
            break
    
    print(f"  ✓ Found {len(copybook_files)} unique copybook files total")
    return copybook_files

def get_copybook_content(copybook_filename: str) -> Optional[str]:
    """Retrieve copybook source code from code_chunks index."""
    try:
        results = code_client.search(
            search_text=copybook_filename,
            select=['name', 'text', 'start_line', 'end_line', 'path'],
            top=100  # Get all chunks
        )
        
        # Collect all matching chunks
        all_chunks = []
        for result in results:
            name = result.get('name', '')
            if name.upper() == copybook_filename.upper():
                all_chunks.append({
                    'text': result.get('text', ''),
                    'start_line': result.get('start_line', 0),
                    'end_line': result.get('end_line', 0),
                    'path': result.get('path', '')
                })
        
        if not all_chunks:
            return None, None
        
        # Sort by start_line and combine all chunks
        all_chunks.sort(key=lambda x: x['start_line'])
        full_text = '\n'.join([c['text'] for c in all_chunks])
        file_path = all_chunks[0]['path']
        
        return full_text, file_path
    except Exception as e:
        print(f"  ⚠️ Error retrieving {copybook_filename}: {e}")
        return None, None

def process_copybooks():
    """Process all copybooks and extract field definitions."""
    print("="*80)
    print("EXTRACTING FIELD DEFINITIONS FROM ALL COPYBOOKS")
    print("="*80)
    
    # Get all copybook files
    copybook_files = get_all_copybook_files()
    total = len(copybook_files)
    
    print(f"\nFound {total} unique copybook files")
    print(f"Starting extraction...\n")
    
    # Track results
    processed = 0
    with_fields = 0
    no_fields = 0
    errors = 0
    
    batch = []
    batch_size = 100
    
    for i, copybook_info in enumerate(copybook_files, 1):
        file_name = copybook_info['file_name']
        copybook_name = copybook_info['copybook_name']
        
        if i % 100 == 0:
            print(f"Progress: {i}/{total} ({i*100//total}%)")
        
        # Get content
        content, file_path = get_copybook_content(file_name)
        
        if not content:
            no_fields += 1
            processed += 1
            continue
        
        # Extract fields
        try:
            field_data = extract_copybook_fields(content)
            
            if field_data['field_count'] > 0:
                # Create/update document for copybook_meta
                doc = {
                    'copybook_name': copybook_name,
                    'file_paths_json': json.dumps([file_path]) if file_path else None,
                    'fields_json': json.dumps(field_data['fields']),
                    'field_count': field_data['field_count'],
                    'record_names_json': json.dumps(field_data['record_names']) if field_data['record_names'] else None,
                    'has_field_definitions': True,
                    'updated_at': datetime.utcnow().isoformat() + 'Z'
                }
                
                batch.append(doc)
                with_fields += 1
                
                # Upload batch when it reaches batch_size
                if len(batch) >= batch_size:
                    try:
                        meta_client.merge_or_upload_documents(documents=batch)
                        print(f"  ✓ Uploaded batch of {len(batch)} documents (Total with fields: {with_fields})")
                        batch = []
                    except Exception as e:
                        print(f"  ⚠️ Batch upload error: {e}")
                        errors += len(batch)
                        batch = []
            else:
                no_fields += 1
            
            processed += 1
            
        except Exception as e:
            print(f"  ⚠️ Error processing {copybook_name}: {e}")
            errors += 1
            processed += 1
    
    # Upload remaining batch
    if batch:
        try:
            meta_client.merge_or_upload_documents(documents=batch)
            print(f"  ✓ Uploaded final batch of {len(batch)} documents")
        except Exception as e:
            print(f"  ⚠️ Final batch upload error: {e}")
            errors += len(batch)
    
    # Summary
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"Total copybooks processed: {processed}")
    print(f"With field definitions: {with_fields}")
    print(f"No field definitions: {no_fields}")
    print(f"Errors: {errors}")
    print(f"\n✅ Extraction complete!")

if __name__ == '__main__':
    process_copybooks()
