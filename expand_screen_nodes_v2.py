#!/usr/bin/env python3
"""
Expand Screen Nodes Dataset - Find More COBOL Screen Definitions
Using available indexes: code-chunks, new_code_chunks, new-cobol-files
"""

import os
import json
import requests
import re
import hashlib
import datetime
from typing import List, Dict, Any
from secrets_loader import load_secrets

# Improved regex patterns for COBOL screen elements
FIELD_DEF_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)\s+PIC\s+([^.\s]+)\.?\s*(.*)', re.IGNORECASE)
LEVEL_01_RE = re.compile(r'^\s*01\s+([A-Z0-9-]+)', re.IGNORECASE)
SCREEN_SECTION_RE = re.compile(r'SCREEN\s+SECTION', re.IGNORECASE)
ACCEPT_RE = re.compile(r'ACCEPT\s+([A-Z0-9-]+)', re.IGNORECASE)
DISPLAY_RE = re.compile(r'DISPLAY\s+([A-Z0-9-]+)', re.IGNORECASE)
LINE_COL_RE = re.compile(r'(LINE|COLUMN|COL)\s+(\d+)', re.IGNORECASE)
VALUE_RE = re.compile(r'VALUE\s+["\']([^"\']*)["\']', re.IGNORECASE)

def expand_screen_nodes_v2():
    """Expand screen nodes using available indexes."""
    
    print("🔍 EXPANDING SCREEN NODES DATASET V2")
    print("=" * 50)
    
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    # Check current screen count
    screen_index_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    current_count_body = {"search": "*", "top": 0, "count": True}
    response = requests.post(screen_index_url, headers=headers, json=current_count_body)
    current_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    print(f"📊 Current screen nodes: {current_count}")
    
    # Try different available indexes
    chunk_indexes = [
        "code-chunks",
        "new_code_chunks"
    ]
    
    all_chunks = {}
    timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + 'Z'
    
    for index_name in chunk_indexes:
        print(f"\n🔍 Searching index: {index_name}")
        chunks_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Test if index exists and get some sample data
        test_body = {"search": "*", "top": 1, "select": "*"}
        test_response = requests.post(chunks_url, headers=headers, json=test_body)
        
        if test_response.status_code != 200:
            print(f"   ❌ Index {index_name} not accessible: {test_response.status_code}")
            continue
            
        # Check what fields are available
        test_data = test_response.json().get('value', [])
        if test_data:
            print(f"   ✅ Index accessible, sample fields: {list(test_data[0].keys())}")
            
            # Adjust field names based on what's available
            sample_record = test_data[0]
            
            # Determine field names for this index
            text_field = None
            program_id_field = None
            chunk_id_field = None
            
            for field in sample_record.keys():
                if 'text' in field.lower() or 'content' in field.lower():
                    text_field = field
                if 'program' in field.lower() and 'id' in field.lower():
                    program_id_field = field
                if 'chunk' in field.lower() and 'id' in field.lower():
                    chunk_id_field = field
                if field == 'id' and not chunk_id_field:
                    chunk_id_field = field
            
            print(f"   📝 Using fields: text='{text_field}', program_id='{program_id_field}', chunk_id='{chunk_id_field}'")
            
            if not text_field:
                print(f"   ⚠️ No text field found in {index_name}")
                continue
            
            # Search for screen-related content
            search_strategies = [
                "SCREEN SECTION",
                "ACCEPT AND DISPLAY", 
                "PIC AND (LINE OR COLUMN)",
                "01 AND PIC AND (ACCEPT OR DISPLAY)"
            ]
            
            for strategy in search_strategies:
                print(f"      🔎 Strategy: {strategy}")
                
                select_fields = []
                if chunk_id_field:
                    select_fields.append(chunk_id_field)
                if program_id_field:
                    select_fields.append(program_id_field)
                if text_field:
                    select_fields.append(text_field)
                
                search_body = {
                    "search": strategy,
                    "top": 200,
                    "queryType": "simple",
                    "select": ",".join(select_fields) if select_fields else "*"
                }
                
                response = requests.post(chunks_url, headers=headers, json=search_body)
                
                if response.status_code == 200:
                    chunks = response.json().get('value', [])
                    print(f"         Found: {len(chunks)} chunks")
                    
                    for chunk in chunks:
                        chunk_id = chunk.get(chunk_id_field) or chunk.get('id', f"chunk_{len(all_chunks)}")
                        
                        # Normalize chunk data
                        normalized_chunk = {
                            'chunk_id': chunk_id,
                            'program_id': chunk.get(program_id_field, 'unknown'),
                            'text': chunk.get(text_field, ''),
                            'source_index': index_name
                        }
                        
                        all_chunks[chunk_id] = normalized_chunk
                else:
                    print(f"         ❌ Error: {response.status_code}")
    
    print(f"\n📦 Total unique chunks to process: {len(all_chunks)}")
    
    if not all_chunks:
        print("❌ No chunks found to process. Trying alternative approach...")
        return try_alternative_expansion(search_endpoint, headers, current_count)
    
    # Process chunks
    print("\n🔄 Processing chunks for screen definitions...")
    
    screen_docs = []
    processed_programs = set()
    
    for chunk in all_chunks.values():
        program_id = chunk.get('program_id', 'unknown')
        text = chunk.get('text', '')
        
        if not text:
            continue
            
        # Extract screen definitions
        screens = extract_comprehensive_screen_data(program_id, text, timestamp)
        
        if screens:
            screen_docs.extend(screens)
            processed_programs.add(program_id)
    
    print(f"   📊 Extracted {len(screen_docs)} potential screen definitions")
    print(f"   📊 From {len(processed_programs)} programs")
    
    # Filter new screens
    print("\n🔍 Filtering duplicates and existing screens...")
    
    existing_body = {"search": "*", "top": 1000, "select": "screen_id"}
    response = requests.post(screen_index_url, headers=headers, json=existing_body)
    existing_ids = set()
    
    if response.status_code == 200:
        existing_records = response.json().get('value', [])
        existing_ids = {record['screen_id'] for record in existing_records}
    
    new_screens = []
    for screen in screen_docs:
        if screen['screen_id'] not in existing_ids:
            new_screens.append(screen)
    
    print(f"   📊 New screens to add: {len(new_screens)}")
    
    # Upload new screens
    if new_screens:
        print(f"\n📤 Uploading {len(new_screens)} new screen definitions...")
        upload_screen_docs(search_endpoint, headers, new_screens)
        
        # Final count
        response = requests.post(screen_index_url, headers=headers, json=current_count_body)
        final_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
        
        print(f"\n🎉 EXPANSION COMPLETE!")
        print(f"   Previous count: {current_count}")
        print(f"   New count: {final_count}")
        print(f"   Added: {final_count - current_count} screens")
    else:
        print(f"\n ℹ️  No new screens found to add")


def try_alternative_expansion(search_endpoint: str, headers: Dict, current_count: int):
    """Try alternative expansion using program metadata."""
    print("\n🔄 TRYING ALTERNATIVE EXPANSION APPROACH")
    
    # Try using new_cobol_program_meta or new-cobol-files
    alt_indexes = ["new_cobol_program_meta", "new-cobol-files"]
    
    for index_name in alt_indexes:
        print(f"\n   🔍 Checking {index_name}...")
        index_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Get sample to understand structure
        test_body = {"search": "*", "top": 5, "select": "*"}
        response = requests.post(index_url, headers=headers, json=test_body)
        
        if response.status_code == 200:
            results = response.json().get('value', [])
            if results:
                print(f"      ✅ Found {len(results)} records")
                sample = results[0]
                print(f"      📝 Fields: {list(sample.keys())}")
                
                # Look for file content or source code
                content_field = None
                for field in sample.keys():
                    if any(keyword in field.lower() for keyword in ['content', 'source', 'text', 'code']):
                        content_field = field
                        break
                
                if content_field:
                    print(f"      📄 Using content field: {content_field}")
                    # Continue with extraction logic...
                    return True
        else:
            print(f"      ❌ {index_name} not accessible: {response.status_code}")
    
    print("\n⚠️  Could not find suitable data source for expansion")
    return False


def extract_comprehensive_screen_data(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data using comprehensive analysis."""
    screens = []
    
    # Method 1: SCREEN SECTION
    if SCREEN_SECTION_RE.search(text):
        screens.extend(extract_screen_section_definitions(program_id, text, timestamp))
    
    # Method 2: Field groups with positioning
    screens.extend(extract_positioned_field_groups(program_id, text, timestamp))
    
    # Method 3: ACCEPT/DISPLAY interaction patterns
    screens.extend(extract_ui_interaction_screens(program_id, text, timestamp))
    
    return screens


def extract_screen_section_definitions(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract from SCREEN SECTION."""
    screens = []
    lines = text.split('\n')
    
    in_screen_section = False
    current_screen = None
    fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        if SCREEN_SECTION_RE.search(line_strip):
            in_screen_section = True
            continue
            
        if not in_screen_section:
            continue
            
        # End of section
        if re.match(r'^\s*(DATA|WORKING-STORAGE|PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            break
            
        # Look for 01 level screens
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            if current_screen and fields:
                screen_doc = create_screen_document(program_id, current_screen, fields, timestamp)
                screens.append(screen_doc)
            
            current_screen = level_01_match.group(1)
            fields = []
        
        # Look for field definitions
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_screen:
            fields.append(create_field_info(field_match, i, line_strip))
    
    if current_screen and fields:
        screen_doc = create_screen_document(program_id, current_screen, fields, timestamp)
        screens.append(screen_doc)
    
    return screens


def extract_positioned_field_groups(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract field groups that have positioning information."""
    screens = []
    lines = text.split('\n')
    
    current_group = None
    fields = []
    has_positioning = False
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            # Save previous group if it looks like a screen
            if current_group and fields and has_positioning:
                screen_doc = create_screen_document(program_id, current_group, fields, timestamp)
                screens.append(screen_doc)
            
            current_group = level_01_match.group(1)
            fields = []
            has_positioning = False
        
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_group:
            field_info = create_field_info(field_match, i, line_strip)
            
            # Check for positioning
            if LINE_COL_RE.search(field_match.group(4)):
                has_positioning = True
            
            fields.append(field_info)
    
    if current_group and fields and has_positioning:
        screen_doc = create_screen_document(program_id, current_group, fields, timestamp)
        screens.append(screen_doc)
    
    return screens


def extract_ui_interaction_screens(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screens based on UI interaction patterns."""
    screens = []
    
    # Find ACCEPT/DISPLAY fields
    accept_fields = set(ACCEPT_RE.findall(text))
    display_fields = set(DISPLAY_RE.findall(text))
    
    if accept_fields or display_fields:
        fields = []
        
        for field_name in accept_fields:
            field_info = {
                'level': '05',
                'name': field_name,
                'pic': 'X(20)',
                'type': 'input',
                'raw': f'ACCEPT {field_name}',
                'line': 0
            }
            fields.append(field_info)
        
        for field_name in display_fields:
            field_info = {
                'level': '05',
                'name': field_name,
                'pic': 'X(20)', 
                'type': 'display',
                'raw': f'DISPLAY {field_name}',
                'line': 0
            }
            fields.append(field_info)
        
        if fields:
            screen_name = f"INTERACTIVE_{program_id[-8:].upper()}"
            screen_doc = create_screen_document(program_id, screen_name, fields, timestamp)
            screens.append(screen_doc)
    
    return screens


def create_field_info(field_match, line_num: int, line_text: str) -> Dict:
    """Create field info from regex match."""
    level = field_match.group(1)
    name = field_match.group(2)
    pic = field_match.group(3)
    rest = field_match.group(4)
    
    field_info = {
        'level': level,
        'name': name,
        'pic': pic,
        'line': line_num + 1,
        'attributes': rest.strip(),
        'raw': line_text
    }
    
    # Determine field type
    if VALUE_RE.search(rest):
        field_info['type'] = 'display'
    elif 'ACCEPT' in rest.upper():
        field_info['type'] = 'input'
    else:
        field_info['type'] = 'data'
    
    return field_info


def create_screen_document(program_id: str, screen_name: str, fields: List[Dict], timestamp: str) -> Dict:
    """Create a screen document."""
    
    # Generate unique screen ID
    content = f"{program_id}_{screen_name}_{len(fields)}"
    content_hash = hashlib.md5(content.encode()).hexdigest()[:16]
    screen_id = f"{program_id}_{content_hash}"
    
    # Categorize fields
    input_fields = [f for f in fields if f.get('type') in ['input', 'data']]
    display_fields = [f for f in fields if f.get('type') == 'display']
    
    # Create summary
    summary_text = f"Screen {screen_name}; {len(input_fields)} input fields, {len(display_fields)} display fields"
    
    # Determine screen type
    screen_type = 'form' if input_fields else 'report' if display_fields else 'unknown'
    
    return {
        "screen_id": screen_id,
        "program_id": program_id,
        "screen_name": screen_name,
        "screen_type": screen_type,
        "field_count": len(fields),
        "action_count": 0,
        "transition_count": 0,
        "fields_json": json.dumps(fields),
        "actions_json": json.dumps([]),
        "transitions_json": json.dumps([]),
        "screen_elements_json": json.dumps([]),
        "layout_info_json": json.dumps({}),
        "raw_span_text": "",
        "summary_text": summary_text,
        "generated_at": timestamp,
        "has_vector": False
        # Remove summary_vector field - will be added later by embedding process
    }


def upload_screen_docs(search_endpoint: str, headers: Dict, docs: List[Dict]):
    """Upload screen documents to the index."""
    
    upload_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    batch_size = 100
    total_uploaded = 0
    
    for i in range(0, len(docs), batch_size):
        batch = docs[i:i + batch_size]
        
        upload_body = {
            "value": [{"@search.action": "mergeOrUpload", **doc} for doc in batch]
        }
        
        response = requests.post(upload_url, headers=headers, json=upload_body)
        
        if response.status_code in [200, 201]:
            total_uploaded += len(batch)
            print(f"     ✅ Uploaded batch {i//batch_size + 1}: {len(batch)} documents")
        else:
            print(f"     ❌ Failed to upload batch {i//batch_size + 1}: {response.status_code}")
            print(f"     Error: {response.text[:200]}")
    
    print(f"   📊 Total uploaded: {total_uploaded} screen definitions")


if __name__ == "__main__":
    expand_screen_nodes_v2()