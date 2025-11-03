"""
Recreate new_cobol_screen_nodes index from scratch with correct extraction logic.

This script will:
1. Delete the existing incorrect index
2. Create a new index with improved schema
3. Search for actual COBOL field definitions and UI elements
4. Populate with real screen/form data
"""
import os
import json
import requests
import re
import hashlib
import datetime
from typing import List, Dict, Any
from secrets_loader import load_secrets

# Improved regex patterns for real COBOL field definitions
FIELD_DEF_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)\s+PIC\s+([^.\s]+)\.?\s*(.*)', re.IGNORECASE)
LEVEL_01_RE = re.compile(r'^\s*01\s+([A-Z0-9-]+)', re.IGNORECASE)
SCREEN_VALUE_RE = re.compile(r'VALUE\s+["\']([^"\']*)["\']', re.IGNORECASE)
SCREEN_TO_RE = re.compile(r'TO\s+([A-Z0-9-]+)', re.IGNORECASE)
SCREEN_FROM_RE = re.compile(r'FROM\s+([A-Z0-9-]+)', re.IGNORECASE)

# Look for actual screen-related keywords
SCREEN_KEYWORDS = [
    'SCREEN SECTION', 'LINE', 'COLUMN', 'COL', 'VALUE', 'TO', 'FROM',
    'ACCEPT', 'DISPLAY', 'CURSOR', 'REVERSE-VIDEO', 'HIGHLIGHT',
    'BLINK', 'UNDERLINE', 'BELL', 'BLANK SCREEN', 'ERASE'
]

def recreate_screen_nodes():
    """Recreate the screen nodes index from scratch."""
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        print("❌ Failed to load search configuration")
        return
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    print("🔄 RECREATING NEW_COBOL_SCREEN_NODES INDEX")
    print("=" * 60)
    
    # 1. Delete existing index
    print("\n🗑️  STEP 1: Deleting existing index...")
    delete_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes?api-version=2023-11-01"
    delete_response = requests.delete(delete_url, headers=headers)
    
    if delete_response.status_code in [200, 204]:
        print("   ✅ Existing index deleted successfully")
    elif delete_response.status_code == 404:
        print("   ℹ️  Index didn't exist, proceeding...")
    else:
        print(f"   ⚠️  Delete response: {delete_response.status_code}")
    
    # 2. Create new index with improved schema
    print("\n🏗️  STEP 2: Creating new index...")
    create_new_index(search_endpoint, headers)
    
    # 3. Extract and populate with correct data
    print("\n📊 STEP 3: Extracting real screen/field definitions...")
    extract_and_populate(search_endpoint, headers)
    
    print("\n✅ SCREEN NODES INDEX RECREATION COMPLETE!")


def create_new_index(search_endpoint: str, headers: Dict):
    """Create new index with improved schema."""
    
    index_definition = {
        "name": "new_cobol_screen_nodes",
        "fields": [
            {"name": "screen_id", "type": "Edm.String", "key": True, "searchable": True, "filterable": True},
            {"name": "program_id", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},
            {"name": "screen_name", "type": "Edm.String", "searchable": True, "filterable": True},
            {"name": "screen_type", "type": "Edm.String", "searchable": True, "filterable": True, "facetable": True},  # NEW: data-entry, display, menu, etc.
            {"name": "field_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "action_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "transition_count", "type": "Edm.Int32", "filterable": True, "sortable": True},
            {"name": "fields_json", "type": "Edm.String", "searchable": True},
            {"name": "actions_json", "type": "Edm.String", "searchable": True},
            {"name": "transitions_json", "type": "Edm.String", "searchable": True},
            {"name": "screen_elements_json", "type": "Edm.String", "searchable": True},  # NEW: UI elements like labels, inputs
            {"name": "layout_info_json", "type": "Edm.String", "searchable": True},     # NEW: positioning info
            {"name": "raw_span_text", "type": "Edm.String", "searchable": True},
            {"name": "summary_text", "type": "Edm.String", "searchable": True},
            {"name": "generated_at", "type": "Edm.DateTimeOffset", "filterable": True, "sortable": True},
            {"name": "has_vector", "type": "Edm.Boolean", "filterable": True}
        ],
        "semantic": {
            "configurations": [{
                "name": "semantic-default",
                "prioritizedFields": {
                    "titleField": {"fieldName": "screen_name"},
                    "prioritizedContentFields": [
                        {"fieldName": "summary_text"},
                        {"fieldName": "raw_span_text"}
                    ],
                    "prioritizedKeywordsFields": [
                        {"fieldName": "screen_type"}
                    ]
                }
            }]
        }
    }
    
    create_url = f"{search_endpoint}/indexes?api-version=2023-11-01"
    create_response = requests.post(create_url, headers=headers, json=index_definition)
    
    if create_response.status_code in [200, 201]:
        print("   ✅ New index created successfully")
    else:
        print(f"   ❌ Failed to create index: {create_response.status_code}")
        print(f"   Error: {create_response.text}")
        return False
    
    return True


def extract_and_populate(search_endpoint: str, headers: Dict):
    """Extract real screen/field definitions and populate the index."""
    
    # Strategy: Look for chunks with actual field definitions (PIC clauses)
    # and UI-related keywords rather than false "SCREEN SECTION" matches
    
    chunks_url = f"{search_endpoint}/indexes/new_code_chunks/docs/search?api-version=2023-11-01"
    
    # Multi-step extraction approach
    screen_docs = []
    
    # 1. Find chunks with field definitions (PIC clauses)
    print("   🔍 Finding chunks with field definitions...")
    field_chunks = find_field_definition_chunks(chunks_url, headers)
    print(f"   Found {len(field_chunks)} chunks with field definitions")
    
    # 2. Find chunks with UI keywords
    print("   🔍 Finding chunks with UI keywords...")
    ui_chunks = find_ui_chunks(chunks_url, headers)
    print(f"   Found {len(ui_chunks)} chunks with UI elements")
    
    # 3. Process and extract screen definitions
    print("   📝 Processing chunks...")
    
    processed_programs = set()
    timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + 'Z'
    
    # Process field definition chunks
    for chunk in field_chunks:
        program_id = chunk.get('program_id', '')
        if program_id in processed_programs:
            continue
            
        text = chunk.get('text', '')
        screen_data = extract_screen_data_from_fields(program_id, text, timestamp)
        if screen_data:
            screen_docs.extend(screen_data)
            processed_programs.add(program_id)
    
    # Process UI chunks (may find additional screens)
    for chunk in ui_chunks:
        program_id = chunk.get('program_id', '')
        if program_id in processed_programs:
            continue
            
        text = chunk.get('text', '')
        screen_data = extract_screen_data_from_ui(program_id, text, timestamp)
        if screen_data:
            screen_docs.extend(screen_data)
            processed_programs.add(program_id)
    
    print(f"   📊 Extracted {len(screen_docs)} screen definitions from {len(processed_programs)} programs")
    
    # 4. Upload to index
    if screen_docs:
        print("   📤 Uploading screen definitions...")
        upload_screen_docs(search_endpoint, headers, screen_docs)
    else:
        print("   ⚠️  No valid screen definitions found")


def find_field_definition_chunks(chunks_url: str, headers: Dict) -> List[Dict]:
    """Find chunks containing field definitions with PIC clauses."""
    chunks = []
    
    # Search for PIC clauses that look like field definitions
    search_body = {
        "search": "PIC AND (01 OR 03 OR 05)",
        "top": 1000,
        "queryType": "simple",
        "select": "chunk_id,program_id,text"
    }
    
    response = requests.post(chunks_url, headers=headers, json=search_body)
    
    if response.status_code == 200:
        data = response.json()
        chunks = data.get('value', [])
    
    return chunks


def find_ui_chunks(chunks_url: str, headers: Dict) -> List[Dict]:
    """Find chunks containing UI-related keywords."""
    chunks = []
    
    # Search for UI-related terms
    ui_terms = ["ACCEPT", "DISPLAY", "CURSOR", "LINE", "COLUMN"]
    search_query = " OR ".join(ui_terms)
    
    search_body = {
        "search": search_query,
        "top": 500,
        "queryType": "simple",
        "select": "chunk_id,program_id,text"
    }
    
    response = requests.post(chunks_url, headers=headers, json=search_body)
    
    if response.status_code == 200:
        data = response.json()
        chunks = data.get('value', [])
    
    return chunks


def extract_screen_data_from_fields(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data from field definition chunks."""
    screens = []
    
    lines = text.split('\n')
    current_screen = None
    fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        # Look for 01-level record definitions (potential screens)
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            # Save previous screen if it had fields
            if current_screen and fields:
                screen_doc = create_screen_document(
                    program_id, current_screen, fields, [], [], text, timestamp
                )
                screens.append(screen_doc)
            
            # Start new screen
            current_screen = level_01_match.group(1)
            fields = []
        
        # Look for field definitions
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_screen:
            level = field_match.group(1)
            name = field_match.group(2)
            pic = field_match.group(3)
            rest = field_match.group(4)
            
            field_info = {
                'level': level,
                'name': name,
                'pic': pic,
                'line': i + 1,
                'attributes': rest.strip(),
                'raw': line_strip
            }
            
            # Check for VALUE clauses (display fields)
            value_match = SCREEN_VALUE_RE.search(rest)
            if value_match:
                field_info['value'] = value_match.group(1)
                field_info['type'] = 'display'
            else:
                field_info['type'] = 'data'
            
            fields.append(field_info)
    
    # Don't forget the last screen
    if current_screen and fields:
        screen_doc = create_screen_document(
            program_id, current_screen, fields, [], [], text, timestamp
        )
        screens.append(screen_doc)
    
    return screens


def extract_screen_data_from_ui(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data from UI-related chunks."""
    screens = []
    
    # Look for ACCEPT/DISPLAY statements that suggest screen interaction
    lines = text.split('\n')
    ui_elements = []
    actions = []
    
    for i, line in enumerate(lines):
        line_upper = line.upper()
        
        if 'ACCEPT' in line_upper:
            actions.append('ACCEPT')
            # Try to extract field name
            parts = line.split()
            for j, part in enumerate(parts):
                if part.upper() == 'ACCEPT' and j + 1 < len(parts):
                    field_name = parts[j + 1].strip('.,')
                    ui_elements.append({
                        'type': 'input',
                        'name': field_name,
                        'line': i + 1,
                        'statement': line.strip()
                    })
                    break
        
        if 'DISPLAY' in line_upper:
            actions.append('DISPLAY')
            # Try to extract display text
            if '"' in line or "'" in line:
                ui_elements.append({
                    'type': 'output',
                    'line': i + 1,
                    'statement': line.strip()
                })
    
    # If we found UI elements, create a screen document
    if ui_elements or actions:
        screen_name = f"{program_id}_UI_SCREEN"
        screen_doc = create_screen_document(
            program_id, screen_name, [], actions, ui_elements, text, timestamp
        )
        screens.append(screen_doc)
    
    return screens


def create_screen_document(program_id: str, screen_name: str, fields: List[Dict], 
                         actions: List[str], ui_elements: List[Dict], 
                         raw_text: str, timestamp: str) -> Dict:
    """Create a screen document for the index."""
    
    # Generate unique screen ID
    content_hash = hashlib.sha256(f"{program_id}_{screen_name}_{len(fields)}".encode()).hexdigest()[:16]
    screen_id = f"{program_id}_{content_hash}"
    
    # Determine screen type
    screen_type = determine_screen_type(fields, actions, ui_elements)
    
    # Create summary
    summary_parts = [f"Screen {screen_name}"]
    
    if fields:
        input_fields = [f for f in fields if f.get('type') == 'data']
        display_fields = [f for f in fields if f.get('type') == 'display']
        summary_parts.append(f"{len(input_fields)} input fields, {len(display_fields)} display fields")
    
    if actions:
        unique_actions = list(set(actions))
        summary_parts.append(f"actions: {', '.join(unique_actions[:3])}")
    
    if ui_elements:
        summary_parts.append(f"{len(ui_elements)} UI elements")
    
    summary_text = "; ".join(summary_parts)
    
    return {
        "screen_id": screen_id,
        "program_id": program_id,
        "screen_name": screen_name,
        "screen_type": screen_type,
        "field_count": len(fields),
        "action_count": len(set(actions)),
        "transition_count": 0,  # Will be populated later if needed
        "fields_json": json.dumps(fields),
        "actions_json": json.dumps(list(set(actions))),
        "transitions_json": json.dumps([]),
        "screen_elements_json": json.dumps(ui_elements),
        "layout_info_json": json.dumps({}),
        "raw_span_text": raw_text[:2000],  # First 2000 chars
        "summary_text": summary_text,
        "generated_at": timestamp,
        "has_vector": False
    }


def determine_screen_type(fields: List[Dict], actions: List[str], ui_elements: List[Dict]) -> str:
    """Determine the type of screen based on its content."""
    
    if 'ACCEPT' in actions:
        return 'data-entry'
    elif 'DISPLAY' in actions:
        return 'display'
    elif fields:
        input_fields = [f for f in fields if f.get('type') == 'data']
        if input_fields:
            return 'form'
        else:
            return 'report'
    elif ui_elements:
        return 'interactive'
    else:
        return 'unknown'


def upload_screen_docs(search_endpoint: str, headers: Dict, docs: List[Dict]):
    """Upload screen documents to the index."""
    
    upload_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    # Upload in batches of 100
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
    recreate_screen_nodes()