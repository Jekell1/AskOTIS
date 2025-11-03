#!/usr/bin/env python3
"""
Expand Screen Nodes Dataset - Find More COBOL Screen Definitions

This script will search for additional COBOL screen definitions using:
1. Broader search patterns for field definitions
2. SCREEN SECTION searches
3. ACCEPT/DISPLAY statement patterns
4. Form/report structures
5. UI interaction patterns
"""

import os
import json
import requests
import re
import hashlib
import datetime
from typing import List, Dict, Any
from secrets_loader import load_secrets

# Expanded regex patterns for COBOL screen elements
FIELD_DEF_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)\s+PIC\s+([^.\s]+)\.?\s*(.*)', re.IGNORECASE)
LEVEL_01_RE = re.compile(r'^\s*01\s+([A-Z0-9-]+)', re.IGNORECASE)
LEVEL_05_RE = re.compile(r'^\s*05\s+([A-Z0-9-]+)', re.IGNORECASE)
LEVEL_03_RE = re.compile(r'^\s*03\s+([A-Z0-9-]+)', re.IGNORECASE)

# Screen section patterns
SCREEN_SECTION_RE = re.compile(r'SCREEN\s+SECTION', re.IGNORECASE)
SCREEN_DEF_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+PIC\s+([^.\s]+))?\s*(.*)', re.IGNORECASE)

# UI interaction patterns
ACCEPT_RE = re.compile(r'ACCEPT\s+([A-Z0-9-]+)', re.IGNORECASE)
DISPLAY_RE = re.compile(r'DISPLAY\s+([A-Z0-9-]+)', re.IGNORECASE)
MOVE_TO_RE = re.compile(r'MOVE\s+.*\s+TO\s+([A-Z0-9-]+)', re.IGNORECASE)

# Screen positioning and attributes
LINE_COL_RE = re.compile(r'(LINE|COLUMN|COL)\s+(\d+)', re.IGNORECASE)
VALUE_RE = re.compile(r'VALUE\s+["\']([^"\']*)["\']', re.IGNORECASE)
ATTR_RE = re.compile(r'(REVERSE-VIDEO|HIGHLIGHT|BLINK|UNDERLINE|BELL)', re.IGNORECASE)

def expand_screen_nodes():
    """Expand the screen nodes dataset with more comprehensive search."""
    
    print("🔍 EXPANDING SCREEN NODES DATASET")
    print("=" * 50)
    
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
    
    chunks_url = f"{search_endpoint}/indexes/cobol_chunks/docs/search?api-version=2023-11-01"
    
    # Step 1: Get current screen count
    screen_index_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    current_count_body = {"search": "*", "top": 0, "count": True}
    response = requests.post(screen_index_url, headers=headers, json=current_count_body)
    current_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    print(f"📊 Current screen nodes: {current_count}")
    print("\n🔍 Searching for additional screen definitions...")
    
    # Step 2: Search strategies
    search_strategies = [
        {
            "name": "SCREEN SECTION chunks",
            "query": "SCREEN SECTION",
            "queryType": "simple"
        },
        {
            "name": "ACCEPT/DISPLAY patterns",
            "query": "ACCEPT OR DISPLAY",
            "queryType": "simple"
        },
        {
            "name": "Form field patterns",
            "query": "(01 OR 03 OR 05) AND PIC AND (LINE OR COLUMN)",
            "queryType": "simple"
        },
        {
            "name": "Data entry patterns", 
            "query": "PIC AND (ACCEPT OR VALUE)",
            "queryType": "simple"
        },
        {
            "name": "Screen positioning",
            "query": "(LINE AND COLUMN) OR (ROW AND COL)",
            "queryType": "simple"
        }
    ]
    
    all_chunks = {}  # Use dict to avoid duplicates
    timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + 'Z'
    
    for strategy in search_strategies:
        print(f"\n   🔎 Strategy: {strategy['name']}")
        
        search_body = {
            "search": strategy["query"],
            "top": 1000,
            "queryType": strategy["queryType"],
            "select": "chunk_id,program_id,text"
        }
        
        response = requests.post(chunks_url, headers=headers, json=search_body)
        
        if response.status_code == 200:
            chunks = response.json().get('value', [])
            print(f"      Found: {len(chunks)} chunks")
            
            for chunk in chunks:
                chunk_id = chunk.get('chunk_id')
                if chunk_id not in all_chunks:
                    all_chunks[chunk_id] = chunk
        else:
            print(f"      ❌ Error: {response.status_code}")
    
    print(f"\n📦 Total unique chunks to process: {len(all_chunks)}")
    
    # Step 3: Process chunks to extract screen definitions
    print("\n🔄 Processing chunks for screen definitions...")
    
    screen_docs = []
    processed_programs = set()
    
    for chunk in all_chunks.values():
        program_id = chunk.get('program_id', '')
        text = chunk.get('text', '')
        
        # Extract screen definitions using multiple approaches
        screens = []
        
        # Approach 1: Look for SCREEN SECTION definitions
        screens.extend(extract_screen_section_data(program_id, text, timestamp))
        
        # Approach 2: Look for field definition groups
        screens.extend(extract_field_group_data(program_id, text, timestamp))
        
        # Approach 3: Look for ACCEPT/DISPLAY patterns
        screens.extend(extract_ui_interaction_data(program_id, text, timestamp))
        
        if screens:
            screen_docs.extend(screens)
            processed_programs.add(program_id)
    
    print(f"   📊 Extracted {len(screen_docs)} potential screen definitions")
    print(f"   📊 From {len(processed_programs)} programs")
    
    # Step 4: Filter out duplicates and existing screens
    print("\n🔍 Filtering duplicates and existing screens...")
    
    # Get existing screen IDs
    existing_body = {"search": "*", "top": 1000, "select": "screen_id"}
    response = requests.post(screen_index_url, headers=headers, json=existing_body)
    existing_ids = set()
    
    if response.status_code == 200:
        existing_records = response.json().get('value', [])
        existing_ids = {record['screen_id'] for record in existing_records}
    
    # Filter new screens
    new_screens = []
    for screen in screen_docs:
        if screen['screen_id'] not in existing_ids:
            new_screens.append(screen)
    
    print(f"   📊 New screens to add: {len(new_screens)}")
    
    # Step 5: Upload new screens
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


def extract_screen_section_data(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data from SCREEN SECTION definitions."""
    screens = []
    lines = text.split('\n')
    
    in_screen_section = False
    current_screen = None
    fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        # Check for SCREEN SECTION
        if SCREEN_SECTION_RE.search(line_strip):
            in_screen_section = True
            continue
            
        if not in_screen_section:
            continue
            
        # Check for end of section
        if re.match(r'^\s*(DATA|WORKING-STORAGE|PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            break
            
        # Look for screen definitions
        screen_match = SCREEN_DEF_RE.match(line_strip)
        if screen_match:
            level = screen_match.group(1)
            name = screen_match.group(2)
            pic = screen_match.group(3) or ''
            rest = screen_match.group(4) or ''
            
            if level == '01':
                # Save previous screen
                if current_screen and fields:
                    screen_doc = create_screen_document(
                        program_id, current_screen, fields, [], [], text, timestamp
                    )
                    screens.append(screen_doc)
                
                # Start new screen
                current_screen = name
                fields = []
            else:
                # Add field to current screen
                if current_screen:
                    field_info = {
                        'level': level,
                        'name': name,
                        'pic': pic,
                        'line': i + 1,
                        'attributes': rest.strip(),
                        'raw': line_strip
                    }
                    
                    # Determine field type
                    if VALUE_RE.search(rest):
                        field_info['type'] = 'display'
                    elif 'ACCEPT' in rest.upper():
                        field_info['type'] = 'input'
                    else:
                        field_info['type'] = 'data'
                    
                    fields.append(field_info)
    
    # Don't forget last screen
    if current_screen and fields:
        screen_doc = create_screen_document(
            program_id, current_screen, fields, [], [], text, timestamp
        )
        screens.append(screen_doc)
    
    return screens


def extract_field_group_data(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data from groups of field definitions."""
    screens = []
    lines = text.split('\n')
    
    current_group = None
    fields = []
    has_positioning = False
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        # Look for 01-level start
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            # Save previous group if it looks like a screen
            if current_group and fields and (has_positioning or len(fields) >= 2):
                screen_doc = create_screen_document(
                    program_id, current_group, fields, [], [], text, timestamp
                )
                screens.append(screen_doc)
            
            # Start new group
            current_group = level_01_match.group(1)
            fields = []
            has_positioning = False
        
        # Look for field definitions within the group
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_group:
            level = field_match.group(1)
            name = field_match.group(2)
            pic = field_match.group(3)
            rest = field_match.group(4)
            
            # Check for screen-like attributes
            if LINE_COL_RE.search(rest) or ATTR_RE.search(rest):
                has_positioning = True
            
            field_info = {
                'level': level,
                'name': name,
                'pic': pic,
                'line': i + 1,
                'attributes': rest.strip(),
                'raw': line_strip
            }
            
            # Determine field type
            if VALUE_RE.search(rest):
                field_info['type'] = 'display'
            elif ACCEPT_RE.search(text[max(0, i-5):i+5]):  # Look around for ACCEPT
                field_info['type'] = 'input'
            else:
                field_info['type'] = 'data'
            
            fields.append(field_info)
    
    # Check last group
    if current_group and fields and (has_positioning or len(fields) >= 2):
        screen_doc = create_screen_document(
            program_id, current_group, fields, [], [], text, timestamp
        )
        screens.append(screen_doc)
    
    return screens


def extract_ui_interaction_data(program_id: str, text: str, timestamp: str) -> List[Dict]:
    """Extract screen data from UI interaction patterns."""
    screens = []
    lines = text.split('\n')
    
    # Look for ACCEPT/DISPLAY patterns with field names
    accept_fields = set()
    display_fields = set()
    
    for line in lines:
        # Find ACCEPT statements
        accept_matches = ACCEPT_RE.findall(line)
        accept_fields.update(accept_matches)
        
        # Find DISPLAY statements
        display_matches = DISPLAY_RE.findall(line)
        display_fields.update(display_matches)
    
    # If we have interactive fields, create a screen definition
    if accept_fields or display_fields:
        all_fields = []
        
        # Create field entries for ACCEPT fields
        for field_name in accept_fields:
            field_info = {
                'level': '05',
                'name': field_name,
                'pic': 'X(20)',  # Default PIC
                'line': 0,
                'attributes': 'ACCEPT field',
                'raw': f'ACCEPT {field_name}',
                'type': 'input'
            }
            all_fields.append(field_info)
        
        # Create field entries for DISPLAY fields
        for field_name in display_fields:
            field_info = {
                'level': '05', 
                'name': field_name,
                'pic': 'X(20)',  # Default PIC
                'line': 0,
                'attributes': 'DISPLAY field',
                'raw': f'DISPLAY {field_name}',
                'type': 'display'
            }
            all_fields.append(field_info)
        
        if all_fields:
            # Create a screen name based on program
            screen_name = f"UI_SCREEN_{program_id[-8:].upper()}"
            
            screen_doc = create_screen_document(
                program_id, screen_name, all_fields, [], [], text, timestamp
            )
            screens.append(screen_doc)
    
    return screens


def create_screen_document(program_id: str, screen_name: str, fields: List[Dict], 
                          actions: List[Dict], transitions: List[Dict], 
                          raw_text: str, timestamp: str) -> Dict:
    """Create a screen document."""
    
    # Generate unique screen ID
    content = f"{program_id}_{screen_name}_{len(fields)}"
    content_hash = hashlib.md5(content.encode()).hexdigest()[:16]
    screen_id = f"{program_id}_{content_hash}"
    
    # Categorize fields
    input_fields = [f for f in fields if f.get('type') == 'input' or f.get('type') == 'data']
    display_fields = [f for f in fields if f.get('type') == 'display']
    
    # Create summary
    summary_parts = [f"Screen {screen_name}"]
    summary_parts.append(f"{len(input_fields)} input fields, {len(display_fields)} display fields")
    
    # Determine screen type
    if len(input_fields) > 0:
        screen_type = 'form'
    elif len(display_fields) > 0:
        screen_type = 'report'
    else:
        screen_type = 'unknown'
    
    summary_text = "; ".join(summary_parts)
    
    return {
        "screen_id": screen_id,
        "program_id": program_id,
        "screen_name": screen_name,
        "screen_type": screen_type,
        "field_count": len(fields),
        "action_count": len(actions),
        "transition_count": len(transitions),
        "fields_json": json.dumps(fields),
        "actions_json": json.dumps(actions),
        "transitions_json": json.dumps(transitions),
        "screen_elements_json": json.dumps([]),
        "layout_info_json": json.dumps({}),
        "raw_span_text": raw_text[:2000],  # Limit size
        "summary_text": summary_text,
        "generated_at": timestamp,
        "has_vector": False,
        "summary_vector": None
    }


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
    expand_screen_nodes()