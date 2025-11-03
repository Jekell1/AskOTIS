#!/usr/bin/env python3
"""
Comprehensive Screen Generation from All CBL and CPY Source Files
Target: ~9,500 screen definitions from complete COBOL source codebase
"""

import os
import json
import requests
import re
import hashlib
import datetime
from typing import List, Dict, Any, Set
from secrets_loader import load_secrets

# Enhanced regex patterns for comprehensive COBOL screen detection
FIELD_DEF_RE = re.compile(r'^\s*(\d{2})\s+([A-Z0-9-]+)\s+(?:PIC\s+([^.\s]+))?\.?\s*(.*)', re.IGNORECASE)
LEVEL_01_RE = re.compile(r'^\s*01\s+([A-Z0-9-]+)', re.IGNORECASE)
LEVEL_05_RE = re.compile(r'^\s*05\s+([A-Z0-9-]+)', re.IGNORECASE)
LEVEL_03_RE = re.compile(r'^\s*03\s+([A-Z0-9-]+)', re.IGNORECASE)
LEVEL_77_RE = re.compile(r'^\s*77\s+([A-Z0-9-]+)', re.IGNORECASE)

# Screen section and copybook patterns
SCREEN_SECTION_RE = re.compile(r'SCREEN\s+SECTION', re.IGNORECASE)
COPY_RE = re.compile(r'COPY\s+([A-Z0-9-]+)', re.IGNORECASE)
INCLUDE_RE = re.compile(r'INCLUDE\s+([A-Z0-9-]+)', re.IGNORECASE)

# UI interaction patterns - expanded
ACCEPT_RE = re.compile(r'ACCEPT\s+([A-Z0-9-]+)', re.IGNORECASE)
DISPLAY_RE = re.compile(r'DISPLAY\s+([A-Z0-9-\s,\'\"]+)', re.IGNORECASE)
MOVE_TO_RE = re.compile(r'MOVE\s+.*\s+TO\s+([A-Z0-9-]+)', re.IGNORECASE)

# Screen positioning and formatting
LINE_COL_RE = re.compile(r'(LINE|COLUMN|COL|ROW)\s+(\d+|PLUS\s+\d+)', re.IGNORECASE)
VALUE_RE = re.compile(r'VALUE\s+(?:["\']([^"\']*)["\']|([A-Z0-9-]+))', re.IGNORECASE)
ATTR_RE = re.compile(r'(REVERSE-VIDEO|HIGHLIGHT|BLINK|UNDERLINE|BELL|BLANK\s+SCREEN|ERASE)', re.IGNORECASE)

# Data definition patterns
PIC_RE = re.compile(r'PIC\s+([X9SAV\(\)\+\-\.\,]+)', re.IGNORECASE)
USAGE_RE = re.compile(r'USAGE\s+(BINARY|COMP|COMP-3|DISPLAY|PACKED-DECIMAL)', re.IGNORECASE)

# Working storage and linkage patterns
WORKING_STORAGE_RE = re.compile(r'WORKING-STORAGE\s+SECTION', re.IGNORECASE)
LINKAGE_RE = re.compile(r'LINKAGE\s+SECTION', re.IGNORECASE)
FILE_SECTION_RE = re.compile(r'FILE\s+SECTION', re.IGNORECASE)

def generate_screens_from_all_sources():
    """Generate screen definitions from all CBL and CPY source files."""
    
    print("🔍 COMPREHENSIVE SCREEN GENERATION FROM ALL SOURCE FILES")
    print("=" * 70)
    print("Target: ~9,500 screen definitions from complete COBOL codebase")
    print()
    
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
    
    # Step 1: Get all source files from available file indexes
    print("\n🔍 Step 1: Locating all COBOL source files...")
    
    all_files = get_all_cobol_source_files(search_endpoint, headers)
    print(f"   📁 Found {len(all_files)} COBOL source files")
    
    # Step 2: Process files in batches to extract screen definitions
    print(f"\n🔄 Step 2: Processing source files for screen definitions...")
    
    timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + 'Z'
    all_screens = []
    processed_files = 0
    
    batch_size = 50  # Process files in batches
    
    for i in range(0, len(all_files), batch_size):
        batch_files = all_files[i:i + batch_size]
        print(f"\n   📦 Processing batch {i//batch_size + 1}/{(len(all_files) + batch_size - 1)//batch_size}")
        print(f"      Files: {i+1}-{min(i+batch_size, len(all_files))} of {len(all_files)}")
        
        batch_screens = process_file_batch(batch_files, search_endpoint, headers, timestamp)
        all_screens.extend(batch_screens)
        processed_files += len(batch_files)
        
        print(f"      ✅ Batch complete: {len(batch_screens)} screens found")
        print(f"      📊 Running total: {len(all_screens)} screens from {processed_files} files")
    
    print(f"\n📊 EXTRACTION COMPLETE:")
    print(f"   Processed files: {processed_files}")
    print(f"   Total screens found: {len(all_screens)}")
    
    # Step 3: Filter duplicates and existing screens
    print(f"\n🔍 Step 3: Filtering duplicates and existing screens...")
    
    # Get existing screen IDs
    existing_ids = get_existing_screen_ids(screen_index_url, headers)
    print(f"   📋 Existing screens in index: {len(existing_ids)}")
    
    # Remove duplicates by screen_id
    unique_screens = {}
    for screen in all_screens:
        screen_id = screen['screen_id']
        if screen_id not in existing_ids and screen_id not in unique_screens:
            unique_screens[screen_id] = screen
    
    new_screens = list(unique_screens.values())
    print(f"   📊 New unique screens to add: {len(new_screens)}")
    
    # Step 4: Upload new screens in batches
    if new_screens:
        print(f"\n📤 Step 4: Uploading {len(new_screens)} new screen definitions...")
        upload_success = upload_screen_docs_robust(search_endpoint, headers, new_screens)
        
        if upload_success:
            # Final count
            response = requests.post(screen_index_url, headers=headers, json=current_count_body)
            final_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
            
            print(f"\n🎉 COMPREHENSIVE GENERATION COMPLETE!")
            print(f"   Previous count: {current_count}")
            print(f"   New count: {final_count}")
            print(f"   Added: {final_count - current_count} screens")
            print(f"   Target achievement: {(final_count/9500)*100:.1f}% of 9,500 target")
        else:
            print(f"\n❌ Upload failed - screens not added to index")
    else:
        print(f"\n ℹ️  No new screens found to add")


def get_all_cobol_source_files(search_endpoint: str, headers: Dict) -> List[Dict]:
    """Get all COBOL source files from available indexes."""
    
    all_files = []
    
    # Try different file indexes
    file_indexes = [
        "new-cobol-files",
        "new_cobol_program_meta",
        "new_cobol_program_inventory"
    ]
    
    for index_name in file_indexes:
        print(f"   🔍 Checking index: {index_name}")
        index_url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        
        # Test index accessibility
        test_body = {"search": "*", "top": 1, "select": "*"}
        test_response = requests.post(index_url, headers=headers, json=test_body)
        
        if test_response.status_code != 200:
            print(f"      ❌ Index not accessible: {test_response.status_code}")
            continue
            
        # Get total count
        count_body = {"search": "*", "top": 0, "count": True}
        count_response = requests.post(index_url, headers=headers, json=count_body)
        total_count = count_response.json().get('@odata.count', 0) if count_response.status_code == 200 else 0
        
        print(f"      ✅ Found {total_count} records")
        
        if total_count == 0:
            continue
            
        # Get sample to understand structure
        sample_data = test_response.json().get('value', [])
        if not sample_data:
            continue
            
        sample_record = sample_data[0]
        print(f"      📝 Sample fields: {list(sample_record.keys())}")
        
        # Determine relevant fields
        content_field = find_content_field(sample_record)
        file_path_field = find_file_path_field(sample_record)
        program_id_field = find_program_id_field(sample_record)
        
        if not content_field:
            print(f"      ⚠️ No content field found")
            continue
            
        print(f"      📄 Using: content='{content_field}', path='{file_path_field}', program_id='{program_id_field}'")
        
        # Get all files from this index
        files_from_index = get_files_from_index(index_url, headers, content_field, file_path_field, program_id_field, total_count)
        all_files.extend(files_from_index)
        
        print(f"      ✅ Retrieved {len(files_from_index)} files")
    
    # Remove duplicates by file path or program ID
    unique_files = {}
    for file_info in all_files:
        key = file_info.get('file_path') or file_info.get('program_id') or file_info.get('id', 'unknown')
        if key not in unique_files:
            unique_files[key] = file_info
    
    return list(unique_files.values())


def find_content_field(record: Dict) -> str:
    """Find the field containing source code content."""
    content_fields = ['source_code', 'content', 'text', 'file_content', 'code', 'source', 'body']
    for field in content_fields:
        if field in record:
            return field
    return None


def find_file_path_field(record: Dict) -> str:
    """Find the field containing file path."""
    path_fields = ['file_path', 'path', 'file_name', 'name', 'filename']
    for field in path_fields:
        if field in record:
            return field
    return None


def find_program_id_field(record: Dict) -> str:
    """Find the field containing program ID."""
    program_fields = ['program_id', 'id', 'program_name', 'file_id']
    for field in program_fields:
        if field in record:
            return field
    return None


def get_files_from_index(index_url: str, headers: Dict, content_field: str, 
                        file_path_field: str, program_id_field: str, total_count: int) -> List[Dict]:
    """Get all files from a specific index."""
    
    files = []
    batch_size = 1000  # Azure Search max
    
    for skip in range(0, total_count, batch_size):
        select_fields = [content_field]
        if file_path_field:
            select_fields.append(file_path_field)
        if program_id_field:
            select_fields.append(program_id_field)
        
        body = {
            "search": "*",
            "top": min(batch_size, total_count - skip),
            "skip": skip,
            "select": ",".join(select_fields)
        }
        
        response = requests.post(index_url, headers=headers, json=body)
        
        if response.status_code == 200:
            batch_files = response.json().get('value', [])
            
            # Normalize file info
            for file_data in batch_files:
                file_info = {
                    'content': file_data.get(content_field, ''),
                    'file_path': file_data.get(file_path_field, 'unknown'),
                    'program_id': file_data.get(program_id_field, 'unknown'),
                    'source_index': index_url.split('/')[-3]  # Extract index name
                }
                files.append(file_info)
        else:
            print(f"      ❌ Error fetching batch at skip {skip}: {response.status_code}")
            break
    
    return files


def process_file_batch(files: List[Dict], search_endpoint: str, headers: Dict, timestamp: str) -> List[Dict]:
    """Process a batch of files to extract screen definitions."""
    
    batch_screens = []
    
    for file_info in files:
        content = file_info.get('content', '')
        program_id = file_info.get('program_id', 'unknown')
        file_path = file_info.get('file_path', 'unknown')
        
        if not content:
            continue
            
        # Extract screens using comprehensive methods
        file_screens = extract_all_screen_types(program_id, content, file_path, timestamp)
        batch_screens.extend(file_screens)
    
    return batch_screens


def extract_all_screen_types(program_id: str, content: str, file_path: str, timestamp: str) -> List[Dict]:
    """Extract all types of screen definitions from source content."""
    
    screens = []
    
    # Method 1: SCREEN SECTION definitions
    screens.extend(extract_screen_section_complete(program_id, content, timestamp))
    
    # Method 2: Working Storage field groups
    screens.extend(extract_working_storage_screens(program_id, content, timestamp))
    
    # Method 3: Linkage Section definitions
    screens.extend(extract_linkage_screens(program_id, content, timestamp))
    
    # Method 4: File Section record definitions
    screens.extend(extract_file_section_screens(program_id, content, timestamp))
    
    # Method 5: ACCEPT/DISPLAY interaction patterns
    screens.extend(extract_interaction_screens(program_id, content, timestamp))
    
    # Method 6: COPY/INCLUDE definitions
    screens.extend(extract_copybook_screens(program_id, content, timestamp))
    
    # Method 7: 77-level standalone fields
    screens.extend(extract_standalone_field_screens(program_id, content, timestamp))
    
    return screens


def extract_screen_section_complete(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract comprehensive SCREEN SECTION definitions."""
    
    screens = []
    lines = content.split('\n')
    
    in_screen_section = False
    current_screen = None
    fields = []
    screen_attributes = {}
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        if SCREEN_SECTION_RE.search(line_strip):
            in_screen_section = True
            continue
            
        if not in_screen_section:
            continue
            
        # Check for end of SCREEN SECTION
        if re.search(r'^\s*(WORKING-STORAGE|LINKAGE|FILE|PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            if current_screen and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_screen, fields, screen_attributes, timestamp, 'screen_section'
                )
                screens.append(screen_doc)
            break
            
        # Look for screen definitions (01 level)
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            # Save previous screen
            if current_screen and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_screen, fields, screen_attributes, timestamp, 'screen_section'
                )
                screens.append(screen_doc)
            
            # Start new screen
            current_screen = level_01_match.group(1)
            fields = []
            screen_attributes = {}
            
            # Look for screen-level attributes
            if LINE_COL_RE.search(line_strip):
                screen_attributes['positioning'] = True
            if ATTR_RE.search(line_strip):
                screen_attributes['formatting'] = True
        
        # Look for field definitions within screen
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_screen:
            field_info = create_enhanced_field_info(field_match, i, line_strip, content, i)
            fields.append(field_info)
    
    # Don't forget the last screen
    if current_screen and fields:
        screen_doc = create_enhanced_screen_document(
            program_id, current_screen, fields, screen_attributes, timestamp, 'screen_section'
        )
        screens.append(screen_doc)
    
    return screens


def extract_working_storage_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screen definitions from WORKING-STORAGE SECTION."""
    
    screens = []
    lines = content.split('\n')
    
    in_working_storage = False
    current_group = None
    fields = []
    group_has_screen_attributes = False
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        if WORKING_STORAGE_RE.search(line_strip):
            in_working_storage = True
            continue
            
        if not in_working_storage:
            continue
            
        # Check for end of WORKING-STORAGE
        if re.search(r'^\s*(LINKAGE|FILE|PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            if current_group and fields and group_has_screen_attributes:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_group, fields, {'section': 'working_storage'}, timestamp, 'working_storage'
                )
                screens.append(screen_doc)
            break
            
        # Look for 01 level groups
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            # Save previous group if it looks like a screen
            if current_group and fields and group_has_screen_attributes:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_group, fields, {'section': 'working_storage'}, timestamp, 'working_storage'
                )
                screens.append(screen_doc)
            
            # Start new group
            current_group = level_01_match.group(1)
            fields = []
            group_has_screen_attributes = False
            
            # Check if this looks like a screen group
            if any(keyword in line_strip.upper() for keyword in ['SCREEN', 'DISPLAY', 'FORM', 'MENU', 'PANEL']):
                group_has_screen_attributes = True
        
        # Look for field definitions
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_group:
            field_info = create_enhanced_field_info(field_match, i, line_strip, content, i)
            fields.append(field_info)
            
            # Check for screen-like attributes
            if (LINE_COL_RE.search(line_strip) or ATTR_RE.search(line_strip) or 
                VALUE_RE.search(line_strip) or len(fields) >= 3):
                group_has_screen_attributes = True
    
    # Check last group
    if current_group and fields and group_has_screen_attributes:
        screen_doc = create_enhanced_screen_document(
            program_id, current_group, fields, {'section': 'working_storage'}, timestamp, 'working_storage'
        )
        screens.append(screen_doc)
    
    return screens


def extract_linkage_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screen definitions from LINKAGE SECTION."""
    
    screens = []
    lines = content.split('\n')
    
    in_linkage = False
    current_group = None
    fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        if LINKAGE_RE.search(line_strip):
            in_linkage = True
            continue
            
        if not in_linkage:
            continue
            
        # End of linkage section
        if re.search(r'^\s*(PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            if current_group and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_group, fields, {'section': 'linkage'}, timestamp, 'linkage'
                )
                screens.append(screen_doc)
            break
            
        # 01 level groups
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            if current_group and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_group, fields, {'section': 'linkage'}, timestamp, 'linkage'
                )
                screens.append(screen_doc)
            
            current_group = level_01_match.group(1)
            fields = []
        
        # Field definitions
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_group:
            field_info = create_enhanced_field_info(field_match, i, line_strip, content, i)
            fields.append(field_info)
    
    # Last group
    if current_group and fields:
        screen_doc = create_enhanced_screen_document(
            program_id, current_group, fields, {'section': 'linkage'}, timestamp, 'linkage'
        )
        screens.append(screen_doc)
    
    return screens


def extract_file_section_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screen definitions from FILE SECTION."""
    
    screens = []
    lines = content.split('\n')
    
    in_file_section = False
    current_record = None
    fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        if FILE_SECTION_RE.search(line_strip):
            in_file_section = True
            continue
            
        if not in_file_section:
            continue
            
        # End of file section
        if re.search(r'^\s*(WORKING-STORAGE|LINKAGE|PROCEDURE)\s+SECTION', line_strip, re.IGNORECASE):
            if current_record and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_record, fields, {'section': 'file'}, timestamp, 'file_record'
                )
                screens.append(screen_doc)
            break
            
        # 01 level records
        level_01_match = LEVEL_01_RE.match(line_strip)
        if level_01_match:
            if current_record and fields:
                screen_doc = create_enhanced_screen_document(
                    program_id, current_record, fields, {'section': 'file'}, timestamp, 'file_record'
                )
                screens.append(screen_doc)
            
            current_record = level_01_match.group(1)
            fields = []
        
        # Field definitions
        field_match = FIELD_DEF_RE.match(line_strip)
        if field_match and current_record:
            field_info = create_enhanced_field_info(field_match, i, line_strip, content, i)
            fields.append(field_info)
    
    # Last record
    if current_record and fields:
        screen_doc = create_enhanced_screen_document(
            program_id, current_record, fields, {'section': 'file'}, timestamp, 'file_record'
        )
        screens.append(screen_doc)
    
    return screens


def extract_interaction_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screens based on ACCEPT/DISPLAY interaction patterns."""
    
    screens = []
    
    # Find all ACCEPT and DISPLAY statements
    accept_fields = set(ACCEPT_RE.findall(content))
    display_matches = DISPLAY_RE.findall(content)
    
    # Process DISPLAY matches more carefully
    display_fields = set()
    for match in display_matches:
        # Extract field names from DISPLAY statements
        words = re.findall(r'[A-Z0-9-]+', match)
        display_fields.update(words)
    
    if accept_fields or display_fields:
        fields = []
        
        # Create field entries for ACCEPT fields
        for field_name in accept_fields:
            if len(field_name) > 1 and field_name not in ['FROM', 'TO', 'WITH']:  # Filter noise
                field_info = {
                    'level': '05',
                    'name': field_name,
                    'pic': 'X(20)',
                    'type': 'input',
                    'source': 'ACCEPT',
                    'line': 0,
                    'raw': f'ACCEPT {field_name}'
                }
                fields.append(field_info)
        
        # Create field entries for DISPLAY fields
        for field_name in display_fields:
            if (len(field_name) > 1 and field_name not in accept_fields and 
                field_name not in ['UPON', 'WITH', 'AT', 'LINE', 'COLUMN']):  # Filter noise
                field_info = {
                    'level': '05',
                    'name': field_name,
                    'pic': 'X(20)',
                    'type': 'display',
                    'source': 'DISPLAY',
                    'line': 0,
                    'raw': f'DISPLAY {field_name}'
                }
                fields.append(field_info)
        
        if fields:
            screen_name = f"UI_INTERACTION_{program_id[-8:].upper()}"
            screen_doc = create_enhanced_screen_document(
                program_id, screen_name, fields, {'interaction': True}, timestamp, 'ui_interaction'
            )
            screens.append(screen_doc)
    
    return screens


def extract_copybook_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screens based on COPY/INCLUDE statements."""
    
    screens = []
    
    copy_books = set(COPY_RE.findall(content))
    include_books = set(INCLUDE_RE.findall(content))
    
    all_copybooks = copy_books.union(include_books)
    
    for copybook in all_copybooks:
        if any(keyword in copybook.upper() for keyword in ['SCREEN', 'FORM', 'MAP', 'PANEL']):
            # Create a screen reference for screen-related copybooks
            field_info = {
                'level': '01',
                'name': copybook,
                'pic': 'COPYBOOK',
                'type': 'copybook',
                'source': 'COPY/INCLUDE',
                'line': 0,
                'raw': f'COPY {copybook}'
            }
            
            screen_name = f"COPYBOOK_{copybook}"
            screen_doc = create_enhanced_screen_document(
                program_id, screen_name, [field_info], {'copybook': True}, timestamp, 'copybook_ref'
            )
            screens.append(screen_doc)
    
    return screens


def extract_standalone_field_screens(program_id: str, content: str, timestamp: str) -> List[Dict]:
    """Extract screens from 77-level standalone fields."""
    
    screens = []
    lines = content.split('\n')
    
    standalone_fields = []
    
    for i, line in enumerate(lines):
        line_strip = line.strip()
        
        # Look for 77-level fields
        level_77_match = LEVEL_77_RE.match(line_strip)
        if level_77_match:
            field_name = level_77_match.group(1)
            
            # Look for PIC clause
            pic_match = PIC_RE.search(line_strip)
            pic_clause = pic_match.group(1) if pic_match else 'X'
            
            field_info = {
                'level': '77',
                'name': field_name,
                'pic': pic_clause,
                'type': 'standalone',
                'source': '77_LEVEL',
                'line': i + 1,
                'raw': line_strip
            }
            standalone_fields.append(field_info)
    
    # If we have multiple standalone fields, group them into a screen
    if len(standalone_fields) >= 3:
        screen_name = f"STANDALONE_FIELDS_{program_id[-8:].upper()}"
        screen_doc = create_enhanced_screen_document(
            program_id, screen_name, standalone_fields, {'standalone': True}, timestamp, 'standalone_fields'
        )
        screens.append(screen_doc)
    
    return screens


def create_enhanced_field_info(field_match, line_num: int, line_text: str, full_content: str, context_line: int) -> Dict:
    """Create enhanced field info with more context."""
    
    level = field_match.group(1)
    name = field_match.group(2)
    pic = field_match.group(3) or 'X'
    rest = field_match.group(4) or ''
    
    field_info = {
        'level': level,
        'name': name,
        'pic': pic,
        'line': line_num + 1,
        'attributes': rest.strip(),
        'raw': line_text.strip()
    }
    
    # Enhanced type detection
    if VALUE_RE.search(rest):
        field_info['type'] = 'display'
        value_match = VALUE_RE.search(rest)
        if value_match:
            field_info['value'] = value_match.group(1) or value_match.group(2)
    elif 'ACCEPT' in rest.upper():
        field_info['type'] = 'input'
    elif LINE_COL_RE.search(rest):
        field_info['type'] = 'positioned'
        field_info['has_positioning'] = True
    elif 'COMP' in rest.upper() or 'BINARY' in rest.upper():
        field_info['type'] = 'computational'
    else:
        field_info['type'] = 'data'
    
    # Add context analysis
    lines = full_content.split('\n')
    context_start = max(0, context_line - 2)
    context_end = min(len(lines), context_line + 3)
    context_text = '\n'.join(lines[context_start:context_end])
    
    # Look for screen-related keywords in context
    if any(keyword in context_text.upper() for keyword in ['ACCEPT', 'DISPLAY', 'LINE', 'COLUMN']):
        field_info['screen_context'] = True
    
    return field_info


def create_enhanced_screen_document(program_id: str, screen_name: str, fields: List[Dict], 
                                  attributes: Dict, timestamp: str, source_type: str) -> Dict:
    """Create enhanced screen document with detailed metadata."""
    
    # Generate unique screen ID
    content = f"{program_id}_{screen_name}_{len(fields)}_{source_type}"
    content_hash = hashlib.md5(content.encode()).hexdigest()[:16]
    screen_id = f"{program_id}_{content_hash}"
    
    # Enhanced field categorization
    input_fields = [f for f in fields if f.get('type') in ['input', 'data', 'positioned']]
    display_fields = [f for f in fields if f.get('type') in ['display']]
    computational_fields = [f for f in fields if f.get('type') == 'computational']
    copybook_fields = [f for f in fields if f.get('type') == 'copybook']
    
    # Determine screen type with more sophistication
    if copybook_fields:
        screen_type = 'copybook_reference'
    elif source_type == 'ui_interaction':
        screen_type = 'interactive'
    elif source_type == 'file_record':
        screen_type = 'file_record'
    elif input_fields and display_fields:
        screen_type = 'mixed_form'
    elif input_fields:
        screen_type = 'form'
    elif display_fields:
        screen_type = 'report'
    elif computational_fields:
        screen_type = 'computational'
    else:
        screen_type = 'data_structure'
    
    # Create enhanced summary
    summary_parts = [f"Screen {screen_name}"]
    summary_parts.append(f"{len(input_fields)} input, {len(display_fields)} display")
    
    if computational_fields:
        summary_parts.append(f"{len(computational_fields)} computational")
    if copybook_fields:
        summary_parts.append(f"{len(copybook_fields)} copybook refs")
    
    summary_parts.append(f"({source_type})")
    
    summary_text = "; ".join(summary_parts)
    
    # Enhanced metadata
    metadata = {
        'source_type': source_type,
        'has_positioning': any(f.get('has_positioning') for f in fields),
        'has_values': any(f.get('value') for f in fields),
        'complexity_score': len(fields) + (2 if attributes.get('positioning') else 0),
        'field_types': list(set(f.get('type') for f in fields)),
        **attributes
    }
    
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
        "layout_info_json": json.dumps(metadata),
        "raw_span_text": "",
        "summary_text": summary_text,
        "generated_at": timestamp,
        "has_vector": False
    }


def get_existing_screen_ids(screen_index_url: str, headers: Dict) -> Set[str]:
    """Get all existing screen IDs from the index."""
    
    existing_ids = set()
    
    # Get total count first
    count_body = {"search": "*", "top": 0, "count": True}
    response = requests.post(screen_index_url, headers=headers, json=count_body)
    total_count = response.json().get('@odata.count', 0) if response.status_code == 200 else 0
    
    # Get all existing IDs
    batch_size = 1000
    for skip in range(0, total_count, batch_size):
        body = {
            "search": "*",
            "top": min(batch_size, total_count - skip),
            "skip": skip,
            "select": "screen_id"
        }
        
        response = requests.post(screen_index_url, headers=headers, json=body)
        
        if response.status_code == 200:
            records = response.json().get('value', [])
            for record in records:
                existing_ids.add(record['screen_id'])
        else:
            break
    
    return existing_ids


def upload_screen_docs_robust(search_endpoint: str, headers: Dict, docs: List[Dict]) -> bool:
    """Upload screen documents with robust error handling."""
    
    upload_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
    batch_size = 100
    total_uploaded = 0
    failed_batches = 0
    
    for i in range(0, len(docs), batch_size):
        batch = docs[i:i + batch_size]
        
        upload_body = {
            "value": [{"@search.action": "mergeOrUpload", **doc} for doc in batch]
        }
        
        response = requests.post(upload_url, headers=headers, json=upload_body)
        
        if response.status_code in [200, 201]:
            total_uploaded += len(batch)
            print(f"     ✅ Uploaded batch {i//batch_size + 1}: {len(batch)} documents (total: {total_uploaded})")
        else:
            failed_batches += 1
            print(f"     ❌ Failed batch {i//batch_size + 1}: {response.status_code}")
            if failed_batches < 3:  # Show first few errors
                print(f"        Error: {response.text[:200]}")
    
    print(f"   📊 Upload summary: {total_uploaded} uploaded, {failed_batches} failed batches")
    
    return total_uploaded > 0


if __name__ == "__main__":
    generate_screens_from_all_sources()