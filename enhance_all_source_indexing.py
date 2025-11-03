"""
Comprehensive COBOL source file indexing enhancement.

This tool analyzes all COBOL source files (.CBL and .CPY) and indexes all external references
using the 21 identified patterns, then uploads enhanced data to the existing new_cobol_calls index.
"""

import os
import json
import requests
from pathlib import Path
from typing import Dict, List, Set, Tuple
import re
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
from datetime import datetime, timezone

# Import our comprehensive patterns
from comprehensive_external_references import get_cobol_external_reference_patterns, extract_all_external_references

def find_all_cobol_files(root_dir: str = "cobol_src") -> List[Path]:
    """Find all COBOL source files in the directory tree."""
    
    cobol_extensions = {'.cbl', '.cob', '.cobol', '.pgm', '.cpy'}
    cobol_files = []
    
    root_path = Path(root_dir)
    if not root_path.exists():
        print(f"❌ Directory {root_dir} not found")
        return []
    
    for file_path in root_path.rglob('*'):
        if file_path.is_file() and file_path.suffix.lower() in cobol_extensions:
            cobol_files.append(file_path)
    
    return sorted(cobol_files)

def extract_program_name_from_file(file_path: Path) -> str:
    """Extract program name from file path or content."""
    
    # Try to get from filename first
    program_name = file_path.stem.upper()
    
    # Try to read PROGRAM-ID from file content
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read(2000)  # Read first 2KB to find PROGRAM-ID
            
        program_id_match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9\-_]+)', content, re.IGNORECASE)
        if program_id_match:
            program_name = program_id_match.group(1).upper()
    
    except Exception:
        pass  # Use filename if content read fails
    
    return program_name

def process_single_file(file_path: Path) -> Dict:
    """Process a single COBOL file for external references."""
    
    try:
        # Read file content
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        # Extract program name
        program_name = extract_program_name_from_file(file_path)
        
        # Get all external references
        references = extract_all_external_references(content, program_name)
        
        # Add file metadata
        references['file_path'] = str(file_path)
        references['file_size'] = file_path.stat().st_size
        references['last_modified'] = file_path.stat().st_mtime
        references['analysis_timestamp'] = datetime.now(timezone.utc).isoformat()
        
        return references
    
    except Exception as e:
        return {
            'program_name': file_path.stem.upper(),
            'file_path': str(file_path),
            'error': str(e),
            'analysis_timestamp': datetime.now(timezone.utc).isoformat()
        }

def create_enhanced_dependencies_index():
    """Extend the existing new_cobol_calls index with enhanced fields if needed."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "new_cobol_calls"  # Use existing index
    
    # Check if the index already has our enhanced fields
    schema_url = f"{endpoint}/indexes/{index_name}?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    response = requests.get(schema_url, headers=headers)
    if response.status_code != 200:
        print(f"❌ Error getting current schema: {response.status_code}")
        return False
    
    current_schema = response.json()
    existing_field_names = [f['name'] for f in current_schema['fields']]
    
    # Check if enhanced fields exist
    enhanced_fields = ['reference_type', 'reference_description', 'category', 'is_program_call', 'is_copybook', 'is_system_call', 'enhanced_data']
    missing_fields = [f for f in enhanced_fields if f not in existing_field_names]
    
    if missing_fields:
        print(f"❌ Index {index_name} is missing enhanced fields: {missing_fields}")
        print("Please run extend_calls_index.py first")
        return False
    else:
        print(f"✅ Index {index_name} already has enhanced fields")
        return True

def categorize_reference(ref_type: str, referenced_name: str) -> Tuple[str, bool, bool, bool]:
    """Categorize a reference for better indexing."""
    
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

def upload_enhanced_dependencies(all_references: List[Dict]):
    """Upload enhanced dependency data to the existing new_cobol_calls index."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "new_cobol_calls"  # Use existing index
    upload_url = f"{endpoint}/indexes/{index_name}/docs/index?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    # Prepare documents
    documents = []
    timestamp = datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
    
    for file_refs in all_references:
        if 'error' in file_refs:
            continue  # Skip files with errors
        
        program_name = file_refs['program_name']
        file_path = file_refs['file_path']
        
        for ref in file_refs.get('all_references', []):
            # Create unique call_id for enhanced entries
            call_id = f"ENHANCED_{program_name}_{ref['line']:05d}_{ref['type']}"
            
            category, is_program_call, is_copybook, is_system_call = categorize_reference(
                ref['type'], ref['referenced_name']
            )
            
            # Map to calls index structure
            documents.append({
                "@search.action": "mergeOrUpload",
                "call_id": call_id,
                "caller_program": program_name,
                "callee_program": ref['referenced_name'],
                "file_id": file_path,
                "file_path": file_path,
                "line": ref['line'],
                "col": 1,  # Default column
                "occurrence": 0,
                "call_type": "enhanced",  # Enhanced call type
                "is_dynamic": ref['type'] in ['CALL_DYNAMIC', 'CANCEL_DYNAMIC'],
                "snippet": ref['statement'][:200],  # Truncate for snippet field
                "has_vector": False,  # No vector for now
                # Enhanced fields
                "reference_type": ref['type'],
                "reference_description": ref['description'],
                "category": category,
                "is_program_call": is_program_call,
                "is_copybook": is_copybook,
                "is_system_call": is_system_call,
                "enhanced_data": True,
                "ingested_at": timestamp
            })
    
    # Upload in batches
    batch_size = 100
    total_uploaded = 0
    failed_batches = 0
    
    for i in range(0, len(documents), batch_size):
        batch = documents[i:i + batch_size]
        
        response = requests.post(upload_url, headers=headers, json={"value": batch})
        
        if response.status_code in [200, 201]:
            total_uploaded += len(batch)
            print(f"✅ Uploaded batch {i // batch_size + 1}: {len(batch)} documents (total: {total_uploaded})")
        else:
            failed_batches += 1
            print(f"❌ Failed to upload batch {i // batch_size + 1}: {response.status_code}")
            if failed_batches <= 3:  # Show first few errors only
                print(f"   Error: {response.text[:200]}")
    
    return total_uploaded, failed_batches

def enhance_all_source_files():
    """Main function to enhance indexing for all source files."""
    
    print("🚀 COMPREHENSIVE COBOL SOURCE FILE ENHANCEMENT")
    print("=" * 80)
    
    # Find all COBOL files
    print("📂 Finding COBOL source files (.CBL and .CPY)...")
    cobol_files = find_all_cobol_files()
    print(f"   Found {len(cobol_files)} COBOL files")
    
    if not cobol_files:
        print("❌ No COBOL files found")
        return
    
    # Show file distribution
    cbl_files = [f for f in cobol_files if f.suffix.lower() in ['.cbl', '.cob', '.cobol', '.pgm']]
    cpy_files = [f for f in cobol_files if f.suffix.lower() == '.cpy']
    print(f"   • Programs (.CBL, .COB, etc.): {len(cbl_files)}")
    print(f"   • Copybooks (.CPY): {len(cpy_files)}")
    
    # Check if enhanced index exists
    print("\n🔧 Checking enhanced dependencies index...")
    if not create_enhanced_dependencies_index():
        print("❌ Enhanced index not ready")
        return
    
    # Process files in parallel
    print(f"\n⚙️  Processing {len(cobol_files)} files...")
    start_time = time.time()
    
    all_references = []
    processed_count = 0
    error_count = 0
    
    with ThreadPoolExecutor(max_workers=4) as executor:
        # Submit all files for processing
        future_to_file = {executor.submit(process_single_file, file_path): file_path 
                         for file_path in cobol_files}
        
        # Collect results
        for future in as_completed(future_to_file):
            file_path = future_to_file[future]
            try:
                result = future.result()
                all_references.append(result)
                
                if 'error' in result:
                    error_count += 1
                    print(f"❌ Error processing {file_path}: {result['error']}")
                else:
                    processed_count += 1
                    refs_count = result.get('total_references', 0)
                    unique_count = result.get('unique_references', 0)
                    print(f"✅ {result['program_name']}: {refs_count} refs, {unique_count} unique")
                
            except Exception as e:
                error_count += 1
                print(f"❌ Exception processing {file_path}: {e}")
    
    processing_time = time.time() - start_time
    
    print(f"\n📊 Processing Summary:")
    print(f"   Total files: {len(cobol_files)}")
    print(f"   Processed successfully: {processed_count}")
    print(f"   Errors: {error_count}")
    print(f"   Processing time: {processing_time:.2f} seconds")
    
    # Calculate totals
    total_references = sum(ref.get('total_references', 0) for ref in all_references if 'error' not in ref)
    total_unique = sum(ref.get('unique_references', 0) for ref in all_references if 'error' not in ref)
    
    print(f"   Total external references: {total_references:,}")
    print(f"   Total unique references: {total_unique:,}")
    
    # Upload to Azure Search
    if processed_count > 0:
        print(f"\n📤 Uploading enhanced dependency data...")
        uploaded_count, failed_batches = upload_enhanced_dependencies(all_references)
        print(f"   Uploaded {uploaded_count:,} dependency records")
        if failed_batches > 0:
            print(f"   Failed batches: {failed_batches}")
    
    print("\n✅ Comprehensive source file enhancement complete!")
    print(f"🎯 Enhanced {processed_count} COBOL files with {uploaded_count:,} dependencies")
    print("   RAG system now has comprehensive visibility into all external references!")
    
    return all_references

if __name__ == "__main__":
    enhance_all_source_files()