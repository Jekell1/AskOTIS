"""
Enhanced COBOL call extractor that handles dynamic calling patterns.

This tool recognizes both direct calls and the dynamic calling pattern:
MOVE "program" TO FORM-NAM
CALL FORM-PROGX USING...

And properly indexes all called programs.
"""

import re
import json
import os
from pathlib import Path
from typing import Dict, List, Set, Tuple
import requests

def extract_dynamic_calls(cobol_content: str, program_name: str) -> Dict:
    """Extract both direct and dynamic calls from COBOL content."""
    
    lines = cobol_content.split('\n')
    
    # Results storage
    direct_calls = []
    dynamic_calls = []
    form_nam_moves = []
    call_statements = []
    
    # Patterns
    direct_call_pattern = re.compile(r'CALL\s+"([^"]+)"', re.IGNORECASE)
    move_to_form_nam_pattern = re.compile(r'MOVE\s+"([^"]+)"\s+TO\s+FORM-NAM', re.IGNORECASE)
    call_form_progx_pattern = re.compile(r'CALL\s+FORM-PROGX', re.IGNORECASE)
    
    # First pass: collect all MOVE TO FORM-NAM and CALL FORM-PROGX statements
    for line_num, line in enumerate(lines, 1):
        line = line.strip()
        
        # Direct calls
        direct_match = direct_call_pattern.search(line)
        if direct_match:
            called_program = direct_match.group(1)
            direct_calls.append({
                'line': line_num,
                'program': called_program,
                'statement': line,
                'type': 'direct'
            })
        
        # MOVE to FORM-NAM
        move_match = move_to_form_nam_pattern.search(line)
        if move_match:
            target_program = move_match.group(1)
            form_nam_moves.append({
                'line': line_num,
                'program': target_program,
                'statement': line
            })
        
        # CALL FORM-PROGX
        call_match = call_form_progx_pattern.search(line)
        if call_match:
            call_statements.append({
                'line': line_num,
                'statement': line
            })
    
    # Second pass: match MOVE TO FORM-NAM with subsequent CALL FORM-PROGX
    for move in form_nam_moves:
        # Find the next CALL FORM-PROGX after this MOVE
        for call in call_statements:
            # Look for CALL within reasonable distance (typically within 10 lines)
            if call['line'] > move['line'] and call['line'] <= move['line'] + 10:
                dynamic_calls.append({
                    'line': call['line'],
                    'program': move['program'],
                    'move_line': move['line'],
                    'move_statement': move['statement'],
                    'call_statement': call['statement'],
                    'type': 'dynamic'
                })
                break  # Take the first matching CALL
    
    # Combine and deduplicate
    all_calls = direct_calls + dynamic_calls
    unique_programs = set()
    for call in all_calls:
        unique_programs.add(call['program'])
    
    return {
        'program_name': program_name,
        'total_calls': len(all_calls),
        'unique_programs': len(unique_programs),
        'called_programs': sorted(list(unique_programs)),
        'direct_calls': direct_calls,
        'dynamic_calls': dynamic_calls,
        'all_calls': all_calls,
        'move_statements': form_nam_moves,
        'call_form_progx_statements': call_statements
    }

def enhance_apipay_calls():
    """Enhanced analysis of APIPAY calls."""
    
    # Read APIPAY.CBL
    apipay_path = Path('cobol_src/SP/APIPAY.CBL')
    if not apipay_path.exists():
        print("❌ APIPAY.CBL not found")
        return
    
    with open(apipay_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Extract calls
    results = extract_dynamic_calls(content, 'APIPAY')
    
    print("=" * 80)
    print("ENHANCED APIPAY CALL ANALYSIS")
    print("=" * 80)
    print(f"Program: {results['program_name']}")
    print(f"Total call statements: {results['total_calls']}")
    print(f"Unique programs called: {results['unique_programs']}")
    print()
    
    print("📋 CALLED PROGRAMS:")
    print("-" * 40)
    for i, program in enumerate(results['called_programs'], 1):
        print(f"{i:2d}. {program}")
    
    print("\n🔗 DIRECT CALLS:")
    print("-" * 40)
    if results['direct_calls']:
        for call in results['direct_calls']:
            print(f"Line {call['line']:4d}: {call['program']}")
            print(f"          {call['statement']}")
    else:
        print("   (No direct calls found)")
    
    print("\n🔄 DYNAMIC CALLS:")
    print("-" * 40)
    if results['dynamic_calls']:
        for call in results['dynamic_calls']:
            print(f"Line {call['move_line']:4d}: MOVE \"{call['program']}\" TO FORM-NAM")
            print(f"Line {call['line']:4d}: {call['call_statement']}")
            print(f"          → Calls: {call['program']}")
            print()
    else:
        print("   (No dynamic calls found)")
    
    print("\n📊 CALL FREQUENCY:")
    print("-" * 40)
    program_count = {}
    for call in results['all_calls']:
        program = call['program']
        program_count[program] = program_count.get(program, 0) + 1
    
    for program, count in sorted(program_count.items(), key=lambda x: x[1], reverse=True):
        print(f"{program:15s}: {count} call(s)")
    
    return results

def update_calls_index_with_enhanced_data():
    """Update the calls index with enhanced dynamic call data."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    # Extract enhanced call data
    apipay_path = Path('cobol_src/SP/APIPAY.CBL')
    with open(apipay_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    results = extract_dynamic_calls(content, 'APIPAY')
    
    # Prepare documents for calls index
    documents = []
    
    for call in results['all_calls']:
        doc_id = f"APIPAY_CALL_{call['line']:04d}"
        
        documents.append({
            "@search.action": "mergeOrUpload",
            "call_id": doc_id,
            "caller_program": "APIPAY",
            "called_program": call['program'],
            "call_type": call['type'],
            "line_number": call['line'],
            "call_statement": call.get('call_statement', call.get('statement', '')),
            "move_statement": call.get('move_statement', ''),
            "move_line": call.get('move_line', 0),
            "file_path": "cobol_src/SP/APIPAY.CBL"
        })
    
    # Upload to calls index
    upload_url = f"{endpoint}/indexes/calls/docs/index?api-version=2024-07-01"
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    if documents:
        response = requests.post(upload_url, headers=headers, json={"value": documents})
        
        if response.status_code in [200, 201]:
            print(f"\n✅ Successfully uploaded {len(documents)} enhanced call records to calls index")
        else:
            print(f"\n❌ Failed to upload to calls index: {response.status_code}")
            print(f"Response: {response.text[:200]}")
    
    return results

if __name__ == "__main__":
    print("🔧 ENHANCED COBOL CALL EXTRACTOR")
    print("=" * 80)
    
    # Run enhanced analysis
    results = enhance_apipay_calls()
    
    # Update the index
    if results:
        print("\n" + "=" * 80)
        print("UPDATING CALLS INDEX")
        print("=" * 80)
        update_calls_index_with_enhanced_data()
    
    print("\n✅ Enhanced call extraction complete!")