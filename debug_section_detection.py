#!/usr/bin/env python3

import sys
import os
import re

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

def debug_section_detection():
    """Debug section detection and data item assignment"""
    
    # Test COBOL content
    test_cobol_content = '''       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECTION-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEMP              PIC X(20).
       01  WS-COUNTER           PIC 9(3) VALUE 0.
       
       LINKAGE SECTION.
       01  L-INPUT-ONLY         PIC X(10).
       01  L-OUTPUT-ONLY        PIC 9(5).
       01  L-INOUT-PARAM        PIC X(20).
       01  L-MODIFY-PARAM       PIC X(30).
       
       PROCEDURE DIVISION USING L-INPUT-ONLY L-OUTPUT-ONLY L-INOUT-PARAM L-MODIFY-PARAM.
       MAIN-PARA.
           MOVE L-INPUT-ONLY TO WS-TEMP.
           COMPUTE L-OUTPUT-ONLY = WS-COUNTER + 1.
           MOVE L-INOUT-PARAM TO WS-TEMP.
           MOVE "PROCESSED" TO L-INOUT-PARAM.
           MOVE "INITIAL" TO L-MODIFY-PARAM.
           MOVE L-MODIFY-PARAM TO WS-TEMP.
       END-PARA.
           STOP RUN.'''
    
    print("Debugging section detection and data item assignment...")
    
    # Import the normalization functions
    from cobolparser import normalize_cobol, RE_DATA_ITEM, RE_STORAGE_SECTION
    
    raw_lines, norm_lines = normalize_cobol(test_cobol_content)
    
    print(f"\nStorage section regex pattern: {RE_STORAGE_SECTION.pattern}")
    
    print("\nProcessing each line:")
    current_section = "UNKNOWN"
    data_items = []
    
    for i, line in enumerate(norm_lines, 1):
        # Check for section changes
        section_match = RE_STORAGE_SECTION.match(line)
        if section_match:
            old_section = current_section
            current_section = section_match.group(1).upper()
            print(f"Line {i:2d}: ✓ SECTION CHANGE: {old_section} -> {current_section}")
            print(f"         Line content: {repr(line)}")
            continue
            
        # Test data item regex
        dm = RE_DATA_ITEM.match(line)
        if dm:
            level = int(dm.group(1))
            name = dm.group(2).upper()
            pic = dm.group(7)
            
            print(f"Line {i:2d}: ✓ DATA ITEM: {name} (Level {level}) in section {current_section}")
            if pic:
                print(f"         PIC: {pic}")
            
            data_items.append({
                'name': name,
                'level': level,
                'section': current_section,
                'pic': pic,
                'line': i
            })
        else:
            stripped = line.strip()
            if stripped and not stripped.startswith('*') and not any(word in stripped.upper() for word in ['IDENTIFICATION', 'PROGRAM-ID', 'DATA', 'PROCEDURE', 'PARA', 'MOVE', 'COMPUTE', 'STOP']):
                if len(stripped) > 3:  # Ignore very short lines
                    print(f"Line {i:2d}: ? OTHER: {repr(line[:50])}")
    
    print(f"\n=== SUMMARY ===")
    print(f"Total data items found: {len(data_items)}")
    
    # Group by section
    sections = {}
    for item in data_items:
        section = item['section']
        if section not in sections:
            sections[section] = []
        sections[section].append(item)
    
    for section, items in sections.items():
        print(f"\n{section} section: {len(items)} items")
        for item in items:
            print(f"  - {item['name']} (Level {item['level']}) - {item['pic'] or 'No PIC'}")
    
    # Test some statements to see if they would match
    print(f"\n=== TESTING STATEMENT PARSING ===")
    test_statements = [
        "MOVE L-INPUT-ONLY TO WS-TEMP.",
        "COMPUTE L-OUTPUT-ONLY = WS-COUNTER + 1.",
        "MOVE L-INOUT-PARAM TO WS-TEMP.", 
        "MOVE \"PROCESSED\" TO L-INOUT-PARAM.",
        "MOVE \"INITIAL\" TO L-MODIFY-PARAM.",
        "MOVE L-MODIFY-PARAM TO WS-TEMP."
    ]
    
    from cobolparser import RE_MOVE, RE_COMPUTE
    
    for stmt in test_statements:
        print(f"\nTesting statement: {stmt}")
        
        move_match = RE_MOVE.match(stmt)
        if move_match:
            src = move_match.group(1).strip()
            tgt = move_match.group(2).upper()
            print(f"  ✓ MOVE: {src} -> {tgt}")
        else:
            compute_match = RE_COMPUTE.match(stmt)
            if compute_match:
                tgt = compute_match.group(1).upper()
                expr = compute_match.group(2).strip()
                print(f"  ✓ COMPUTE: {tgt} = {expr}")
            else:
                print(f"  ✗ NO MATCH")

if __name__ == "__main__":
    debug_section_detection()
