#!/usr/bin/env python3

import sys
import os
import re

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

def debug_data_item_regex():
    """Debug the data item regex pattern"""
    
    # Test COBOL content
    test_cobol_content = '''       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEBUG-TEST.
       
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
    
    print("Debugging data item regex...")
    
    # Import the normalization functions
    from cobolparser import normalize_cobol, RE_DATA_ITEM, RE_STORAGE_SECTION
    
    raw_lines, norm_lines = normalize_cobol(test_cobol_content)
    
    print("\nNormalized lines:")
    for i, line in enumerate(norm_lines, 1):
        print(f"{i:2d}: {repr(line)}")
    
    print(f"\nData item regex pattern: {RE_DATA_ITEM.pattern}")
    
    print("\nTesting data item regex on each line:")
    current_section = "UNKNOWN"
    
    for i, line in enumerate(norm_lines, 1):
        # Check for section changes
        if RE_STORAGE_SECTION.match(line):
            current_section = RE_STORAGE_SECTION.match(line).group(1).upper()
            print(f"Line {i:2d}: Section change to {current_section}")
            continue
            
        # Test data item regex
        dm = RE_DATA_ITEM.match(line)
        if dm:
            print(f"Line {i:2d}: ✓ DATA ITEM MATCH - Level: {dm.group(1)}, Name: {dm.group(2)}, Section: {current_section}")
            print(f"         Full match: {dm.groups()}")
        else:
            # Check if line looks like it could be a data item
            stripped = line.strip()
            if stripped and stripped[0].isdigit():
                print(f"Line {i:2d}: ✗ LOOKS LIKE DATA ITEM BUT NO MATCH: {repr(line)}")
                
                # Test partial patterns
                level_match = re.match(r"^\s*(01|02|03|04|05|10|77)\s+([A-Z0-9\-]+)", line, re.IGNORECASE)
                if level_match:
                    print(f"         Level and name match: {level_match.groups()}")
                    
                    # Check what's after the name
                    remainder = line[level_match.end():]
                    print(f"         Remainder: {repr(remainder)}")
                    
                    # Check for PIC
                    pic_match = re.search(r"\s+PIC\s+([A-Z9VXS\(\)9\.\+\/\-]+)", remainder, re.IGNORECASE)
                    if pic_match:
                        print(f"         PIC match: {pic_match.groups()}")
                    
                    # Check for period at end
                    if remainder.strip().endswith('.'):
                        print(f"         Has period at end")
                    else:
                        print(f"         NO PERIOD AT END - this might be the issue!")

if __name__ == "__main__":
    debug_data_item_regex()
