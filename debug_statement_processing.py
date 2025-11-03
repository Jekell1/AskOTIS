#!/usr/bin/env python3

import sys
import os

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

from cobolparser import split_statements, normalize_cobol, RE_MOVE, RE_COMPUTE
import re

def debug_statement_processing():
    """Debug statement processing to see which statements are being parsed"""
    
    # Test COBOL content
    test_cobol_content = '''       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATEMENT-TEST.
       
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
    
    print("Debugging statement processing...")
    
    raw_lines, norm_lines = normalize_cobol(test_cobol_content)
    
    print(f"\nNormalized lines:")
    for i, line in enumerate(norm_lines, 1):
        print(f"{i:2d}: {repr(line)}")
    
    print(f"\n=== TESTING split_statements FUNCTION ===")
    statements = split_statements(norm_lines)
    
    print(f"Found {len(statements)} statements:")
    for end_line, stmt in statements:
        print(f"Line {end_line:2d}: {repr(stmt)}")
    
    print(f"\n=== TESTING STATEMENT REGEX MATCHING ===")
    
    for end_line, stmt in statements:
        s = stmt.strip()
        if not s:
            continue
            
        print(f"\nTesting statement at line {end_line}: {s}")
        
        # Test MOVE
        move_match = RE_MOVE.match(s)
        if move_match:
            src = move_match.group(1).strip()
            tgt = move_match.group(2).upper()
            print(f"  ✓ MOVE MATCH: {src} -> {tgt}")
            
            # Test token extraction from source
            tokens = re.findall(r"[A-Z0-9\-]+", src, flags=re.IGNORECASE)
            print(f"    Source tokens: {tokens}")
            continue
        
        # Test COMPUTE  
        compute_match = RE_COMPUTE.match(s)
        if compute_match:
            tgt = compute_match.group(1).upper()
            expr = compute_match.group(2).strip()
            print(f"  ✓ COMPUTE MATCH: {tgt} = {expr}")
            
            # Test token extraction from expression
            tokens = re.findall(r"[A-Z0-9\-]+", expr, flags=re.IGNORECASE)
            print(f"    Expression tokens: {tokens}")
            continue
        
        print(f"  ✗ NO MATCH for: {s}")

if __name__ == "__main__":
    debug_statement_processing()
