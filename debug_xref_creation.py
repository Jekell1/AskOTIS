#!/usr/bin/env python3

import sys
import os

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

# Create a debug version of the xref creation logic
def debug_xref_creation():
    """Debug xref creation to see why some are missing"""
    
    # Test COBOL content
    test_cobol_content = '''       IDENTIFICATION DIVISION.
       PROGRAM-ID. XREF-TEST.
       
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
    
    print("Debugging xref creation...")
    
    # Import what we need
    from cobolparser import (normalize_cobol, split_statements, RE_MOVE, RE_COMPUTE, 
                             RE_DATA_ITEM, RE_STORAGE_SECTION, DataItemRec)
    import re
    
    raw_lines, norm_lines = normalize_cobol(test_cobol_content)
    
    # Parse data items first (like in the real parser)
    print("\n=== PARSING DATA ITEMS ===")
    data_items = []
    current_section = "UNKNOWN"
    
    for i, line in enumerate(norm_lines, start=1):
        if RE_STORAGE_SECTION.match(line):
            current_section = RE_STORAGE_SECTION.match(line).group(1).upper()
            print(f"Line {i:2d}: Section changed to {current_section}")
            continue
            
        dm = RE_DATA_ITEM.match(line)
        if dm:
            level = int(dm.group(1))
            name = dm.group(2).upper()
            pic = dm.group(7)
            
            qualified = name  # For simplicity, no qualification
            
            data_items.append(DataItemRec(
                file_id="test",
                name=name,
                qualified_name=qualified,
                level=level,
                section=current_section,
                pic=pic,
                usage=None,
                occurs_low=None,
                occurs_high=None,
                depends_on=None,
                redefines=None,
                value=None,
                start_line=i,
                end_line=i,
            ))
            
            print(f"Line {i:2d}: Data item {name} in {current_section} section")
    
    print(f"\nTotal data items parsed: {len(data_items)}")
    
    # Build qmap
    print(f"\n=== BUILDING QMAP ===")
    qmap = {}
    for di in data_items:
        qmap.setdefault(di.name, []).append(di.qualified_name)
        print(f"qmap[{di.name}] = {qmap[di.name]}")
    
    print(f"\nqmap keys: {list(qmap.keys())}")
    
    # Process statements
    print(f"\n=== PROCESSING STATEMENTS FOR XREFS ===")
    
    statements = split_statements(norm_lines)
    xrefs_created = []
    
    for end_line, stmt in statements:
        s = stmt.strip()
        if not s:
            continue
            
        print(f"\nProcessing statement at line {end_line}: {s}")
        
        # MOVE
        m = RE_MOVE.match(s)
        if m:
            src = m.group(1).strip()
            tgt = m.group(2).upper()
            
            print(f"  MOVE: {src} -> {tgt}")
            
            # Check target
            print(f"    Checking target '{tgt}' in qmap: {tgt in qmap}")
            if tgt in qmap:
                for qn in qmap[tgt]:
                    xrefs_created.append(f"Line {end_line}: {qn} ({tgt}) - write")
                    print(f"      Created write xref: {qn}")
            
            # Check source tokens
            tokens = re.findall(r"[A-Z0-9\-]+", src, flags=re.IGNORECASE)
            print(f"    Source tokens: {tokens}")
            for token in tokens:
                t_up = token.upper()
                print(f"      Checking token '{t_up}' in qmap: {t_up in qmap}")
                if t_up in qmap:
                    for qn in qmap[t_up]:
                        xrefs_created.append(f"Line {end_line}: {qn} ({t_up}) - read")
                        print(f"        Created read xref: {qn}")
            continue
        
        # COMPUTE
        m = RE_COMPUTE.match(s)
        if m:
            tgt = m.group(1).upper()
            expr = m.group(2).strip()
            
            print(f"  COMPUTE: {tgt} = {expr}")
            
            # Check target
            print(f"    Checking target '{tgt}' in qmap: {tgt in qmap}")
            if tgt in qmap:
                for qn in qmap[tgt]:
                    xrefs_created.append(f"Line {end_line}: {qn} ({tgt}) - write")
                    print(f"      Created write xref: {qn}")
            
            # Check expression tokens
            tokens = re.findall(r"[A-Z0-9\-]+", expr, flags=re.IGNORECASE)
            print(f"    Expression tokens: {tokens}")
            for token in tokens:
                t_up = token.upper()
                print(f"      Checking token '{t_up}' in qmap: {t_up in qmap}")
                if t_up in qmap:
                    for qn in qmap[t_up]:
                        xrefs_created.append(f"Line {end_line}: {qn} ({t_up}) - read")
                        print(f"        Created read xref: {qn}")
            continue
    
    print(f"\n=== SUMMARY ===")
    print(f"Total xrefs created: {len(xrefs_created)}")
    for xref in xrefs_created:
        print(f"  {xref}")

if __name__ == "__main__":
    debug_xref_creation()
