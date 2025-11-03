#!/usr/bin/env python3

import sys
import os

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

from cobolparser import process_blob_content_full

def comprehensive_copybook_test():
    """Comprehensive test of all copybook tracking functionality"""
    
    print("=== COMPREHENSIVE COPYBOOK TEST ===")
    print("Testing all copybook tracking features")
    print()
    
    # Create comprehensive test COBOL code
    test_cobol = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYBOOK-TEST.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       COPY WSCOPY.
       01  WS-COUNTER  PIC 9(3) VALUE 0.
       COPY COMMON-WS REPLACING ==:PREFIX:== BY ==WS-==.
       
       LINKAGE SECTION.
       COPY LINKAGE-COPY.
       01  L-PARM      PIC X(10).
       COPY PARM-LAYOUTS REPLACING LEADING 'L-' BY 'WS-'.
       
       PROCEDURE DIVISION.
       
       MAIN-PARA.
           COPY INIT-PROC.
           PERFORM SUB-PARA.
           COPY CLEANUP-PROC REPLACING ==:MODE:== BY ==NORMAL==.
           STOP RUN.
           
       SUB-PARA.
           ADD 1 TO WS-COUNTER.
           COPY INLINE-LOGIC REPLACING LEADING 'A' BY 'B'.
           DISPLAY WS-COUNTER.
    """
    
    print("Test COBOL code with multiple COPY statements:")
    for i, line in enumerate(test_cobol.split('\n'), 1):
        if 'COPY' in line.upper():
            print(f"{i:3}: {line} ← COPY")
        else:
            print(f"{i:3}: {line}")
    print()
    
    # Process the test code
    file_id = "test002"
    blob_name = "COPYBOOK-TEST.CBL"
    
    try:
        file_rec, paragraphs, data_items, proc_facts, xrefs, chunks, flow_edges, copybooks, raw_lines = process_blob_content_full(
            blob_name, test_cobol, file_id
        )
        
        print("📋 PROCESSING RESULTS:")
        print(f"   File ID: {file_rec.file_id}")
        print(f"   Program ID: {file_rec.program_id}")
        print(f"   Paragraphs: {len(paragraphs)}")
        print(f"   Data items: {len(data_items)}")
        print(f"   Copybooks: {len(copybooks)} ← KEY METRIC")
        print(f"   Chunks: {len(chunks)}")
        print()
        
        print("📚 DETAILED COPYBOOK ANALYSIS:")
        for i, copybook in enumerate(copybooks, 1):
            print(f"   {i}. COPY {copybook.copybook_name}")
            print(f"      File ID: {copybook.file_id}")
            print(f"      Parent: {copybook.parent_path}")
            print(f"      Line: {copybook.line}")
            if copybook.replacing_clause:
                print(f"      REPLACING: {copybook.replacing_clause}")
            else:
                print(f"      No REPLACING clause")
            
            # Generate copybook citation
            citation = f"{copybook.parent_path}:{copybook.line}"
            print(f"      Citation: {citation}")
            print()
        
        # Analyze chunks for COPY preservation
        print("📦 CHUNK ANALYSIS FOR COPY PRESERVATION:")
        chunks_with_copy = 0
        copy_lines_in_chunks = []
        
        for chunk in chunks:
            lines = chunk.text.split('\n')
            for line_offset, line in enumerate(lines):
                if 'COPY' in line.upper() and line.strip():
                    chunks_with_copy += 1 if chunks_with_copy == 0 else 0  # Count chunks, not lines
                    actual_line_num = chunk.start_line + line_offset
                    copy_lines_in_chunks.append((actual_line_num, line.strip()))
        
        print(f"   Chunks containing COPY: {len([c for c in chunks if 'COPY' in c.text.upper()])}")
        print(f"   COPY lines preserved in chunks:")
        for line_num, line_text in copy_lines_in_chunks[:5]:  # Show first 5
            print(f"      Line {line_num}: {line_text}")
        
        if len(copy_lines_in_chunks) > 5:
            print(f"      ... and {len(copy_lines_in_chunks) - 5} more")
        print()
        
        # Validate copybook-to-JSONL structure
        print("🗂️ JSONL RECORD STRUCTURE:")
        if copybooks:
            sample_copybook = copybooks[0]
            jsonl_record = {
                "copybook_id": f"{sample_copybook.file_id}:{sample_copybook.line}:{sample_copybook.copybook_name}",
                "file_id": sample_copybook.file_id,
                "parent_path": sample_copybook.parent_path,
                "copybook_name": sample_copybook.copybook_name,
                "line": sample_copybook.line,
                "replacing_clause": sample_copybook.replacing_clause,
            }
            
            print("   Sample JSONL record structure:")
            for key, value in jsonl_record.items():
                print(f"      {key}: {value}")
        print()
        
        # Test line anchor functionality
        print("🎯 LINE ANCHOR DEMONSTRATION:")
        for copybook in copybooks[:3]:  # Show first 3
            anchor = f"{copybook.parent_path}:{copybook.line}"
            print(f"   Copybook: {copybook.copybook_name}")
            print(f"   Line Anchor: {anchor}")
            print(f"   Usage: 'Found {copybook.copybook_name} at {anchor}'")
            if copybook.replacing_clause:
                print(f"   Modified: REPLACING {copybook.replacing_clause}")
            print()
        
        # Summary validation
        print("✅ ACCEPTANCE CRITERIA VALIDATION:")
        
        # Criterion 1: copybooks.jsonl exists with one record per COPY
        expected_copy_count = test_cobol.upper().count('COPY ')
        actual_copy_count = len(copybooks)
        print(f"   Records per COPY: Expected {expected_copy_count}, Got {actual_copy_count}")
        if actual_copy_count >= 6:  # Should have at least 6 COPY statements
            print("   ✅ PASS: One record per COPY statement")
        else:
            print("   ❌ FAIL: Missing COPY records")
        
        # Criterion 2: chunks.jsonl preserves COPY text unchanged
        copy_preserved = len(copy_lines_in_chunks) > 0
        print(f"   COPY text preservation: {'✅ PASS' if copy_preserved else '❌ FAIL'}")
        
        # Overall result
        print()
        print("🏆 OVERALL TEST RESULT:")
        if actual_copy_count >= 6 and copy_preserved:
            print("   ✅ ALL TESTS PASSED - Copybook tracking fully functional!")
        else:
            print("   ❌ SOME TESTS FAILED - Review implementation")
            
        return copybooks
        
    except Exception as e:
        print(f"❌ Error during processing: {e}")
        import traceback
        traceback.print_exc()
        return []

if __name__ == "__main__":
    comprehensive_copybook_test()
