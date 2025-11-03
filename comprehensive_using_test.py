#!/usr/bin/env python3

import sys
import os

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

# Import the cobolparser module
from cobolparser import load_azure_credentials, process_blob_content_full
from azure.storage.blob import BlobServiceClient
import json
import re

def find_actual_using_parameters():
    """Find COBOL files that have actual PROCEDURE DIVISION USING parameters (not just comments)"""
    print("Searching for COBOL files with actual PROCEDURE DIVISION USING parameters...")
    
    try:
        # Load Azure credentials
        connection_string = load_azure_credentials()
        blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        
        # Get the aisearch container
        container_name = "aisearch"
        container_client = blob_service_client.get_container_client(container_name)
        
        print(f"Connected to container: {container_name}")
        
        # Search through S35-Source files - expand search
        prefix = "S35-Source/"
        files_with_using = []
        files_checked = 0
        max_files_to_check = 200  # Increase search size
        
        print(f"\nSearching through first {max_files_to_check} COBOL files...")
        
        for blob in container_client.list_blobs(name_starts_with=prefix):
            if files_checked >= max_files_to_check:
                break
                
            blob_path = blob.name
            if blob_path.upper().endswith(('.CBL', '.CPY', '.COB')):
                files_checked += 1
                
                try:
                    # Download and check for USING parameters
                    blob_client = container_client.get_blob_client(blob.name)
                    blob_content = blob_client.download_blob().readall().decode('utf-8', errors='ignore')
                    
                    # Look for actual PROCEDURE DIVISION USING pattern (not in comments)
                    lines = blob_content.split('\n')
                    for i, line in enumerate(lines, 1):
                        stripped = line.strip()
                        # Skip comment lines
                        if stripped.startswith('*') or stripped.startswith('/'):
                            continue
                        
                        # Look for PROCEDURE DIVISION USING that's not in comments
                        if re.search(r'^\s*PROCEDURE\s+DIVISION\s+USING\s+', line, re.IGNORECASE):
                            print(f"  Found actual USING in {blob.name} at line {i}: {line.strip()}")
                            files_with_using.append((blob.name, line.strip(), blob_content))
                            break
                    
                    if files_checked % 25 == 0:
                        print(f"  Checked {files_checked} files, found {len(files_with_using)} with actual USING...")
                        
                except Exception as e:
                    if "does not exist" not in str(e):
                        print(f"  Error checking {blob.name}: {e}")
                    continue
        
        print(f"\nSearch complete. Checked {files_checked} files.")
        print(f"Found {len(files_with_using)} files with actual PROCEDURE DIVISION USING:")
        
        for filename, using_line, _ in files_with_using:
            print(f"  - {filename}")
            print(f"    {using_line}")
        
        if files_with_using:
            # Test with the first file that has actual USING parameters
            test_file, using_line, blob_content = files_with_using[0]
            print(f"\n=== TESTING ENHANCED PARSER WITH {test_file} ===")
            print(f"USING line: {using_line}")
            
            file_id = "actual_using_test001"
            file_rec, paras, data_items, proc_facts, xrefs, chunks, raw_lines = process_blob_content_full(
                test_file, blob_content, file_id
            )
            
            if file_rec:
                print(f"Program ID: {file_rec.program_id}")
                print(f"PROCEDURE DIVISION USING: {file_rec.procedure_using}")
                
                # Show LINKAGE section items
                linkage_items = [item for item in data_items if item.section == "LINKAGE"]
                print(f"\nLINKAGE section items: {len(linkage_items)}")
                for item in linkage_items:
                    print(f"  - {item.name} (Level {item.level}) - {item.pic or 'No PIC'}")
                
                # Show all cross-references with directions
                all_xrefs = sorted(xrefs, key=lambda x: (x.direction or 'zzz', x.simple_name))
                print(f"\nAll cross-references: {len(all_xrefs)}")
                
                # Group by direction
                directions = {}
                for xref in all_xrefs:
                    direction = xref.direction or 'no_direction'
                    if direction not in directions:
                        directions[direction] = []
                    directions[direction].append(xref)
                
                for direction, xrefs_list in directions.items():
                    print(f"\n  {direction}: {len(xrefs_list)} references")
                    for xref in xrefs_list[:3]:  # Show first 3 of each type
                        print(f"    - {xref.simple_name} ({xref.kind}) at line {xref.line}")
                
                # Save comprehensive output
                comprehensive_output = {
                    "test_info": {
                        "file_tested": test_file,
                        "using_line": using_line,
                        "parser_version": "enhanced_with_param_direction"
                    },
                    "file_info": {
                        "file_id": file_rec.file_id,
                        "path": file_rec.path,
                        "program_id": file_rec.program_id,
                        "format": file_rec.fmt,
                        "procedure_using": file_rec.procedure_using
                    },
                    "linkage_items": [
                        {
                            "name": item.name,
                            "qualified_name": item.qualified_name,
                            "level": item.level,
                            "section": item.section,
                            "pic": item.pic
                        }
                        for item in linkage_items
                    ],
                    "xref_summary": {
                        direction: len(xrefs_list) for direction, xrefs_list in directions.items()
                    },
                    "sample_xrefs": {
                        direction: [
                            {
                                "simple_name": xref.simple_name,
                                "qualified_name": xref.qualified_name,
                                "kind": xref.kind,
                                "direction": xref.direction,
                                "line": xref.line,
                                "snippet": xref.snippet[:80]
                            }
                            for xref in xrefs_list[:5]  # First 5 of each direction
                        ]
                        for direction, xrefs_list in directions.items()
                    }
                }
                
                with open("comprehensive_parser_test.json", "w") as f:
                    json.dump(comprehensive_output, f, indent=2)
                
                print(f"\nComprehensive output saved to: comprehensive_parser_test.json")
                print("\n=== COMPREHENSIVE ENHANCEMENT VERIFICATION COMPLETE ===")
                
                # Verify our goals are met
                print("\n=== VERIFICATION OF REQUIREMENTS ===")
                print(f"1. PROCEDURE DIVISION USING captured: {'✓' if file_rec.procedure_using else '✗'}")
                print(f"2. LINKAGE section items found: {'✓' if linkage_items else '✗'} ({len(linkage_items)} items)")
                print(f"3. Parameter directions implemented: {'✓' if any(d.startswith('param') for d in directions.keys()) else '✗'}")
                print(f"4. Direction analysis working: {'✓' if 'param_in' in directions or 'param_out' in directions else '✗'}")
                
            else:
                print(f"Failed to parse {test_file}")
        else:
            print("\nNo files with actual PROCEDURE DIVISION USING found in the expanded sample.")
            print("Let's create a synthetic test to verify our implementation:")
            
            # Create synthetic test
            synthetic_cobol = '''       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYNTHETIC-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER           PIC 9(3) VALUE 0.
       
       LINKAGE SECTION.
       01  L-INPUT-PARM         PIC X(10).
       01  L-OUTPUT-PARM        PIC 9(5).
       01  L-INOUT-PARM         PIC X(20).
       
       PROCEDURE DIVISION USING L-INPUT-PARM L-OUTPUT-PARM L-INOUT-PARM.
       MAIN-PARA.
           MOVE L-INPUT-PARM TO WS-COUNTER.
           COMPUTE L-OUTPUT-PARM = WS-COUNTER + 1.
           MOVE "HELLO" TO L-INOUT-PARM.
           DISPLAY L-INOUT-PARM.
       END-PARA.
           STOP RUN.'''
           
            print("\nTesting with synthetic COBOL program...")
            file_id = "synthetic_test001"
            file_rec, paras, data_items, proc_facts, xrefs, chunks, raw_lines = process_blob_content_full(
                "SYNTHETIC-TEST.CBL", synthetic_cobol, file_id
            )
            
            if file_rec:
                print(f"Synthetic test - Program ID: {file_rec.program_id}")
                print(f"Synthetic test - USING: {file_rec.procedure_using}")
                
                linkage_items = [item for item in data_items if item.section == "LINKAGE"]
                print(f"Synthetic test - LINKAGE items: {len(linkage_items)}")
                
                param_xrefs = [xref for xref in xrefs if xref.direction and xref.direction.startswith('param')]
                print(f"Synthetic test - Parameter xrefs: {len(param_xrefs)}")
                
                for xref in param_xrefs:
                    print(f"  - {xref.simple_name}: {xref.direction} ({xref.kind}) at line {xref.line}")
                    
                print("\n=== SYNTHETIC TEST VERIFICATION COMPLETE ===")
            
    except Exception as e:
        print(f"Error in comprehensive search: {str(e)}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    find_actual_using_parameters()
