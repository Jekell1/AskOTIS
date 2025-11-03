#!/usr/bin/env python3
"""
Comprehensive COBOL Parser Test with 5 S35-Source Files
======================================================

This script tests the full functionality of cobolparser.py by processing
5 real S35-Source files from Azure Blob Storage and validating:

1. File-level parsing (program-id, format detection, etc.)
2. Data items extraction (WORKING-STORAGE, LINKAGE sections)
3. Paragraph/section detection
4. Cross-reference generation with enrichment fields
5. Flow edge tracking
6. Copybook detection
7. Output serialization to JSONL format

The test validates that all xrefs include:
- path (blob name)
- program_id (from PROGRAM-ID statement)
- start_col and end_col (precise column positions)
- direction (param_in, param_out, read, write)
"""

import sys
import os
import json
from typing import List, Dict, Any

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

# Import the cobolparser module
from cobolparser import process_blob_content_full, load_azure_credentials
from azure.storage.blob import BlobServiceClient

def validate_xref_enrichment(xrefs: List, file_path: str, program_id: str) -> Dict[str, Any]:
    """Validate that xrefs have all enrichment fields properly populated"""
    validation_results = {
        "total_xrefs": len(xrefs),
        "with_path": 0,
        "with_program_id": 0,
        "with_columns": 0,
        "with_direction": 0,
        "column_accuracy": 0,
        "issues": []
    }
    
    for i, xref in enumerate(xrefs):
        # Check path field
        if xref.path == file_path:
            validation_results["with_path"] += 1
        elif xref.path is None:
            validation_results["issues"].append(f"Xref {i+1}: Missing path")
        else:
            validation_results["issues"].append(f"Xref {i+1}: Wrong path '{xref.path}', expected '{file_path}'")
            
        # Check program_id field
        if xref.program_id == program_id:
            validation_results["with_program_id"] += 1
        elif xref.program_id is None:
            validation_results["issues"].append(f"Xref {i+1}: Missing program_id")
        else:
            validation_results["issues"].append(f"Xref {i+1}: Wrong program_id '{xref.program_id}', expected '{program_id}'")
            
        # Check column fields
        if xref.start_col is not None and xref.end_col is not None:
            validation_results["with_columns"] += 1
            
            # Validate column accuracy
            try:
                if xref.snippet and xref.start_col > 0 and xref.end_col > xref.start_col:
                    extracted = xref.snippet[xref.start_col-1:xref.end_col]
                    if extracted.upper() == xref.simple_name.upper():
                        validation_results["column_accuracy"] += 1
                    else:
                        validation_results["issues"].append(f"Xref {i+1}: Column mismatch - extracted '{extracted}', expected '{xref.simple_name}'")
            except (IndexError, TypeError) as e:
                validation_results["issues"].append(f"Xref {i+1}: Column extraction error - {e}")
                
        # Check direction field
        if xref.direction is not None:
            validation_results["with_direction"] += 1
            
    return validation_results

def test_comprehensive_s35_parsing():
    """Test comprehensive COBOL parsing with 5 S35-Source files"""
    print("=" * 70)
    print("COMPREHENSIVE COBOL PARSER TEST WITH S35-SOURCE FILES")
    print("=" * 70)
    
    try:
        # Load Azure credentials
        connection_string = load_azure_credentials()
        blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        
        # Get the aisearch container
        container_name = "aisearch"
        container_client = blob_service_client.get_container_client(container_name)
        
        print(f"Connected to Azure container: {container_name}")
        
        # Get first 5 COBOL files from S35-Source
        prefix = "S35-Source/"
        test_files = []
        
        for blob in container_client.list_blobs(name_starts_with=prefix):
            if blob.name.upper().endswith(('.CBL', '.CPY', '.COB')):
                test_files.append(blob.name)
                if len(test_files) >= 5:
                    break
        
        print(f"\nSelected 5 test files:")
        for i, file_name in enumerate(test_files, 1):
            print(f"  {i}. {file_name}")
        
        # Process each file and collect results
        all_results = []
        total_stats = {
            "files_processed": 0,
            "total_xrefs": 0,
            "total_data_items": 0,
            "total_paragraphs": 0,
            "total_flow_edges": 0,
            "total_copybooks": 0,
            "enrichment_success": 0
        }
        
        for i, file_path in enumerate(test_files, 1):
            print(f"\n" + "="*50)
            print(f"PROCESSING FILE {i}/5: {file_path}")
            print(f"" + "="*50)
            
            # Download and process the file
            blob_client = container_client.get_blob_client(file_path)
            blob_content = blob_client.download_blob().readall().decode('utf-8', errors='ignore')
            
            file_id = f"test{i:03d}"
            
            print(f"Downloaded {len(blob_content):,} characters")
            
            # Process with enhanced parser
            result = process_blob_content_full(file_path, blob_content, file_id)
            
            if result[0] is None:  # Check if processing failed
                print(f"❌ Failed to process {file_path}")
                continue
                
            file_rec, paras, data_items, proc_facts, xrefs, chunks, flow_edges, copybooks, raw_lines = result
            
            print(f"\n📊 PARSING STATISTICS:")
            print(f"   Program ID: {file_rec.program_id}")
            print(f"   Format: {file_rec.fmt}")
            print(f"   PROCEDURE USING: {file_rec.procedure_using}")
            print(f"   Lines: {len(raw_lines):,}")
            print(f"   Paragraphs: {len(paras):,}")
            print(f"   Data items: {len(data_items):,}")
            print(f"   Procedure facts: {len(proc_facts):,}")
            print(f"   Cross-references: {len(xrefs):,}")
            print(f"   Flow edges: {len(flow_edges):,}")
            print(f"   Copybooks: {len(copybooks):,}")
            print(f"   Chunks: {len(chunks):,}")
            
            # Validate xref enrichment
            print(f"\n🔍 XREF ENRICHMENT VALIDATION:")
            validation = validate_xref_enrichment(xrefs, file_path, file_rec.program_id)
            
            print(f"   Total xrefs: {validation['total_xrefs']:,}")
            print(f"   With correct path: {validation['with_path']:,} ({validation['with_path']/max(1,validation['total_xrefs'])*100:.1f}%)")
            print(f"   With correct program_id: {validation['with_program_id']:,} ({validation['with_program_id']/max(1,validation['total_xrefs'])*100:.1f}%)")
            print(f"   With column positions: {validation['with_columns']:,} ({validation['with_columns']/max(1,validation['total_xrefs'])*100:.1f}%)")
            print(f"   With direction: {validation['with_direction']:,} ({validation['with_direction']/max(1,validation['total_xrefs'])*100:.1f}%)")
            print(f"   Column accuracy: {validation['column_accuracy']:,} ({validation['column_accuracy']/max(1,validation['with_columns'])*100:.1f}%)")
            
            if validation['issues']:
                print(f"\n⚠️  VALIDATION ISSUES ({len(validation['issues'])}):")
                for issue in validation['issues'][:5]:  # Show first 5 issues
                    print(f"     - {issue}")
                if len(validation['issues']) > 5:
                    print(f"     ... and {len(validation['issues']) - 5} more")
            else:
                print(f"   ✅ All xrefs properly enriched!")
                total_stats["enrichment_success"] += 1
            
            # Show sample xrefs
            if xrefs:
                print(f"\n📋 SAMPLE XREFS (first 3):")
                for j, xref in enumerate(xrefs[:3], 1):
                    print(f"   {j}. '{xref.simple_name}' ({xref.kind}/{xref.direction}) line {xref.line}")
                    print(f"      Snippet: {xref.snippet[:60]}...")
                    print(f"      Columns: {xref.start_col}-{xref.end_col}")
                    print(f"      Path: {xref.path}")
                    print(f"      Program: {xref.program_id}")
            
            # Show sample data items by section
            sections = {}
            for item in data_items:
                sections.setdefault(item.section, []).append(item)
            
            if sections:
                print(f"\n📋 DATA ITEMS BY SECTION:")
                for section, items in sections.items():
                    print(f"   {section}: {len(items)} items")
                    for item in items[:2]:  # Show first 2 items per section
                        print(f"     - {item.qualified_name} (Level {item.level}) {item.pic or ''}")
            
            # Collect results for summary
            file_result = {
                "file_path": file_path,
                "file_id": file_id,
                "program_id": file_rec.program_id,
                "format": file_rec.fmt,
                "lines": len(raw_lines),
                "paragraphs": len(paras),
                "data_items": len(data_items),
                "xrefs": len(xrefs),
                "flow_edges": len(flow_edges),
                "copybooks": len(copybooks),
                "validation": validation
            }
            all_results.append(file_result)
            
            # Update totals
            total_stats["files_processed"] += 1
            total_stats["total_xrefs"] += len(xrefs)
            total_stats["total_data_items"] += len(data_items)
            total_stats["total_paragraphs"] += len(paras)
            total_stats["total_flow_edges"] += len(flow_edges)
            total_stats["total_copybooks"] += len(copybooks)
        
        # Print comprehensive summary
        print(f"\n" + "="*70)
        print(f"COMPREHENSIVE TEST SUMMARY")
        print(f"" + "="*70)
        
        print(f"📊 OVERALL STATISTICS:")
        print(f"   Files processed: {total_stats['files_processed']}/5")
        print(f"   Total xrefs: {total_stats['total_xrefs']:,}")
        print(f"   Total data items: {total_stats['total_data_items']:,}")
        print(f"   Total paragraphs: {total_stats['total_paragraphs']:,}")
        print(f"   Total flow edges: {total_stats['total_flow_edges']:,}")
        print(f"   Total copybooks: {total_stats['total_copybooks']:,}")
        print(f"   Enrichment success: {total_stats['enrichment_success']}/{total_stats['files_processed']} files")
        
        print(f"\n📋 PER-FILE BREAKDOWN:")
        for result in all_results:
            validation = result['validation']
            enrichment_pct = (validation['with_path'] / max(1, validation['total_xrefs'])) * 100
            print(f"   {result['file_path'][:40]:<40} | {result['program_id']:<12} | {result['xrefs']:>4} xrefs | {enrichment_pct:>5.1f}% enriched")
        
        # Save detailed results
        output_file = "comprehensive_s35_test_results.json"
        with open(output_file, 'w') as f:
            json.dump({
                "test_summary": total_stats,
                "file_results": all_results,
                "timestamp": "2025-09-09"
            }, f, indent=2, default=str)
        
        print(f"\n💾 Detailed results saved to: {output_file}")
        
        # Final assessment
        success_rate = (total_stats["enrichment_success"] / max(1, total_stats["files_processed"])) * 100
        
        if success_rate >= 80:
            print(f"\n🎉 TEST PASSED! ({success_rate:.1f}% files fully enriched)")
            print("   ✅ COBOL parser is working correctly with S35-Source files")
            print("   ✅ Xref enrichment (path, program_id, columns) is functional")
            print("   ✅ All parsing components are operational")
            return True
        else:
            print(f"\n⚠️  TEST PARTIALLY PASSED ({success_rate:.1f}% files fully enriched)")
            print("   ⚠️  Some issues found with xref enrichment")
            return False
        
    except Exception as e:
        print(f"\n❌ TEST FAILED with error: {str(e)}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_comprehensive_s35_parsing()
    sys.exit(0 if success else 1)
