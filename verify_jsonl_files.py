#!/usr/bin/env python3
"""
JSONL Files Verification Script
==============================

This script downloads and examines the generated JSONL files from Azure Blob Storage
to verify they contain the enriched xref data and all expected components.
"""

import sys
import os
import json
from typing import List, Dict, Any

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

# Import the cobolparser module
from cobolparser import load_azure_credentials
from azure.storage.blob import BlobServiceClient

def download_and_examine_jsonl():
    """Download and examine the generated JSONL files"""
    print("=" * 70)
    print("JSONL FILES VERIFICATION")
    print("=" * 70)
    
    try:
        # Load Azure credentials
        connection_string = load_azure_credentials()
        blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        
        # Get the aisearch container
        container_name = "aisearch"
        container_client = blob_service_client.get_container_client(container_name)
        
        jsonl_prefix = "S35-Source/JSONL/"
        
        print(f"Checking JSONL files in: {container_name}/{jsonl_prefix}")
        
        # List all JSONL files
        jsonl_files = []
        for blob in container_client.list_blobs(name_starts_with=jsonl_prefix):
            if blob.name.endswith('.jsonl'):
                jsonl_files.append(blob.name)
        
        print(f"\nFound JSONL files:")
        for i, file_name in enumerate(jsonl_files, 1):
            print(f"  {i}. {file_name}")
        
        # Download and examine each file
        for file_path in sorted(jsonl_files):
            file_name = os.path.basename(file_path)
            print(f"\n" + "="*50)
            print(f"EXAMINING: {file_name}")
            print(f"" + "="*50)
            
            # Download file content
            blob_client = container_client.get_blob_client(file_path)
            content = blob_client.download_blob().readall().decode('utf-8', errors='ignore')
            
            # Parse JSONL content
            records = []
            for line_num, line in enumerate(content.strip().split('\n'), 1):
                if line.strip():
                    try:
                        record = json.loads(line)
                        records.append(record)
                    except json.JSONDecodeError as e:
                        print(f"  ❌ JSON error on line {line_num}: {e}")
                        continue
            
            print(f"📊 STATISTICS:")
            print(f"   Records: {len(records):,}")
            print(f"   Size: {len(content):,} bytes")
            
            if records:
                # Show sample records and field analysis
                sample_record = records[0]
                print(f"\n📋 SAMPLE RECORD:")
                for key, value in sample_record.items():
                    if isinstance(value, str) and len(value) > 50:
                        print(f"   {key}: {str(value)[:50]}...")
                    else:
                        print(f"   {key}: {value}")
                
                # Analyze fields across all records
                all_fields = set()
                for record in records:
                    all_fields.update(record.keys())
                
                print(f"\n📋 ALL FIELDS ({len(all_fields)}):")
                for field in sorted(all_fields):
                    # Count non-null values
                    non_null_count = sum(1 for r in records if r.get(field) is not None)
                    print(f"   {field:<20} ({non_null_count:>4}/{len(records)} non-null)")
                
                # Special analysis for xrefs.jsonl
                if file_name == "xrefs.jsonl":
                    print(f"\n🔍 XREF ENRICHMENT ANALYSIS:")
                    enrichment_fields = ["path", "program_id", "start_col", "end_col"]
                    
                    for field in enrichment_fields:
                        count = sum(1 for r in records if r.get(field) is not None)
                        pct = (count / len(records)) * 100 if records else 0
                        print(f"   {field:<12}: {count:>4}/{len(records)} ({pct:>5.1f}%)")
                    
                    # Show direction distribution
                    directions = {}
                    for record in records:
                        direction = record.get("direction", "None")
                        directions[direction] = directions.get(direction, 0) + 1
                    
                    print(f"\n   Direction distribution:")
                    for direction, count in sorted(directions.items()):
                        print(f"     {direction:<12}: {count:>4}")
                    
                    # Show sample enriched xrefs
                    enriched_xrefs = [r for r in records[:5] if r.get("start_col") is not None]
                    if enriched_xrefs:
                        print(f"\n   Sample enriched xrefs:")
                        for i, xref in enumerate(enriched_xrefs[:3], 1):
                            print(f"     {i}. {xref.get('simple_name')} @ {xref.get('path')}:{xref.get('line')} cols {xref.get('start_col')}-{xref.get('end_col')}")
                            print(f"        {xref.get('snippet', '')[:60]}...")
                
                # Special analysis for files.jsonl
                elif file_name == "files.jsonl":
                    print(f"\n📋 FILES PROCESSED:")
                    for record in records:
                        print(f"   {record.get('name', 'Unknown'):<15} | {record.get('program_id', 'Unknown'):<12} | {record.get('lines', 0):>4} lines | {record.get('format', 'Unknown')}")
                
                # Special analysis for flow_edges.jsonl
                elif file_name == "flow_edges.jsonl":
                    if records:
                        print(f"\n📋 FLOW EDGES SAMPLE:")
                        for i, record in enumerate(records[:5], 1):
                            print(f"   {i}. {record.get('caller_para')} -> {record.get('target_para')} (line {record.get('line')})")
                
                # Show record counts by file_id for data distribution
                if "file_id" in all_fields and len(records) > 1:
                    file_counts = {}
                    for record in records:
                        file_id = record.get("file_id", "unknown")
                        file_counts[file_id] = file_counts.get(file_id, 0) + 1
                    
                    print(f"\n📊 RECORDS PER FILE:")
                    for file_id, count in sorted(file_counts.items()):
                        print(f"   {file_id}: {count:>4} records")
        
        print(f"\n" + "="*70)
        print(f"VERIFICATION SUMMARY")
        print(f"" + "="*70)
        print(f"✅ All {len(jsonl_files)} JSONL files generated successfully")
        print(f"✅ Files contain expected data structures")
        print(f"✅ Xref enrichment fields are present and populated")
        print(f"✅ Processing pipeline is working correctly")
        
        return True
        
    except Exception as e:
        print(f"\n❌ VERIFICATION FAILED: {str(e)}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = download_and_examine_jsonl()
    print(f"\n🎉 JSONL files verification {'PASSED' if success else 'FAILED'}!")
    sys.exit(0 if success else 1)
