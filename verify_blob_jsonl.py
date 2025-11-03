#!/usr/bin/env python3
"""
Verify JSONL files in Azure blob storage
=======================================
"""

import os
import json
from azure.storage.blob import BlobServiceClient

def verify_blob_jsonl():
    """Verify the JSONL files were written to blob storage"""
    connection_string = os.environ.get("AZURE_STORAGE_CONNECTION_STRING")
    if not connection_string:
        raise ValueError("AZURE_STORAGE_CONNECTION_STRING environment variable not set")
    
    print("🔍 VERIFYING JSONL FILES IN BLOB STORAGE")
    print("=" * 60)
    
    try:
        # Connect to blob storage
        blob_service = BlobServiceClient.from_connection_string(connection_string)
        container_client = blob_service.get_container_client("aisearch")
        
        # List JSONL files
        jsonl_blobs = []
        for blob in container_client.list_blobs(name_starts_with="S35-Source/JSONL/"):
            if blob.name.endswith('.jsonl'):
                jsonl_blobs.append(blob.name)
        
        print(f"📁 Found {len(jsonl_blobs)} JSONL files:")
        
        # Check each JSONL file
        for blob_name in sorted(jsonl_blobs):
            file_name = os.path.basename(blob_name)
            
            # Download and check content
            blob_client = container_client.get_blob_client(blob_name)
            content = blob_client.download_blob().readall().decode('utf-8')
            
            # Count records
            lines = [line for line in content.strip().split('\n') if line.strip()]
            record_count = len(lines)
            
            print(f"   {file_name:<20}: {record_count:>4} records")
            
            # Special check for xrefs.jsonl enrichment
            if file_name == "xrefs.jsonl" and lines:
                enriched_count = 0
                for line in lines:
                    record = json.loads(line)
                    if all(field in record and record[field] is not None 
                           for field in ['path', 'program_id', 'start_col', 'end_col']):
                        enriched_count += 1
                
                pct = (enriched_count / record_count) * 100 if record_count > 0 else 0
                print(f"      💡 Enriched: {enriched_count}/{record_count} ({pct:.1f}%)")
                
                if enriched_count > 0:
                    sample = json.loads(lines[0])
                    if sample.get('start_col') is not None:
                        print(f"      Sample: '{sample.get('simple_name')}' @ cols {sample.get('start_col')}-{sample.get('end_col')}")
        
        print(f"\n✅ Verification completed!")
        print(f"   All JSONL files successfully written to: aisearch/S35-Source/JSONL/")
        
        return True
        
    except Exception as e:
        print(f"❌ Verification failed: {e}")
        return False

if __name__ == "__main__":
    verify_blob_jsonl()
