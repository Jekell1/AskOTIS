#!/usr/bin/env python3
"""
COBOL COPY Statement Extractor

A lightweight program that scans COBOL files and extracts only COPY statements,
writing them to copybooks.jsonl for dependency mapping.

Features:
- Supports both local filesystem and Azure Blob storage
- Extracts COPY statements with quoted and unquoted syntax
- Minimal memory footprint with streaming JSONL output
- Progress tracking and summary reporting

Usage:
    Local:      python cobol-copybook-extractor.py --local /path/to/folder
    Azure Blob: python cobol-copybook-extractor.py --container aisearch --prefix S35-Source/

Options:
    --max-files N           Process at most N files (0 = no limit)
    --connection-string S   Override Azure connection string
"""
from __future__ import annotations
import argparse
import json
import os
import re
import sys
from dataclasses import dataclass
from typing import List, Optional

# Optional Azure import only when needed
try:
    from azure.storage.blob import BlobServiceClient
except ImportError:
    BlobServiceClient = None

@dataclass
class CopybookRec:
    file_id: str
    parent_path: str
    copybook_name: str
    line: int
    replacing_clause: Optional[str] = None

# COBOL file extensions
COBOL_EXTS = {".cbl", ".cob", ".cobol", ".cpy", ".CBL", ".COB", ".CPY"}

# Regex for COPY statements - supports both quoted and unquoted syntax
RE_COPY = re.compile(r"^\s*COPY\s+(?:['\"]([^'\"]+)['\"]|([A-Z0-9\-]+))(?:\s+REPLACING\s+(.*?))?\s*\.\s*$", re.IGNORECASE)

def extract_copy_statements(file_id: str, parent_path: str, text: str) -> List[CopybookRec]:
    """Extract COPY statements from COBOL text."""
    copybooks: List[CopybookRec] = []
    lines = text.split('\n')
    
    for line_no, line in enumerate(lines, 1):
        cm = RE_COPY.match(line.strip())
        if cm:
            # Handle both quoted and unquoted COPY statements
            quoted_name = cm.group(1)
            unquoted_name = cm.group(2)
            replacing_clause = cm.group(3)
            
            copybook_name = quoted_name or unquoted_name
            if copybook_name:
                # Strip path and .CPY extension for consistency
                if '/' in copybook_name or '\\' in copybook_name:
                    copybook_name = os.path.basename(copybook_name)
                if copybook_name.upper().endswith('.CPY'):
                    copybook_name = copybook_name[:-4]
                
                copybooks.append(CopybookRec(
                    file_id=file_id,
                    parent_path=parent_path,
                    copybook_name=copybook_name,
                    line=line_no,
                    replacing_clause=replacing_clause.strip() if replacing_clause else None
                ))
    
    return copybooks

def write_jsonl(filepath: str, records: List[dict]):
    """Write records to a JSONL file."""
    os.makedirs(os.path.dirname(filepath), exist_ok=True)
    with open(filepath, 'w', encoding='utf-8') as f:
        for record in records:
            f.write(json.dumps(record, ensure_ascii=False) + '\n')

def append_jsonl(filepath: str, records: List[dict]):
    """Append records to a JSONL file."""
    os.makedirs(os.path.dirname(filepath), exist_ok=True)
    with open(filepath, 'a', encoding='utf-8') as f:
        for record in records:
            f.write(json.dumps(record, ensure_ascii=False) + '\n')

def read_local_settings() -> Optional[str]:
    """Read Azure connection string from local.settings.json."""
    try:
        with open("local.settings.json", "r") as f:
            data = json.load(f)
            return (data.get("Values", {}).get("AzureWebJobsStorage") or 
                   data.get("Values", {}).get("DEPLOYMENT_STORAGE_CONNECTION_STRING"))
    except (FileNotFoundError, json.JSONDecodeError, KeyError):
        return None

def get_blob_service(connection_string: Optional[str]):
    """Get Azure Blob Service Client with connection string from various sources."""
    if BlobServiceClient is None:
        raise RuntimeError("azure-storage-blob is not installed. pip install azure-storage-blob")
    
    # Priority order: command line args, local.settings.json, environment variable
    if connection_string:
        return BlobServiceClient.from_connection_string(connection_string)
    
    # Try local.settings.json
    local_conn_str = read_local_settings()
    if local_conn_str:
        print("Using connection string from local.settings.json")
        return BlobServiceClient.from_connection_string(local_conn_str)
    
    # Fall back to environment variable
    env = os.getenv("AZURE_STORAGE_CONNECTION_STRING")
    if env:
        return BlobServiceClient.from_connection_string(env)
        
    raise RuntimeError("No Azure Storage connection found. Add to local.settings.json, set AZURE_STORAGE_CONNECTION_STRING, or pass --connection-string")

def is_cobol_file(name: str) -> bool:
    """Check if file has COBOL extension."""
    return os.path.splitext(name)[1] in COBOL_EXTS

def process_local_folder(folder_path: str, max_files: int = 0) -> List[dict]:
    """Process COBOL files in a local folder and extract COPY statements."""
    print(f"Scanning local folder: {folder_path}")
    
    copybooks_out = []
    files_processed = 0
    
    for root, dirs, files in os.walk(folder_path):
        # Skip JSONL directories
        dirs[:] = [d for d in dirs if d.upper() != 'JSONL']
        
        for file in files:
            if not is_cobol_file(file):
                continue
                
            if max_files > 0 and files_processed >= max_files:
                break
                
            file_path = os.path.join(root, file)
            rel_path = os.path.relpath(file_path, folder_path)
            
            try:
                with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
                    text = f.read()
                
                file_id = rel_path.replace('\\', '/')
                parent_path = os.path.dirname(rel_path).replace('\\', '/')
                
                copybooks = extract_copy_statements(file_id, parent_path, text)
                
                for cb in copybooks:
                    copybooks_out.append({
                        "file_id": cb.file_id,
                        "parent_path": cb.parent_path,
                        "copybook_name": cb.copybook_name,
                        "line": cb.line,
                        "replacing_clause": cb.replacing_clause
                    })
                
                files_processed += 1
                if files_processed % 100 == 0:
                    print(f"  Processed {files_processed} files, found {len(copybooks_out)} COPY statements")
                    
            except Exception as e:
                print(f"  Error processing {file_path}: {e}")
        
        if max_files > 0 and files_processed >= max_files:
            break
    
    return copybooks_out

def process_azure_blob(container_name: str, prefix: str, connection_string: Optional[str], max_files: int = 0) -> List[dict]:
    """Process COBOL files in Azure Blob storage and extract COPY statements."""
    print(f"Scanning Azure Blob container: {container_name}, prefix: {prefix}")
    
    bsc = get_blob_service(connection_string)
    cc = bsc.get_container_client(container_name)
    
    copybooks_out = []
    files_processed = 0
    batch_size = 20000  # Flush every 20K records
    first_upload = True  # Track if this is the first upload
    
    # Clean up prefix
    prefix = prefix.lstrip("/") if prefix else ""
    
    # List and process blobs
    for blob in cc.list_blobs(name_starts_with=prefix):
        name = blob.name
        if blob.size == 0:
            continue
            
        # Skip JSONL directories
        if '/jsonl/' in name.lower() or name.lower().endswith('/jsonl'):
            continue
            
        if not is_cobol_file(name):
            continue
            
        if max_files > 0 and files_processed >= max_files:
            break
        
        try:
            blob_client = cc.get_blob_client(name)
            text = blob_client.download_blob().readall().decode('utf-8', errors='replace')
            
            file_id = name
            parent_path = os.path.dirname(name)
            
            copybooks = extract_copy_statements(file_id, parent_path, text)
            
            for cb in copybooks:
                copybooks_out.append({
                    "file_id": cb.file_id,
                    "parent_path": cb.parent_path,
                    "copybook_name": cb.copybook_name,
                    "line": cb.line,
                    "replacing_clause": cb.replacing_clause
                })
            
            files_processed += 1
            if files_processed % 100 == 0:
                print(f"  Processed {files_processed} files, found {len(copybooks_out)} COPY statements")
            
            # Stream to Azure if we have enough records
            if len(copybooks_out) >= batch_size:
                print(f"  Flushing {len(copybooks_out)} copybook records to Azure...")
                # Always write to the main JSONL directory (remove subdirectory paths)
                main_prefix = prefix.split('/')[0] if '/' in prefix else prefix
                upload_copybooks_to_azure(cc, f"{main_prefix}/JSONL/copybooks.jsonl", copybooks_out, append=not first_upload)
                copybooks_out = []
                first_upload = False
                
        except Exception as e:
            print(f"  Error processing {name}: {e}")
    
    return copybooks_out

def upload_copybooks_to_azure(container_client, blob_path: str, records: List[dict], append: bool = False):
    """Upload copybook records to Azure Blob as JSONL."""
    if not records:
        return
        
    jsonl_content = '\n'.join(json.dumps(record, ensure_ascii=False) for record in records) + '\n'
    
    if append:
        # Download existing content and append
        try:
            existing_blob = container_client.get_blob_client(blob_path)
            existing_content = existing_blob.download_blob().readall().decode('utf-8')
            jsonl_content = existing_content + jsonl_content
        except Exception:
            # File doesn't exist yet, that's fine
            pass
    
    container_client.upload_blob(
        name=blob_path,
        data=jsonl_content.encode('utf-8'),
        overwrite=True
    )

def main():
    parser = argparse.ArgumentParser(description="Extract COPY statements from COBOL files")
    
    mode_group = parser.add_mutually_exclusive_group(required=True)
    mode_group.add_argument("--local", help="Local folder path to process")
    mode_group.add_argument("--container", help="Azure Blob container name")
    
    parser.add_argument("--prefix", help="Azure Blob prefix/folder (for container mode)")
    parser.add_argument("--max-files", type=int, default=0, help="Maximum files to process (0 = no limit)")
    parser.add_argument("--connection-string", help="Azure Storage connection string")
    
    args = parser.parse_args()
    
    if args.container and not args.prefix:
        print("Error: --prefix is required when using --container")
        sys.exit(1)
    
    print("COBOL COPY Statement Extractor")
    print("=" * 40)
    
    try:
        if args.local:
            # Local mode
            copybooks_out = process_local_folder(args.local, args.max_files)
            
            # Write to local JSONL file
            jsonl_dir = os.path.join(args.local, "JSONL")
            output_file = os.path.join(jsonl_dir, "copybooks.jsonl")
            write_jsonl(output_file, copybooks_out)
            
            print(f"\nResults written to: {output_file}")
            print(f"Total COPY statements found: {len(copybooks_out)}")
            
        else:
            # Azure Blob mode
            copybooks_out = process_azure_blob(args.container, args.prefix, args.connection_string, args.max_files)
            
            # Upload final batch if any remaining
            if copybooks_out:
                bsc = get_blob_service(args.connection_string)
                cc = bsc.get_container_client(args.container)
                # Always write to the main JSONL directory (remove subdirectory paths)
                main_prefix = args.prefix.split('/')[0] if '/' in args.prefix else args.prefix
                blob_path = f"{main_prefix}/JSONL/copybooks.jsonl"
                print(f"Uploading final {len(copybooks_out)} copybook records to Azure...")
                upload_copybooks_to_azure(cc, blob_path, copybooks_out, append=True)
            
            print(f"\nResults written to: {args.container}/{main_prefix}/JSONL/copybooks.jsonl")
            print(f"Final batch COPY statements: {len(copybooks_out)}")
    
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
