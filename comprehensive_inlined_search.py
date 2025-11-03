#!/usr/bin/env python3
"""
Comprehensive search for 'inlined' files across all blob containers
"""

import json
import os
from azure.storage.blob import BlobServiceClient

def load_local_settings():
    """Load configuration from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except FileNotFoundError:
        print("❌ local.settings.json not found")
        return {}
    except Exception as e:
        print(f"❌ Error loading local.settings.json: {e}")
        return {}

def search_all_containers_for_inlined():
    """Search all blob containers for files with 'inlined' in their names"""
    
    settings = load_local_settings()
    storage_connection_string = (
        settings.get("AZURE_STORAGE_CONNECTION_STRING") or 
        settings.get("DEPLOYMENT_STORAGE_CONNECTION_STRING") or
        settings.get("AzureWebJobsStorage")
    )
    
    if not storage_connection_string:
        print("❌ No storage connection string found")
        return
    
    try:
        blob_service_client = BlobServiceClient.from_connection_string(storage_connection_string)
        
        print("🔍 COMPREHENSIVE SEARCH FOR 'INLINED' FILES")
        print("=" * 70)
        
        # Get all containers
        containers = list(blob_service_client.list_containers())
        print(f"📦 Found {len(containers)} containers to search")
        
        all_inlined_files = []
        s35_inlined_files = []
        
        for container in containers:
            container_name = container.name
            print(f"\n🔍 Searching container: {container_name}")
            
            try:
                container_client = blob_service_client.get_container_client(container_name)
                container_inlined_files = []
                
                for blob in container_client.list_blobs():
                    blob_name = blob.name.lower()
                    
                    # Check for 'inlined' in filename (case insensitive)
                    if "inlined" in blob_name:
                        file_info = {
                            'container': container_name,
                            'name': blob.name,
                            'size': blob.size,
                            'last_modified': blob.last_modified
                        }
                        container_inlined_files.append(file_info)
                        all_inlined_files.append(file_info)
                        
                        print(f"  ✅ Found: {blob.name}")
                        print(f"     Size: {blob.size:,} bytes")
                        print(f"     Modified: {blob.last_modified}")
                        
                        # Check if it's S35-Source related
                        if "s35" in blob_name or "s35-source" in blob_name:
                            s35_inlined_files.append(file_info)
                            print(f"     📁 S35-Source related!")
                
                if not container_inlined_files:
                    print(f"  ❌ No 'inlined' files found in {container_name}")
                
            except Exception as e:
                print(f"  ❌ Error accessing container {container_name}: {e}")
        
        print("\n" + "=" * 70)
        print("📊 FINAL SUMMARY")
        print("=" * 70)
        print(f"Total containers searched: {len(containers)}")
        print(f"Total 'inlined' files found: {len(all_inlined_files)}")
        print(f"S35-Source related 'inlined' files: {len(s35_inlined_files)}")
        
        if all_inlined_files:
            print(f"\n📁 ALL 'INLINED' FILES FOUND:")
            for i, file_info in enumerate(all_inlined_files, 1):
                print(f"  {i}. {file_info['container']}/{file_info['name']}")
                print(f"     Size: {file_info['size']:,} bytes")
                print(f"     Modified: {file_info['last_modified']}")
                print()
        
        if s35_inlined_files:
            print(f"📁 S35-SOURCE RELATED 'INLINED' FILES:")
            for file_info in s35_inlined_files:
                print(f"  • {file_info['container']}/{file_info['name']}")
        
        # Additional search patterns
        print(f"\n🔍 SEARCHING FOR ADDITIONAL PATTERNS...")
        additional_patterns = ['inline', 'Inline', 'INLINE']
        
        for pattern in additional_patterns:
            print(f"\nSearching for '{pattern}':")
            found_additional = False
            
            for container in containers:
                try:
                    container_client = blob_service_client.get_container_client(container.name)
                    for blob in container_client.list_blobs():
                        if pattern in blob.name:
                            print(f"  ✅ {container.name}/{blob.name}")
                            found_additional = True
                except:
                    continue
            
            if not found_additional:
                print(f"  ❌ No files found with '{pattern}'")
                
    except Exception as e:
        print(f"❌ Error accessing blob storage: {e}")

if __name__ == "__main__":
    search_all_containers_for_inlined()
