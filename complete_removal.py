#!/usr/bin/env python3
"""
Complete removal of inlined COBOL files from Azure AI Search index
This version handles pagination to remove ALL documents for each file
"""

import json
import requests
from typing import List, Dict
import time

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

class AzureSearchCleaner:
    """Class to handle complete removal of specific documents from Azure AI Search"""
    
    def __init__(self):
        settings = load_local_settings()
        self.search_endpoint = settings.get("SEARCH_ENDPOINT")
        self.search_key = settings.get("SEARCH_KEY") 
        self.index_name = "cobol-index"
        
        if not self.search_endpoint or not self.search_key:
            raise ValueError("Missing SEARCH_ENDPOINT or SEARCH_KEY in local.settings.json")
    
    def search_all_documents_by_file(self, filename_pattern: str) -> List[Dict]:
        """Search for ALL documents matching a filename pattern (with pagination)"""
        url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-07-01-preview"
        
        headers = {
            "Content-Type": "application/json",
            "api-key": self.search_key
        }
        
        all_documents = []
        skip = 0
        batch_size = 1000
        
        while True:
            # Search for documents containing the filename in repo_path
            search_body = {
                "search": f'"{filename_pattern}"',
                "searchFields": "repo_path",
                "searchMode": "all",
                "top": batch_size,
                "skip": skip,
                "select": "id,repo_path,line,code"
            }
            
            try:
                response = requests.post(url, headers=headers, json=search_body)
                if response.status_code == 200:
                    result = response.json()
                    batch_documents = result.get('value', [])
                    
                    if not batch_documents:
                        # No more documents
                        break
                    
                    all_documents.extend(batch_documents)
                    
                    if len(batch_documents) < batch_size:
                        # Last batch
                        break
                    
                    skip += batch_size
                    print(f"  📥 Retrieved {len(all_documents)} documents so far...")
                    
                else:
                    print(f"❌ Search failed for {filename_pattern}: {response.status_code} - {response.text}")
                    break
                    
            except Exception as e:
                print(f"❌ Search error for {filename_pattern}: {e}")
                break
        
        return all_documents
    
    def delete_documents_by_ids_batch(self, document_ids: List[str], batch_size: int = 1000) -> bool:
        """Delete documents by their IDs in batches"""
        if not document_ids:
            return True
            
        url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/index?api-version=2023-07-01-preview"
        
        headers = {
            "Content-Type": "application/json",
            "api-key": self.search_key
        }
        
        total_deleted = 0
        
        # Process in batches
        for i in range(0, len(document_ids), batch_size):
            batch_ids = document_ids[i:i + batch_size]
            
            # Prepare batch delete request
            actions = []
            for doc_id in batch_ids:
                actions.append({
                    "@search.action": "delete",
                    "id": doc_id
                })
            
            delete_body = {"value": actions}
            
            try:
                response = requests.post(url, headers=headers, json=delete_body)
                if response.status_code == 200:
                    result = response.json()
                    
                    # Check for any failures in this batch
                    batch_success = 0
                    for item in result.get('value', []):
                        if item.get('status'):
                            batch_success += 1
                        else:
                            print(f"❌ Failed to delete {item.get('key')}: {item.get('errorMessage')}")
                    
                    total_deleted += batch_success
                    print(f"  🗑️  Deleted batch: {batch_success}/{len(batch_ids)} documents (Total: {total_deleted})")
                    
                    # Small delay between batches to avoid rate limiting
                    if i + batch_size < len(document_ids):
                        time.sleep(1)
                        
                else:
                    print(f"❌ Delete batch failed: {response.status_code} - {response.text}")
                    return False
                    
            except Exception as e:
                print(f"❌ Delete batch error: {e}")
                return False
        
        print(f"✅ Total successfully deleted: {total_deleted}/{len(document_ids)} documents")
        return True

def complete_removal():
    """Completely remove all documents for the specified inlined files"""
    
    # Files to remove
    files_to_remove = [
        "LONPF2_Inlined.CBL",
        "LONPFC_Inlined.CBL", 
        "LP__LONPF2_Inlined.CBL.jsonl",
        "LP__LONPFC_Inlined.CBL.jsonl"
    ]
    
    print("🗑️  COMPLETE REMOVAL OF INLINED COBOL FILES")
    print("=" * 60)
    print("This version will remove ALL documents (with pagination)")
    print("Files to remove:")
    for file in files_to_remove:
        print(f"  • {file}")
    print()
    
    # Confirm with user
    confirm = input("⚠️  This will permanently delete ALL these files from the search index. Continue? (yes/no): ")
    if confirm.lower() not in ['yes', 'y']:
        print("❌ Operation cancelled")
        return
    
    try:
        cleaner = AzureSearchCleaner()
        total_deleted = 0
        
        for filename in files_to_remove:
            print(f"\n🔍 Processing: {filename}")
            
            # Find ALL documents for this file (with pagination)
            documents = cleaner.search_all_documents_by_file(filename)
            
            if not documents:
                print(f"  ℹ️  No documents found for {filename}")
                continue
            
            print(f"  ✅ Found {len(documents)} total documents to delete")
            
            # Show sample of what will be deleted
            if len(documents) > 0:
                print("  📄 Sample documents:")
                for i, doc in enumerate(documents[:3]):
                    repo_path = doc.get('repo_path', 'unknown')
                    line = doc.get('line', 'unknown')
                    code_preview = doc.get('code', 'no code')[:50]
                    print(f"    {i+1}. {repo_path}:{line} - {code_preview}...")
                
                if len(documents) > 3:
                    print(f"    ... and {len(documents) - 3} more documents")
            
            # Extract document IDs
            doc_ids = [doc['id'] for doc in documents if 'id' in doc]
            
            if doc_ids:
                print(f"  🗑️  Deleting {len(doc_ids)} documents in batches...")
                success = cleaner.delete_documents_by_ids_batch(doc_ids)
                
                if success:
                    print(f"  ✅ Successfully processed {filename}")
                    total_deleted += len(doc_ids)
                else:
                    print(f"  ❌ Failed to delete documents for {filename}")
            else:
                print(f"  ⚠️  No document IDs found for {filename}")
        
        print(f"\n" + "=" * 60)
        print(f"📊 FINAL SUMMARY")
        print(f"Total documents deleted: {total_deleted}")
        
        if total_deleted > 0:
            print(f"\n✅ Complete removal successful!")
            print(f"ℹ️  All inlined file data has been removed from the search index")
            print(f"ℹ️  Run verify_removal.py to confirm the cleanup")
        else:
            print(f"\nℹ️  No documents were deleted")
            
    except Exception as e:
        print(f"❌ Error during complete cleanup: {e}")

if __name__ == "__main__":
    complete_removal()
