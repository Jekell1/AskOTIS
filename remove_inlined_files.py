#!/usr/bin/env python3
"""
Remove specific inlined COBOL files from Azure AI Search index
This script will delete all documents related to the inlined versions to avoid duplicates
"""

import json
import requests
from typing import List, Dict

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
    """Class to handle removal of specific documents from Azure AI Search"""
    
    def __init__(self):
        settings = load_local_settings()
        self.search_endpoint = settings.get("SEARCH_ENDPOINT")
        self.search_key = settings.get("SEARCH_KEY") 
        self.index_name = "cobol-index"
        
        if not self.search_endpoint or not self.search_key:
            raise ValueError("Missing SEARCH_ENDPOINT or SEARCH_KEY in local.settings.json")
    
    def search_documents_by_file(self, filename_pattern: str) -> List[Dict]:
        """Search for documents matching a filename pattern"""
        url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/search?api-version=2023-07-01-preview"
        
        headers = {
            "Content-Type": "application/json",
            "api-key": self.search_key
        }
        
        # Search for documents containing the filename in repo_path
        search_body = {
            "search": f'"{filename_pattern}"',
            "searchFields": "repo_path",
            "searchMode": "all",
            "top": 1000,  # Get all matches
            "select": "id,repo_path,line,code"
        }
        
        try:
            response = requests.post(url, headers=headers, json=search_body)
            if response.status_code == 200:
                result = response.json()
                return result.get('value', [])
            else:
                print(f"❌ Search failed for {filename_pattern}: {response.status_code} - {response.text}")
                return []
        except Exception as e:
            print(f"❌ Search error for {filename_pattern}: {e}")
            return []
    
    def delete_documents_by_ids(self, document_ids: List[str]) -> bool:
        """Delete documents by their IDs"""
        if not document_ids:
            return True
            
        url = f"{self.search_endpoint}/indexes/{self.index_name}/docs/index?api-version=2023-07-01-preview"
        
        headers = {
            "Content-Type": "application/json",
            "api-key": self.search_key
        }
        
        # Prepare batch delete request
        actions = []
        for doc_id in document_ids:
            actions.append({
                "@search.action": "delete",
                "id": doc_id
            })
        
        delete_body = {"value": actions}
        
        try:
            response = requests.post(url, headers=headers, json=delete_body)
            if response.status_code == 200:
                result = response.json()
                print(f"✅ Delete operation completed")
                
                # Check for any failures
                for item in result.get('value', []):
                    if not item.get('status'):
                        print(f"❌ Failed to delete {item.get('key')}: {item.get('errorMessage')}")
                    
                return True
            else:
                print(f"❌ Delete failed: {response.status_code} - {response.text}")
                return False
        except Exception as e:
            print(f"❌ Delete error: {e}")
            return False

def remove_inlined_files():
    """Remove all documents for the specified inlined files"""
    
    # Files to remove
    files_to_remove = [
        "LONPF2_Inlined.CBL",
        "LONPFC_Inlined.CBL", 
        "LP__LONPF2_Inlined.CBL.jsonl",
        "LP__LONPFC_Inlined.CBL.jsonl"
    ]
    
    print("🗑️  REMOVING INLINED COBOL FILES FROM SEARCH INDEX")
    print("=" * 60)
    print("Files to remove:")
    for file in files_to_remove:
        print(f"  • {file}")
    print()
    
    # Confirm with user
    confirm = input("⚠️  This will permanently delete these files from the search index. Continue? (yes/no): ")
    if confirm.lower() not in ['yes', 'y']:
        print("❌ Operation cancelled")
        return
    
    try:
        cleaner = AzureSearchCleaner()
        total_deleted = 0
        
        for filename in files_to_remove:
            print(f"\n🔍 Searching for documents matching: {filename}")
            
            # Find all documents for this file
            documents = cleaner.search_documents_by_file(filename)
            
            if not documents:
                print(f"  ℹ️  No documents found for {filename}")
                continue
            
            print(f"  ✅ Found {len(documents)} documents to delete")
            
            # Show sample of what will be deleted
            if documents:
                print("  📄 Sample documents:")
                for i, doc in enumerate(documents[:3]):
                    repo_path = doc.get('repo_path', 'unknown')
                    line = doc.get('line', 'unknown')
                    code_preview = doc.get('code', 'no code')[:60]
                    print(f"    {i+1}. {repo_path}:{line} - {code_preview}...")
                
                if len(documents) > 3:
                    print(f"    ... and {len(documents) - 3} more documents")
            
            # Extract document IDs
            doc_ids = [doc['id'] for doc in documents if 'id' in doc]
            
            if doc_ids:
                print(f"  🗑️  Deleting {len(doc_ids)} documents...")
                success = cleaner.delete_documents_by_ids(doc_ids)
                
                if success:
                    print(f"  ✅ Successfully deleted {len(doc_ids)} documents for {filename}")
                    total_deleted += len(doc_ids)
                else:
                    print(f"  ❌ Failed to delete documents for {filename}")
            else:
                print(f"  ⚠️  No document IDs found for {filename}")
        
        print(f"\n" + "=" * 60)
        print(f"📊 SUMMARY")
        print(f"Total documents deleted: {total_deleted}")
        
        if total_deleted > 0:
            print(f"\n✅ Successfully removed inlined file data from the search index!")
            print(f"ℹ️  The search index now contains only the original (non-inlined) versions")
            print(f"ℹ️  You may want to run test_cobol_search.py to verify the changes")
        else:
            print(f"\nℹ️  No documents were deleted (files may not have been in the index)")
            
    except Exception as e:
        print(f"❌ Error during cleanup: {e}")

if __name__ == "__main__":
    remove_inlined_files()
