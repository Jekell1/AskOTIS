#!/usr/bin/env python3
"""
Preview what inlined COBOL files would be removed from Azure AI Search index
This script shows what would be deleted without actually removing anything
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

def search_documents_by_file(filename_pattern: str) -> List[Dict]:
    """Search for documents matching a filename pattern"""
    settings = load_local_settings()
    search_endpoint = settings.get("SEARCH_ENDPOINT")
    search_key = settings.get("SEARCH_KEY")
    index_name = "cobol-index"
    
    if not search_endpoint or not search_key:
        print("❌ Missing SEARCH_ENDPOINT or SEARCH_KEY")
        return []
    
    url = f"{search_endpoint}/indexes/{index_name}/docs/search?api-version=2023-07-01-preview"
    
    headers = {
        "Content-Type": "application/json",
        "api-key": search_key
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

def preview_removal():
    """Preview what would be removed without actually deleting"""
    
    # Files to check
    files_to_check = [
        "LONPF2_Inlined.CBL",
        "LONPFC_Inlined.CBL", 
        "LP__LONPF2_Inlined.CBL.jsonl",
        "LP__LONPFC_Inlined.CBL.jsonl"
    ]
    
    print("👀 PREVIEW: INLINED FILES REMOVAL")
    print("=" * 50)
    print("This script shows what would be deleted (no actual deletion)")
    print()
    
    total_documents = 0
    files_found = 0
    
    for filename in files_to_check:
        print(f"🔍 Checking: {filename}")
        
        # Find all documents for this file
        documents = search_documents_by_file(filename)
        
        if not documents:
            print(f"  ❌ No documents found")
            continue
        
        files_found += 1
        total_documents += len(documents)
        
        print(f"  ✅ Found {len(documents)} documents")
        
        # Show details of what would be deleted
        print(f"  📄 Sample documents that would be deleted:")
        for i, doc in enumerate(documents[:5]):
            repo_path = doc.get('repo_path', 'unknown')
            line = doc.get('line', 'unknown')
            code_preview = doc.get('code', 'no code')[:50]
            print(f"    {i+1}. {repo_path}:{line} - {code_preview}...")
        
        if len(documents) > 5:
            print(f"    ... and {len(documents) - 5} more documents")
        
        # Show line number distribution
        line_numbers = [doc.get('line', 0) for doc in documents if doc.get('line')]
        if line_numbers:
            min_line = min(line_numbers)
            max_line = max(line_numbers)
            print(f"  📊 Line range: {min_line} to {max_line}")
        
        print()
    
    print("=" * 50)
    print("📊 PREVIEW SUMMARY")
    print(f"Files with indexed content: {files_found}")
    print(f"Total documents that would be deleted: {total_documents}")
    
    if total_documents > 0:
        print(f"\n💡 IMPACT ANALYSIS:")
        print(f"   • These documents will be removed from search results")
        print(f"   • Only original (non-inlined) versions will remain") 
        print(f"   • This should reduce duplicate content in RAG responses")
        print(f"   • The inlined files will still exist in blob storage")
        
        print(f"\n🚀 To actually perform the deletion:")
        print(f"   Run: python remove_inlined_files.py")
    else:
        print(f"\n✅ No inlined file content found in the search index")
        print(f"   The index appears to already contain only original versions")

if __name__ == "__main__":
    preview_removal()
