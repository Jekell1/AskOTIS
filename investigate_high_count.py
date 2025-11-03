#!/usr/bin/env python3
"""
Investigate the extremely high document count in the index
"""

import json
import requests

def load_local_settings():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        return settings.get('Values', {})

def investigate_index():
    settings = load_local_settings()
    endpoint = settings.get('SEARCH_ENDPOINT')
    key = settings.get('SEARCH_KEY')
    
    if not endpoint or not key:
        print("Missing configuration")
        return
    
    # Check index info
    url = f'{endpoint}/indexes/cobol-index?api-version=2023-07-01-preview'
    headers = {'Content-Type': 'application/json', 'api-key': key}
    
    print("=== INVESTIGATING HIGH DOCUMENT COUNT ===")
    
    try:
        response = requests.get(url, headers=headers)
        if response.status_code == 200:
            index_info = response.json()
            doc_count = index_info.get('documentCount', 'unknown')
            print(f"Current document count: {doc_count}")
            
            if str(doc_count).replace(',', '').isdigit() and int(str(doc_count).replace(',', '')) > 1000000:
                print("\n⚠️  CRITICAL: Document count is over 1 million!")
                print("This is likely caused by one of these issues:")
                print("1. Indexer ran multiple times without clearing")
                print("2. Duplicate indexing from multiple data sources")
                print("3. Indexer is processing the same files repeatedly")
                
        else:
            print(f"Error: {response.status_code}")
            
    except Exception as e:
        print(f"Error checking index: {e}")
    
    # Sample some documents to understand the pattern
    print("\n=== SAMPLING DOCUMENTS ===")
    search_url = f'{endpoint}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
    
    try:
        # Get a sample of documents
        search_body = {
            "search": "*",
            "top": 20,
            "select": "id,repo_path,line"
        }
        
        response = requests.post(search_url, headers=headers, json=search_body)
        if response.status_code == 200:
            results = response.json()
            documents = results.get('value', [])
            
            print(f"Sample of {len(documents)} documents:")
            
            # Analyze patterns
            file_counts = {}
            line_counts = {}
            
            for doc in documents:
                repo_path = doc.get('repo_path', 'unknown')
                line = doc.get('line', 'unknown')
                
                file_counts[repo_path] = file_counts.get(repo_path, 0) + 1
                line_counts[line] = line_counts.get(line, 0) + 1
            
            print("\nFile frequency in sample:")
            for file, count in sorted(file_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
                print(f"  {file}: {count} occurrences")
            
            print("\nLine number frequency in sample:")
            for line, count in sorted(line_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
                print(f"  Line {line}: {count} occurrences")
                
        else:
            print(f"Error sampling documents: {response.status_code}")
            
    except Exception as e:
        print(f"Error sampling: {e}")
    
    # Check indexers
    print("\n=== CHECKING INDEXERS ===")
    try:
        indexers_url = f'{endpoint}/indexers?api-version=2023-07-01-preview'
        response = requests.get(indexers_url, headers=headers)
        
        if response.status_code == 200:
            indexers = response.json().get('value', [])
            
            for indexer in indexers:
                name = indexer.get('name', 'unknown')
                if 'cobol' in name.lower():
                    print(f"\nIndexer: {name}")
                    
                    # Get indexer status
                    status_url = f'{endpoint}/indexers/{name}/status?api-version=2023-07-01-preview'
                    status_response = requests.get(status_url, headers=headers)
                    
                    if status_response.status_code == 200:
                        status = status_response.json()
                        last_result = status.get('lastResult', {})
                        items_processed = last_result.get('itemsProcessed', 'unknown')
                        print(f"  Last run processed: {items_processed} items")
                        print(f"  Status: {last_result.get('status', 'unknown')}")
                        
        else:
            print(f"Error checking indexers: {response.status_code}")
            
    except Exception as e:
        print(f"Error checking indexers: {e}")

if __name__ == "__main__":
    investigate_index()
