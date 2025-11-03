#!/usr/bin/env python3
"""
Examine search results for deep COBOL content to verify chunking
"""

import json
import requests

def examine_deep_content():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        LOCAL_SETTINGS = settings.get('Values', {})

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')

    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    search_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
    
    print("=== EXAMINING DEEP COBOL CONTENT ===")
    
    # Search specifically for content that should be deeper in files
    test_queries = [
        "PROCEDURE DIVISION",
        "WORKING-STORAGE SECTION", 
        "PERFORM UNTIL",
        "MOVE SPACES TO"
    ]
    
    for query in test_queries:
        print(f"\n🔍 Searching for: '{query}'")
        
        search_body = {
            'search': query,
            'top': 3,
            'select': 'repo_path,code,line',
            'highlight': 'code'
        }
        
        try:
            response = requests.post(search_url, headers=headers, json=search_body)
            
            if response.status_code == 200:
                results = response.json()
                docs = results.get('value', [])
                
                print(f"   Found {len(docs)} matches:")
                
                for i, doc in enumerate(docs):
                    file_path = doc.get('repo_path', 'unknown')
                    code = doc.get('code', '')
                    line_num = doc.get('line', 'unknown')
                    
                    print(f"\n   📄 Match {i+1}:")
                    print(f"      File: {file_path.split('/')[-1]}")
                    print(f"      Line: {line_num}")
                    print(f"      Length: {len(code)} chars")
                    print(f"      Content: {code}")
                    
                    # Check if this indicates chunking is working
                    if line_num != 1 and line_num != "unknown":
                        print(f"      ✅ Found content from line {line_num} - chunking is working!")
                    elif len(code) > 100:
                        print(f"      ✅ Large content block - likely chunked!")
                    else:
                        print(f"      ⚠️  May still be single-line content")
                        
            else:
                print(f"   ❌ Search failed: {response.status_code}")
                
        except Exception as e:
            print(f"   ❌ Error: {e}")
    
    # Now let's check index statistics to see the improvement
    print(f"\n📊 CHECKING OVERALL INDEX IMPROVEMENT:")
    
    try:
        stats_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/stats?api-version=2023-07-01-preview'
        stats_response = requests.get(stats_url, headers=headers)
        
        if stats_response.status_code == 200:
            stats = stats_response.json()
            doc_count = stats.get('documentCount', 0)
            storage_size = stats.get('storageSize', 0)
            
            print(f"   Total Documents: {doc_count:,}")
            print(f"   Storage Size: {storage_size:,} bytes ({storage_size/1024/1024:.1f} MB)")
            
            # Calculate improvement from our previous baseline
            previous_docs = 9951  # From before the fix
            previous_storage = 3149321  # From before the fix
            
            doc_improvement = ((doc_count - previous_docs) / previous_docs * 100)
            storage_improvement = ((storage_size - previous_storage) / previous_storage * 100)
            
            print(f"\n📈 IMPROVEMENT ANALYSIS:")
            print(f"   Document Count: +{doc_improvement:.1f}% ({doc_count - previous_docs:,} more docs)")
            print(f"   Storage Size: +{storage_improvement:.1f}% ({(storage_size - previous_storage)/1024/1024:.1f} MB more content)")
            
            if doc_improvement > 10 or storage_improvement > 1000:
                print(f"   ✅ SIGNIFICANT IMPROVEMENT - Indexer fix was successful!")
            else:
                print(f"   ⚠️  Limited improvement - may need further optimization")
                
    except Exception as e:
        print(f"❌ Error getting stats: {e}")

if __name__ == "__main__":
    examine_deep_content()
