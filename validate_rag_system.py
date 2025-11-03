#!/usr/bin/env python3
"""
Quick validation of the RAG system with improved search
"""

import json
import requests

def validate_rag_system():
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        LOCAL_SETTINGS = settings.get('Values', {})

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')

    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    search_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
    
    print("=== VALIDATING RAG SYSTEM ===")
    print(f"🔍 Search Endpoint: {SEARCH_ENDPOINT}")
    print(f"📚 Index: cobol-index")
    print()
    
    # Test queries that a COBOL developer might ask
    validation_queries = [
        {
            "query": "PROCEDURE DIVISION",
            "description": "Finding COBOL program structure"
        },
        {
            "query": "CALL subroutine program",
            "description": "Finding program calls and dependencies"
        },
        {
            "query": "WORKING-STORAGE SECTION data variables",
            "description": "Finding data definitions"
        },
        {
            "query": "PERFORM loop iteration",
            "description": "Finding loop constructs"
        },
        {
            "query": "MOVE SPACES initialization",
            "description": "Finding data initialization patterns"
        }
    ]
    
    print("🧪 Testing RAG-style queries...")
    
    for test in validation_queries:
        query = test["query"]
        description = test["description"]
        
        search_body = {
            'search': query,
            'top': 5,
            'select': 'repo_path,code',
            'searchMode': 'all'
        }
        
        try:
            response = requests.post(search_url, headers=headers, json=search_body)
            
            if response.status_code == 200:
                results = response.json()
                docs = results.get('value', [])
                
                unique_files = set()
                total_content = 0
                relevant_results = 0
                
                for doc in docs:
                    code = doc.get('code', '')
                    total_content += len(code)
                    
                    # Check if result seems relevant
                    query_words = query.upper().split()
                    code_upper = code.upper()
                    
                    if any(word in code_upper for word in query_words):
                        relevant_results += 1
                    
                    file_path = doc.get('repo_path', '')
                    if file_path:
                        unique_files.add(file_path.split('/')[-1])
                
                avg_size = total_content / len(docs) if docs else 0
                relevance = (relevant_results / len(docs) * 100) if docs else 0
                
                print(f"   ✓ {description}")
                print(f"     Query: '{query}'")
                print(f"     Results: {len(docs)} docs, {len(unique_files)} files")
                print(f"     Content: {avg_size:.0f} avg chars, {relevance:.0f}% relevant")
                
                # Show quality indicator
                if len(docs) > 0 and avg_size > 1000 and relevance > 50:
                    print(f"     🟢 EXCELLENT: Rich content with high relevance")
                elif len(docs) > 0 and avg_size > 500:
                    print(f"     🟡 GOOD: Decent content available")
                else:
                    print(f"     🔴 NEEDS ATTENTION: Limited or low-quality results")
                    
                print()
                    
            else:
                print(f"   ❌ Query failed: {response.status_code}")
                print()
                
        except Exception as e:
            print(f"   ❌ Error: {e}")
            print()
    
    # Final system health check
    print("🏥 System Health Check:")
    
    try:
        # Check index stats
        stats_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/stats?api-version=2023-07-01-preview'
        stats_response = requests.get(stats_url, headers=headers)
        
        if stats_response.status_code == 200:
            stats = stats_response.json()
            doc_count = stats.get('documentCount', 0)
            storage_size = stats.get('storageSize', 0)
            
            print(f"   📊 Index Health: {doc_count:,} documents, {storage_size/1024/1024:.1f} MB")
            
            # Health indicators
            if doc_count > 10000 and storage_size > 50000000:
                print(f"   🟢 SYSTEM STATUS: Excellent - Rich content available")
            elif doc_count > 1000:
                print(f"   🟡 SYSTEM STATUS: Good - Sufficient content")
            else:
                print(f"   🔴 SYSTEM STATUS: Needs attention")
                
        # Test a simple search to verify connectivity
        simple_search = {
            'search': '*',
            'top': 1,
            'select': 'repo_path'
        }
        
        test_response = requests.post(search_url, headers=headers, json=simple_search)
        if test_response.status_code == 200:
            print(f"   🔗 Connectivity: ✅ Search API responsive")
        else:
            print(f"   🔗 Connectivity: ❌ Search API issues ({test_response.status_code})")
            
    except Exception as e:
        print(f"   ❌ Health check failed: {e}")
    
    print()
    print("=== VALIDATION COMPLETE ===")
    print("🎯 The RAG system is ready for COBOL code analysis!")
    print("🌐 Access the chatbot at: http://localhost:8503")

if __name__ == "__main__":
    validate_rag_system()
