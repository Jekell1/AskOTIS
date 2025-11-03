#!/usr/bin/env python3
"""
Debug ACUMEM search issue
"""

import sys
sys.path.append('.')
from simple_cobol_rag import SimpleCOBOLRAG

def debug_acumem_search():
    """Debug why ACUMEM search returns 0 results"""
    print("🔍 Debugging ACUMEM Search Issue")
    print("="*40)
    
    rag = SimpleCOBOLRAG()
    
    # Test 1: Direct search
    print("\n1. Testing search_cobol method for ACUMEM:")
    try:
        result = rag.search_cobol('ACUMEM', max_results=10)
        print(f"   Type: {type(result)}")
        print(f"   Keys: {list(result.keys()) if isinstance(result, dict) else 'Not a dict'}")
        
        if isinstance(result, dict) and 'results' in result:
            results = result['results']
            print(f"   Results count: {len(results)}")
            
            if len(results) > 0:
                print(f"   First result repo_path: {results[0].get('repo_path', 'missing')}")
                # Show all results briefly
                for i, r in enumerate(results[:5]):
                    path = r.get('repo_path', 'unknown')
                    print(f"      [{i+1}] {path}")
            else:
                print("   ❌ No results returned")
        else:
            print(f"   Unexpected result format: {result}")
            
    except Exception as e:
        print(f"   ❌ Search failed: {e}")
        
    # Test 2: Search for common terms
    print("\n2. Testing search for 'PROGRAM-ID':")
    try:
        result2 = rag.search_cobol('PROGRAM-ID', max_results=5)
        if isinstance(result2, dict) and 'results' in result2:
            results2 = result2['results']
            print(f"   PROGRAM-ID results: {len(results2)}")
            if len(results2) > 0:
                print(f"   First PROGRAM-ID result: {results2[0].get('repo_path', 'unknown')}")
        else:
            print(f"   PROGRAM-ID result: {result2}")
    except Exception as e:
        print(f"   ❌ PROGRAM-ID search failed: {e}")
        
    # Test 3: Check search endpoint status
    print("\n3. Testing search endpoint directly:")
    try:
        import json
        import requests
        
        LOCAL_SETTINGS = {}
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            LOCAL_SETTINGS = settings.get('Values', {})
            
        SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')
        SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')
        
        if SEARCH_ENDPOINT and SEARCH_KEY:
            url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
            headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
            body = {'search': 'ACUMEM', 'top': 10}
            
            response = requests.post(url, headers=headers, json=body)
            print(f"   Direct API status: {response.status_code}")
            
            if response.status_code == 200:
                data = response.json()
                direct_results = data.get('value', [])
                print(f"   Direct API results: {len(direct_results)}")
                
                acumem_files = [r for r in direct_results if 'acumem' in r.get('repo_path', '').lower()]
                print(f"   ACUMEM files in direct results: {len(acumem_files)}")
                
                if acumem_files:
                    for af in acumem_files:
                        print(f"      - {af.get('repo_path', 'unknown')}")
                
            else:
                print(f"   Direct API error: {response.text}")
                
        else:
            print("   ❌ Search credentials not available")
            
    except Exception as e:
        print(f"   ❌ Direct search test failed: {e}")
    
    print(f"\n{'='*40}")
    print("🔍 ACUMEM Search Debug Complete")

if __name__ == "__main__":
    debug_acumem_search()
