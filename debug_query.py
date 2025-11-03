#!/usr/bin/env python3
"""
Debug test to see what error is being triggered
"""
from simple_cobol_rag import SimpleCOBOLRAG
import json

def debug_query():
    """Debug query to see what's happening"""
    
    print("🔍 Debug Test - Checking API Call Details...")
    
    # Initialize RAG system
    rag = SimpleCOBOLRAG()
    
    # Test with simple query and very small result set
    query = "What is LONPF2?"
    
    # Get search results directly first
    search_results = rag.search_cobol("LONPF2", max_results=1)  # Just 1 result
    results = search_results.get('value', [])
    
    print(f"📊 Search returned {len(results)} results")
    
    if results:
        result = results[0]
        print(f"📄 Result content length: {len(result.get('content', ''))}")
        print(f"📄 Result preview: {result.get('content', '')[:100]}...")
    
    # Try the full query now
    try:
        response = rag.generate_response(query, max_results=1)
        print(f"\n✅ Response: {response[:200]}...")
    except Exception as e:
        print(f"\n❌ Exception: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    debug_query()
