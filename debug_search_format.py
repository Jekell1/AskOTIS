#!/usr/bin/env python3
"""
Debug script to examine the exact format of search results from Azure Search
"""

import os
import sys
sys.path.append(os.path.dirname(__file__))

from simple_cobol_rag import SimpleCOBOLRAG

def debug_search_results():
    """Debug the exact format of search results"""
    
    # Create the RAG instance
    rag = SimpleCOBOLRAG()
    
    print("🔍 Debugging Search Results Format")
    print("="*50)
    
    # Perform the search
    search_results = rag.search_client.search(
        search_text="date variables",
        top=5,  # Just get a few results
        include_total_count=True
    )
    
    for i, result in enumerate(search_results):
        print(f"\n📝 Result {i+1}:")
        print(f"   Type: {type(result)}")
        
        # Show all available fields
        if hasattr(result, '__dict__'):
            for key, value in result.__dict__.items():
                print(f"   {key}: {type(value)} = {repr(value)[:200]}...")
        else:
            # If it's a dict-like object
            for key in result.keys():
                value = result[key]
                print(f"   {key}: {type(value)} = {repr(value)[:200]}...")
        
        print(f"   --- Raw result ---")
        print(f"   {repr(result)[:500]}...")

if __name__ == "__main__":
    debug_search_results()
