#!/usr/bin/env python3
"""
Debug script to examine actual search results from the COBOL RAG system
"""

import os
import sys
sys.path.append(os.path.dirname(__file__))

from simple_cobol_rag import SimpleCOBOLRAG

def debug_actual_search():
    """Debug actual search results to understand the data format"""
    
    # Create the RAG instance
    rag = SimpleCOBOLRAG()
    
    print("🔍 Debugging Actual Search Results")
    print("="*50)
    
    # Perform the actual search like the app does
    search_results = rag.search_cobol("date variables", max_results=5)
    
    print(f"\n📊 Search Results Overview:")
    print(f"   Type: {type(search_results)}")
    print(f"   Keys: {list(search_results.keys())}")
    print(f"   Total count: {search_results.get('@odata.count', 0)}")
    print(f"   Results returned: {len(search_results.get('value', []))}")
    
    if 'error' in search_results:
        print(f"   ERROR: {search_results['error']}")
        return
    
    # Examine the first few results
    results = search_results.get("value", [])
    for i, result in enumerate(results[:3]):
        print(f"\n📝 Result {i+1}:")
        print(f"   Type: {type(result)}")
        
        if isinstance(result, dict):
            for key, value in result.items():
                if key == 'code':
                    # Show first part of code
                    if isinstance(value, list):
                        print(f"   {key}: (list) {value[0][:100] if value else 'EMPTY'}...")
                    else:
                        print(f"   {key}: (str) {str(value)[:100]}...")
                else:
                    print(f"   {key}: {value}")
        else:
            print(f"   Raw: {result}")
    
    # Test extraction on first result
    if results:
        print(f"\n🧪 Testing extraction on first result:")
        first_result = results[0]
        code = first_result.get('code', '')
        symbol_name = first_result.get('symbol_name')
        program_name = first_result.get('program_name')
        
        print(f"   Original symbol_name: {symbol_name}")
        print(f"   Original program_name: {program_name}")
        
        # Test extraction
        extracted_var = rag._extract_variable_name_from_code(code)
        extracted_prog = rag._extract_program_name_from_code(code)
        
        print(f"   Extracted variable: {extracted_var}")
        print(f"   Extracted program: {extracted_prog}")

if __name__ == "__main__":
    debug_actual_search()
