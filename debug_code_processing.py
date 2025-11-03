"""
Debug the code processing issue specifically
"""

import sys
sys.path.append('.')

from simple_cobol_rag import SimpleCOBOLRAG

def debug_code_processing():
    """Debug how the code is being processed"""
    
    print("🔍 DEBUGGING CODE PROCESSING")
    print("="*60)
    
    rag = SimpleCOBOLRAG()
    
    # Get search results
    search_results = rag.search_cobol("ACUMEM", max_results=1)  # Just 1 result
    
    if not search_results.get('value'):
        print("No search results!")
        return
    
    first_result = search_results['value'][0]
    
    print("Raw first result keys:", list(first_result.keys()))
    print("Code type:", type(first_result.get('code')))
    print("Code preview:", str(first_result.get('code'))[:200])
    
    # Test code processing
    code_raw = first_result.get('code', '')
    print(f"\nCode raw type: {type(code_raw)}")
    print(f"Code raw length: {len(code_raw) if isinstance(code_raw, (list, str)) else 'N/A'}")
    
    if isinstance(code_raw, list):
        print(f"List length: {len(code_raw)}")
        print(f"First 3 items: {code_raw[:3]}")
        
        # Process like the function should
        code_joined = '\n'.join(code_raw[:50])
        print(f"After joining first 50: {len(code_joined)} chars")
        print(f"Preview: {code_joined[:200]}...")
        
        # Test trim function
        trimmed = rag._trim_to_salient_lines(code_joined, max_lines=10)
        print(f"After trimming: {len(trimmed)} chars")
        print(f"Trimmed preview: {trimmed[:200]}...")
    
    # Test building context with just this one result
    context = rag._build_citation_context([first_result])
    print(f"\nContext length: {len(context)} characters")
    print(f"Context preview: {context[:500]}...")

if __name__ == "__main__":
    debug_code_processing()
