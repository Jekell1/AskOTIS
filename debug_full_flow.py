"""
Debug the full generate_response flow step by step
"""

import sys
sys.path.append('.')

from simple_cobol_rag import SimpleCOBOLRAG

def debug_generate_response():
    """Debug the full generate_response flow"""
    
    print("🔍 DEBUGGING FULL generate_response FLOW")
    print("="*60)
    
    rag = SimpleCOBOLRAG()
    query = "Tell me about ACUMEM.CBL"
    
    # Step 1: Parse query
    print(f"Step 1: Parsing query: '{query}'")
    search_query, query_type, optimal_results = rag._parse_user_query(query)
    print(f"  Search query: '{search_query}'")
    print(f"  Query type: '{query_type}'") 
    print(f"  Optimal results: {optimal_results}")
    
    # Step 2: Search
    print(f"\nStep 2: Searching with max_results={optimal_results}")
    search_results = rag.search_cobol(search_query, max_results=optimal_results)
    print(f"  Found {len(search_results.get('value', []))} results")
    
    # Step 3: Context building 
    print(f"\nStep 3: Building context")
    context = rag._build_citation_context(search_results.get('value', []))
    print(f"  Context length: {len(context)} chars")
    print(f"  Context preview: {context[:500]}...")
    
    # Step 4: System prompt selection
    print(f"\nStep 4: System prompt selection")
    system_prompt = rag._get_grounded_system_prompt()
    print(f"  Query type: {query_type}")
    print(f"  System prompt length: {len(system_prompt)} chars")
    print(f"  System prompt preview: {system_prompt[:200]}...")
    
    # Step 5: LLM call
    print(f"\nStep 5: Calling LLM")
    response = rag._call_llm_with_citations(system_prompt, query, context)
    print(f"  Response length: {len(response)} chars")
    print(f"  Response: {response}")
    
    # Step 6: Check fallback condition
    print(f"\nStep 6: Checking fallback conditions")
    fallback_phrases = [
        "not enough information",
        "insufficient data", 
        "cannot determine",
        "unable to analyze",
        "more context needed"
    ]
    
    response_lower = response.lower()
    triggered_fallback = any(phrase in response_lower for phrase in fallback_phrases)
    print(f"  Triggered fallback: {triggered_fallback}")
    
    if triggered_fallback:
        print(f"  Fallback phrases found: {[phrase for phrase in fallback_phrases if phrase in response_lower]}")

if __name__ == "__main__":
    debug_generate_response()
