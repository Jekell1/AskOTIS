"""
Debug the exact context being sent to the LLM
"""

import sys
sys.path.append('.')

from simple_rag import SimpleCodeRAG

def debug_llm_context():
    """Debug what context is actually being sent to LLM"""
    
    print("🔍 DEBUGGING LLM CONTEXT")
    print("="*60)
    
    rag = SimpleCodeRAG()
    
    # Search for ACUMEM
    search_results = rag.search_code("ACUMEM", max_results=5)
    results = search_results.get('value', [])
    
    print(f"Search found {len(results)} results")
    
    if results:
        # Create the context that would be sent to LLM
        context = rag._create_compact_context(results)
        
        # Show the exact system prompt that would be sent
        system_prompt = f"""You are an expert code analyst. Answer questions using the retrieved data below.
Always cite sources using [S1], [S2], etc. Be helpful and provide detailed analysis when the data supports it.
Only say "Not enough information" if there truly isn't relevant data to answer the question.

{context}"""
        
        print("EXACT SYSTEM PROMPT BEING SENT:")
        print("="*60)
        print(system_prompt)
        print("="*60)
        
        # Check if ACUMEM is actually in the context
        if "ACUMEM" in context:
            print("✅ ACUMEM is in the context")
        else:
            print("❌ ACUMEM is NOT in the context")
            
        # Check for specific content
        if "MEMORY USAGE LOGGING" in context:
            print("✅ Description found in context")
        else:
            print("❌ Description NOT found in context")
            
        # Let's also check the raw first result
        print("\nRAW FIRST RESULT:")
        first_result = results[0]
        print(f"repo_path: {first_result.get('repo_path')}")
        print(f"code type: {type(first_result.get('code'))}")
        code_content = str(first_result.get('code'))[:500]
        print(f"code content (first 500 chars): {code_content}")

if __name__ == "__main__":
    debug_llm_context()
