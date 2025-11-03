"""
Debug the context building and LLM flow for ACUMEM
"""

import sys
sys.path.append('.')

from simple_cobol_rag import SimpleCOBOLRAG

def debug_acumem_flow():
    """Debug the full ACUMEM analysis flow"""
    
    print("🔍 DEBUGGING ACUMEM ANALYSIS FLOW")
    print("="*60)
    
    try:
        rag = SimpleCOBOLRAG()
        
        # Step 1: Test search
        print("Step 1: Testing search...")
        search_results = rag.search_cobol("ACUMEM", max_results=10)
        print(f"  Search found: {len(search_results.get('value', []))} results")
        
        # Show the first result in detail
        if search_results.get('value'):
            first_result = search_results['value'][0]
            print(f"  First result path: {first_result.get('repo_path', 'No path')}")
            code_snippet = first_result.get('code', 'No code')
            if isinstance(code_snippet, list):
                code_preview = code_snippet[:3] if len(code_snippet) > 3 else code_snippet
            else:
                code_preview = str(code_snippet)[:200]
            print(f"  Code preview: {code_preview}")
        
        # Step 2: Test _parse_user_query
        print(f"\nStep 2: Testing query parsing...")
        query = "Tell me about ACUMEM.CBL"
        search_query, query_type, optimal_results = rag._parse_user_query(query)
        print(f"  Original query: '{query}'")
        print(f"  Search query: '{search_query}'")
        print(f"  Query type: '{query_type}'")
        print(f"  Optimal results: {optimal_results}")
        
        # Step 3: Test context building
        print(f"\nStep 3: Testing context building...")
        context = rag._build_citation_context(search_results.get('value', []))
        print(f"  Context length: {len(context)} characters")
        print(f"  Context preview: {context[:300]}...")
        
        # Step 4: Test LLM call if available
        print(f"\nStep 4: Testing LLM availability...")
        llm_status = rag.get_llm_status()
        print(f"  LLM Status: {llm_status}")
        
        if llm_status.get('enabled', False):
            print(f"\nStep 5: Testing direct LLM call...")
            system_prompt = """You are analyzing COBOL code. Provide detailed analysis with citations.

<CONTEXT>"""
            
            user_prompt = "Tell me about ACUMEM.CBL - what does it do?"
            
            try:
                llm_response = rag._call_llm_with_citations(system_prompt, user_prompt, context)
                print(f"  LLM Response length: {len(llm_response)} characters")
                print(f"  LLM Response preview: {llm_response[:200]}...")
            except Exception as e:
                print(f"  LLM Error: {e}")
        
        # Step 6: Test full generate_response 
        print(f"\nStep 6: Testing full generate_response...")
        response = rag.generate_response("What does ACUMEM.CBL do?", max_results=5)
        print(f"  Response length: {len(response)} characters")
        print(f"  Response: {response}")
        
    except Exception as e:
        print(f"❌ Exception: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    debug_acumem_flow()
