"""
Examine the exact search results for ACUMEM to see what data we have
"""

import sys
sys.path.append('.')

from simple_cobol_rag import SimpleCOBOLRAG
import json

def examine_acumem_results():
    """Examine what data we actually have for ACUMEM"""
    
    print("🔍 EXAMINING ACUMEM SEARCH RESULTS")
    print("="*60)
    
    rag = SimpleCOBOLRAG()
    
    # Get search results  
    search_results = rag.search_cobol("ACUMEM", max_results=5)
    
    if not search_results.get('value'):
        print("No search results!")
        return
    
    print(f"Found {len(search_results['value'])} results")
    
    for i, result in enumerate(search_results['value'], 1):
        print(f"\n--- RESULT {i} ---")
        print(f"Repo path: {result.get('repo_path', 'N/A')}")
        print(f"Line: {result.get('line', 'N/A')}")
        print(f"Symbol name: {result.get('symbol_name', 'N/A')}")
        print(f"Symbol kind: {result.get('symbol_kind', 'N/A')}")
        print(f"Search score: {result.get('@search.score', 'N/A')}")
        
        # Check code content
        code_raw = result.get('code', '')
        print(f"Code type: {type(code_raw)}")
        print(f"Code length: {len(code_raw) if isinstance(code_raw, (str, list)) else 'N/A'}")
        
        # If it's a JSON string, parse it
        if isinstance(code_raw, str) and code_raw.strip().startswith('['):
            try:
                code_lines = json.loads(code_raw)
                print(f"Parsed to {len(code_lines)} lines")
                print("First 10 lines:")
                for j, line in enumerate(code_lines[:10], 1):
                    print(f"  {j:2d}: {line}")
            except json.JSONDecodeError:
                print("Failed to parse as JSON")
                print(f"Raw preview: {code_raw[:300]}...")
        else:
            print(f"Raw preview: {str(code_raw)[:300]}...")
        
        # Focus on the ACUMEM.CBL file specifically
        if 'ACUMEM.CBL' in result.get('repo_path', ''):
            print("\n*** THIS IS THE ACUMEM.CBL FILE ***")
            if isinstance(code_raw, str) and code_raw.strip().startswith('['):
                try:
                    code_lines = json.loads(code_raw)
                    print("Full ACUMEM.CBL content (first 30 lines):")
                    for j, line in enumerate(code_lines[:30], 1):
                        print(f"  {j:3d}: {line}")
                except json.JSONDecodeError:
                    print("Failed to parse ACUMEM content")

if __name__ == "__main__":
    examine_acumem_results()
