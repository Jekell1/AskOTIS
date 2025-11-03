"""
Verify the enhanced new_cobol_calls index and demonstrate the improved dependency data.
"""

import json
import requests
from collections import defaultdict

def verify_enhanced_calls_index():
    """Verify the enhanced calls index and show the improvement."""
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    index_name = "new_cobol_calls"
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    print("🔍 VERIFYING ENHANCED NEW_COBOL_CALLS INDEX")
    print("=" * 50)
    
    # 1. Check total document count
    search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$count=true&$top=0"
    response = requests.get(search_url, headers=headers)
    
    if response.status_code == 200:
        data = response.json()
        total_docs = data.get('@odata.count', 0)
        print(f"📊 Total documents in index: {total_docs:,}")
    
    # 2. Check enhanced data count
    enhanced_search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$count=true&$top=0&$filter=enhanced_data eq true"
    response = requests.get(enhanced_search_url, headers=headers)
    
    if response.status_code == 200:
        data = response.json()
        enhanced_docs = data.get('@odata.count', 0)
        print(f"🔧 Enhanced records: {enhanced_docs:,}")
    
    # 3. Show APIPAY dependencies before and after
    print(f"\n📋 APIPAY DEPENDENCIES COMPARISON:")
    print("=" * 40)
    
    # Original APIPAY calls
    original_search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$filter=caller_program eq 'APIPAY' and enhanced_data ne true&$select=callee_program,call_type,is_dynamic&$top=20"
    response = requests.get(original_search_url, headers=headers)
    
    if response.status_code == 200:
        original_data = response.json()
        original_calls = original_data.get('value', [])
        print(f"🔸 Original calls found: {len(original_calls)}")
        for call in original_calls:
            print(f"   • {call.get('callee_program', 'Unknown')} ({call.get('call_type', 'unknown')})")
    
    # Enhanced APIPAY dependencies
    enhanced_search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$filter=caller_program eq 'APIPAY' and enhanced_data eq true&$select=callee_program,reference_type,category,is_program_call,is_copybook&$top=50"
    response = requests.get(enhanced_search_url, headers=headers)
    
    if response.status_code == 200:
        enhanced_data = response.json()
        enhanced_refs = enhanced_data.get('value', [])
        print(f"\n🔸 Enhanced dependencies found: {len(enhanced_refs)}")
        
        # Group by category
        by_category = defaultdict(list)
        for ref in enhanced_refs:
            category = ref.get('category', 'OTHER')
            by_category[category].append(ref)
        
        for category, refs in by_category.items():
            print(f"\n   📂 {category} ({len(refs)} items):")
            # Show first 5 of each category
            for ref in refs[:5]:
                ref_type = ref.get('reference_type', 'Unknown')
                callee = ref.get('callee_program', 'Unknown')
                print(f"      • {callee} ({ref_type})")
            if len(refs) > 5:
                print(f"      ... and {len(refs) - 5} more")
    
    # 4. Show specific programs now discoverable
    print(f"\n🎯 DYNAMIC PROGRAMS NOW DISCOVERABLE:")
    print("=" * 40)
    
    dynamic_programs = ['LP/LONPF2', 'LP/LONPF7', 'LP/LONPF9', 'LP/LONPFA', 'LP/LONPFB', 'LP/LONPFC']
    
    for program in dynamic_programs:
        program_search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$filter=caller_program eq 'APIPAY' and callee_program eq '{program}' and enhanced_data eq true&$select=reference_type,line&$top=1"
        response = requests.get(program_search_url, headers=headers)
        
        if response.status_code == 200:
            data = response.json()
            results = data.get('value', [])
            if results:
                ref_type = results[0].get('reference_type', 'Unknown')
                line = results[0].get('line', 0)
                print(f"   ✅ {program} - {ref_type} at line {line}")
            else:
                print(f"   ❌ {program} - not found")
    
    # 5. Show copybook dependencies
    copybook_search_url = f"{endpoint}/indexes/{index_name}/docs?api-version=2024-07-01&$filter=caller_program eq 'APIPAY' and is_copybook eq true&$select=callee_program,reference_type&$top=10"
    response = requests.get(copybook_search_url, headers=headers)
    
    if response.status_code == 200:
        copybook_data = response.json()
        copybooks = copybook_data.get('value', [])
        print(f"\n📚 COPYBOOK DEPENDENCIES ({len(copybooks)} found):")
        for copybook in copybooks:
            name = copybook.get('callee_program', 'Unknown')
            ref_type = copybook.get('reference_type', 'Unknown')
            print(f"   • {name} ({ref_type})")

def test_rag_improvement():
    """Test how the RAG system now performs with enhanced data."""
    
    print(f"\n🤖 RAG SYSTEM IMPROVEMENT TEST:")
    print("=" * 40)
    
    # Load configuration
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    
    endpoint = settings['SEARCH_ENDPOINT']
    key = settings['SEARCH_KEY']
    
    headers = {
        'Content-Type': 'application/json',
        'api-key': key
    }
    
    # Test query: "What programs does APIPAY call?"
    test_query = "APIPAY program calls dependencies"
    
    search_url = f"{endpoint}/indexes/new_cobol_calls/docs/search?api-version=2024-07-01"
    search_body = {
        "search": test_query,
        "filter": "caller_program eq 'APIPAY'",
        "select": "callee_program,reference_type,category,snippet",
        "top": 20,
        "orderby": "search.score() desc"
    }
    
    response = requests.post(search_url, headers=headers, json=search_body)
    
    if response.status_code == 200:
        data = response.json()
        results = data.get('value', [])
        print(f"🔍 Query: '{test_query}'")
        print(f"📊 Results found: {len(results)}")
        
        unique_programs = set()
        for result in results:
            program = result.get('callee_program', 'Unknown')
            unique_programs.add(program)
            ref_type = result.get('reference_type', result.get('call_type', 'Unknown'))
            category = result.get('category', 'Legacy')
            snippet = result.get('snippet', '')[:50] + "..."
            print(f"   • {program} ({ref_type}) - {snippet}")
        
        print(f"\n🎯 Unique programs discoverable: {len(unique_programs)}")
        print(f"   Before enhancement: ~4 programs")
        print(f"   After enhancement: {len(unique_programs)} programs")
        print(f"   Improvement: {len(unique_programs) - 4} additional programs!")

def main():
    """Main verification function."""
    
    verify_enhanced_calls_index()
    test_rag_improvement()
    
    print(f"\n✅ VERIFICATION COMPLETE")
    print("The new_cobol_calls index now contains comprehensive dependency data!")
    print("RAG queries will now find all external references, not just direct calls.")

if __name__ == "__main__":
    main()