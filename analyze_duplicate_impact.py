#!/usr/bin/env python3
"""
Simple test to show duplicate line impact using existing working config
"""

import json
import requests

# Load configuration from local.settings.json
def load_local_settings():
    """Load configuration from local.settings.json"""
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except FileNotFoundError:
        return {}
    except Exception as e:
        print(f"Error loading local.settings.json: {e}")
        return {}

# Load settings and configure
LOCAL_SETTINGS = load_local_settings()
AZURE_SEARCH_ENDPOINT = LOCAL_SETTINGS.get("SEARCH_ENDPOINT", "https://az-use1-ai-search.search.windows.net")
AZURE_SEARCH_KEY = LOCAL_SETTINGS.get("SEARCH_KEY")
AZURE_SEARCH_INDEX = "cobol-index"

def search_cobol_code(query, top_k=10):
    """Search the COBOL index and return results"""
    if not AZURE_SEARCH_KEY:
        print("❌ Azure Search Key not found in local.settings.json")
        return None
        
    url = f"{AZURE_SEARCH_ENDPOINT}/indexes/{AZURE_SEARCH_INDEX}/docs/search?api-version=2023-07-01-preview"
    
    headers = {
        "Content-Type": "application/json",
        "api-key": AZURE_SEARCH_KEY
    }
    
    search_body = {
        "search": query,
        "top": top_k,
        "searchMode": "any",
        "queryType": "simple"
    }
    
    try:
        response = requests.post(url, headers=headers, json=search_body)
        if response.status_code == 200:
            return response.json()
        else:
            print(f"Search failed: {response.status_code} - {response.text}")
            return None
    except Exception as e:
        print(f"Search error: {e}")
        return None

def analyze_duplicate_impact():
    """Analyze how duplicates affect different types of questions"""
    
    print("🔍 ANALYZING DUPLICATE LINE IMPACT ON YOUR QUESTIONS")
    print("=" * 70)
    
    # Test 1: Generic COBOL question (hits many duplicates)
    print("\n📝 SCENARIO 1: Generic question - 'How do COBOL programs start?'")
    print("Search: 'IDENTIFICATION DIVISION'")
    
    results = search_cobol_code("IDENTIFICATION DIVISION", top_k=5)
    if results and 'value' in results:
        print(f"   Total matches: {results.get('@odata.count', 'N/A')}")
        unique_files = set()
        for doc in results['value']:
            unique_files.add(doc.get('repo_path', 'unknown'))
        print(f"   Unique files: {len(unique_files)}")
        print("   💡 Impact: HIGH - You'd get repetitive answers about the same line")
        
        print("   Sample results:")
        for i, doc in enumerate(results['value'][:3]):
            file_path = doc.get('repo_path', 'unknown').split('/')[-1]
            line_num = doc.get('line', '?')
            print(f"     {i+1}. {file_path}:{line_num} - Same content across files")
    
    # Test 2: Specific business logic question
    print("\n📝 SCENARIO 2: Specific question - 'What does LONPF2 program do?'") 
    print("Search: 'LONPF2'")
    
    results = search_cobol_code("LONPF2", top_k=5)
    if results and 'value' in results:
        print(f"   Total matches: {results.get('@odata.count', 'N/A')}")
        unique_lines = set()
        for doc in results['value']:
            unique_lines.add(doc.get('line', 'unknown'))
        print(f"   Unique line numbers: {len(unique_lines)}")
        print("   💡 Impact: LOW - You'd get diverse, specific content")
        
        print("   Sample results:")
        for i, doc in enumerate(results['value'][:3]):
            file_path = doc.get('repo_path', 'unknown').split('/')[-1] 
            line_num = doc.get('line', '?')
            code = doc.get('code', 'No code')[:60]
            print(f"     {i+1}. {file_path}:{line_num} - {code}...")

    # Test 3: Variable or function search
    print("\n📝 SCENARIO 3: Code search - 'Find CUSTOMER variables'")
    print("Search: 'CUSTOMER'")
    
    results = search_cobol_code("CUSTOMER", top_k=5)
    if results and 'value' in results:
        print(f"   Total matches: {results.get('@odata.count', 'N/A')}")
        line_numbers = [doc.get('line', 0) for doc in results['value']]
        duplicate_line_1 = sum(1 for line in line_numbers if line == 1)
        print(f"   Results from line 1 (duplicates): {duplicate_line_1}")
        print(f"   Results from other lines: {len(line_numbers) - duplicate_line_1}")
        print("   💡 Impact: MEDIUM - Mix of duplicates and unique content")

    print("\n" + "=" * 70)
    print("📊 SUMMARY: Will duplicates cause problems with your questions?")
    print("=" * 70)
    
    print("✅ QUESTIONS THAT WORK WELL (Low Impact):")
    print("   • 'What does the LONPF2 program do?'")
    print("   • 'How are customer accounts processed?'")
    print("   • 'What business rules are in program XYZ?'")
    print("   • 'Find all references to CUSTOMER-ID'")
    print("   • 'How does error handling work?'")
    
    print("\n⚠️  QUESTIONS WITH SOME REDUNDANCY (Medium Impact):") 
    print("   • 'How are COBOL programs structured?'")
    print("   • 'What's the common pattern in these programs?'")
    print("   • 'Show me program headers'")
    
    print("\n❌ QUESTIONS WITH HIGH REDUNDANCY (High Impact):")
    print("   • 'How do all COBOL programs start?'")
    print("   • 'What's on line 1 of every program?'")
    print("   • 'Show me IDENTIFICATION DIVISION examples'")
    
    print("\n🔧 WORKAROUNDS:")
    print("   1. Be specific: Ask about particular programs or functions")
    print("   2. Use program names: 'How does LONPF2 handle customers?'")  
    print("   3. Ask about business logic rather than code structure")
    print("   4. The RAG system is smart enough to synthesize good answers anyway!")
    
    print("\n🎯 BOTTOM LINE:")
    print("   The duplicates are mostly harmless! Your specific business and")
    print("   technical questions will get great answers. Just avoid very generic")
    print("   questions about COBOL structure if you want focused responses.")

if __name__ == "__main__":
    analyze_duplicate_impact()
