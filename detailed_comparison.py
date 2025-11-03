"""
Detailed comparison between chatbot search and direct API search
to identify the exact difference causing ACUMEM search failure.
"""

import os
import sys
import requests
import json
from typing import Dict, Any

# Load environment variables from local.settings.json if it exists
import os

try:
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
        for key, value in settings.items():
            os.environ[key] = value
except FileNotFoundError:
    pass

# Configuration from environment
AZURE_SEARCH_ENDPOINT = os.getenv("SEARCH_ENDPOINT")
AZURE_SEARCH_KEY = os.getenv("SEARCH_KEY")
AZURE_SEARCH_INDEX = os.getenv("SEARCH_INDEX", "cobol-index")

def create_chatbot_search_request():
    """Create the exact search request that the chatbot would make"""
    
    # Replicate chatbot's search_cobol method exactly
    query = "ACUMEM"
    filter_type = None
    max_results = None
    
    # From chatbot code: DEFAULT_SEARCH_RESULTS = 50
    DEFAULT_SEARCH_RESULTS = 50
    
    # Optimized page size - never request more than 1000 at once
    page_size = min(max_results or DEFAULT_SEARCH_RESULTS, 1000)
    
    search_body = {
        "search": query,
        "select": "repo_path,line,code,symbol_name,symbol_kind",  # Drop 'calls' unless needed
        "top": page_size,
        "count": False  # Skip count for performance - only calculate when needed
    }
    
    # Apply filters with cleaner mapping
    if filter_type:
        filter_map = {
            "programs": "symbol_kind eq 'program'",
            "variables": "symbol_kind eq 'variable'", 
            "procedures": "symbol_kind eq 'procedure'"
        }
        search_body["filter"] = filter_map.get(filter_type)
        
    return search_body

def create_direct_search_request():
    """Create the exact search request from our working debug script"""
    return {
        "search": "ACUMEM",
        "select": "repo_path,line,code,symbol_name,symbol_kind",
        "top": 50,
        "count": True
    }

def execute_search(search_body: Dict[str, Any], description: str):
    """Execute a search request and return results"""
    
    api_url = f"{AZURE_SEARCH_ENDPOINT}/indexes/{AZURE_SEARCH_INDEX}/docs/search"
    headers = {
        "Content-Type": "application/json",
        "api-key": AZURE_SEARCH_KEY
    }
    params = {"api-version": "2024-07-01"}
    
    print(f"\n{'='*60}")
    print(f"🔍 Testing {description}")
    print(f"{'='*60}")
    
    print(f"URL: {api_url}")
    print(f"Headers: {json.dumps(headers, indent=2)}")
    print(f"Params: {json.dumps(params, indent=2)}")
    print(f"Body: {json.dumps(search_body, indent=2)}")
    
    try:
        response = requests.post(
            api_url,
            headers=headers,
            params=params,
            json=search_body,
            timeout=30
        )
        
        print(f"\nResponse Status: {response.status_code}")
        print(f"Response Headers: {dict(response.headers)}")
        
        if response.status_code == 200:
            data = response.json()
            result_count = len(data.get("value", []))
            total_count = data.get("@odata.count", "Not provided")
            
            print(f"Results found: {result_count}")
            print(f"Total count: {total_count}")
            
            acumem_files = [
                result for result in data.get("value", [])
                if "ACUMEM" in result.get("repo_path", "")
            ]
            
            print(f"ACUMEM files found: {len(acumem_files)}")
            
            for acumem in acumem_files:
                print(f"  - {acumem.get('repo_path', 'Unknown path')}")
                
            return data
            
        else:
            print(f"Error: {response.status_code}")
            print(f"Response: {response.text}")
            return None
            
    except Exception as e:
        print(f"Exception: {str(e)}")
        return None

def compare_requests():
    """Compare chatbot vs direct search requests"""
    
    print("🔍 DETAILED ACUMEM SEARCH COMPARISON")
    print("="*60)
    
    # Test 1: Chatbot-style search
    chatbot_request = create_chatbot_search_request()
    chatbot_results = execute_search(chatbot_request, "CHATBOT-STYLE SEARCH")
    
    # Test 2: Direct search (known working)
    direct_request = create_direct_search_request()
    direct_results = execute_search(direct_request, "DIRECT SEARCH (WORKING)")
    
    # Test 3: Identify the key difference
    print(f"\n{'='*60}")
    print(f"🔍 KEY DIFFERENCES ANALYSIS")
    print(f"{'='*60}")
    
    print("Chatbot request:")
    for key, value in chatbot_request.items():
        print(f"  {key}: {value}")
    
    print("\nDirect request:")
    for key, value in direct_request.items():
        print(f"  {key}: {value}")
    
    print("\nDifferences:")
    for key in set(chatbot_request.keys()) | set(direct_request.keys()):
        chatbot_val = chatbot_request.get(key, "MISSING")
        direct_val = direct_request.get(key, "MISSING")
        if chatbot_val != direct_val:
            print(f"  {key}: CHATBOT={chatbot_val}, DIRECT={direct_val}")
    
    # Test 4: Try chatbot request with count=True
    print(f"\n{'='*60}")
    print(f"🔍 TESTING FIX: CHATBOT REQUEST WITH count=True")
    print(f"{'='*60}")
    
    fixed_request = chatbot_request.copy()
    fixed_request["count"] = True
    fixed_results = execute_search(fixed_request, "FIXED CHATBOT SEARCH")
    
    if fixed_results and len(fixed_results.get("value", [])) > 0:
        print("\n✅ SUCCESS! The issue was count=False in chatbot search.")
        print("   Setting count=True fixes the search results.")
    else:
        print("\n❌ Setting count=True didn't fix the issue. Need deeper investigation.")

if __name__ == "__main__":
    if not all([AZURE_SEARCH_ENDPOINT, AZURE_SEARCH_KEY]):
        print("❌ Missing Azure Search configuration")
        sys.exit(1)
    
    compare_requests()
