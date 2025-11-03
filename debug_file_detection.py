#!/usr/bin/env python3

"""
Debug why accumem.cbl still shows variables from other files
"""

import json
import requests
import sys
import re

def load_settings():
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except:
        return {}

def search_azure(query, settings, top=20):
    search_endpoint = settings.get('SEARCH_ENDPOINT')
    search_key = settings.get('SEARCH_KEY')
    
    if not search_endpoint or not search_key:
        return {'error': 'Missing search endpoint or key'}
    
    url = f"{search_endpoint}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview"
    headers = {'Content-Type': 'application/json', 'api-key': search_key}
    
    body = {'search': query, 'top': top}
    
    try:
        response = requests.post(url, headers=headers, json=body)
        if response.status_code == 200:
            return response.json()
        else:
            return {'error': f'HTTP {response.status_code}: {response.text}'}
    except Exception as e:
        return {'error': f'Exception: {e}'}

def simulate_chatbot_logic(user_query, search_results):
    """Simulate the exact logic from the chatbot"""
    
    print(f"\n🔍 Simulating chatbot logic for: '{user_query}'")
    print("=" * 50)
    
    # Step 1: Check if results exist
    results = search_results.get("value", [])
    error = search_results.get("error")
    
    if error:
        print(f"❌ Search Error: {error}")
        return "error_path"
        
    if not results:
        print("❌ No results found - would call _handle_no_results")
        return "no_results_path"
    
    print(f"✅ Found {len(results)} results")
    
    # Step 2: File detection logic (this should catch accumem.cbl)
    file_pattern = r'\b(\w+\.cbl|\w+\.CBL)\b'
    file_matches = re.findall(file_pattern, user_query)
    
    print(f"🔍 File detection regex found: {file_matches}")
    
    if file_matches and results:
        requested_file = file_matches[0].lower()
        found_files = [r.get("repo_path", "").lower() for r in results]
        
        print(f"📁 Requested file: '{requested_file}'")
        print(f"📂 Found file paths: {found_files[:3]}...")
        
        # Check if the requested file is actually in the results
        file_found = any(requested_file.replace('.cbl', '') in path for path in found_files)
        
        print(f"🎯 File found in results: {file_found}")
        
        if not file_found:
            print("✅ SHOULD SHOW: File not found message")
            return "file_not_found_message"
        else:
            print("✅ File found - would proceed normally")
            return "normal_processing"
    
    print("⚠️  No file-specific query detected - would proceed to LLM")
    return "llm_processing"

def main():
    settings = load_settings()
    
    print("DEBUG: Why accumem.cbl still shows variables from other files")
    print("=" * 60)
    
    # Test the exact query you used
    user_query = "show me the variables in accumem.cbl"
    
    print(f"User query: '{user_query}'")
    
    # Get search results
    search_results = search_azure(user_query, settings, top=20)
    
    # Simulate the chatbot decision path
    path_taken = simulate_chatbot_logic(user_query, search_results)
    
    print(f"\n🎯 DECISION PATH: {path_taken}")
    
    if path_taken == "llm_processing":
        print("\n❌ PROBLEM: The query is going to LLM processing instead of file detection!")
        print("This means the file detection logic isn't working properly.")
        
        # Debug why
        print("\nDebugging file detection...")
        file_pattern = r'\b(\w+\.cbl|\w+\.CBL)\b'
        matches = re.findall(file_pattern, user_query)
        print(f"Regex matches: {matches}")
        
        if matches:
            print("✅ Regex works - file detected")
            
            results = search_results.get("value", [])
            if results:
                requested_file = matches[0].lower()
                found_files = [r.get("repo_path", "").lower() for r in results]
                file_found = any(requested_file.replace('.cbl', '') in path for path in found_files)
                
                print(f"Requested: '{requested_file}'")
                print(f"File paths: {found_files[:3]}")
                print(f"Match found: {file_found}")
                
                if not file_found:
                    print("✅ Logic should work - check code placement in chatbot!")
                else:
                    print("❌ File was found in results (unexpected)")
            else:
                print("❌ No results to check")
        else:
            print("❌ Regex failed to match file in query")
    
    elif path_taken == "file_not_found_message":
        print("\n✅ SUCCESS: Should show file not found message")
    
    print("\n" + "=" * 60)

if __name__ == "__main__":
    main()
