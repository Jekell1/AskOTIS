#!/usr/bin/env python3
"""
Find LONPF2 Program and Dependencies
"""

import requests
import json
import sys

# Configuration
SEARCH_ENDPOINT = "https://az-use1-ai-search.search.windows.net"
SEARCH_KEY = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
SEARCH_INDEX = "cobol-index"

headers = {
    'Content-Type': 'application/json',
    'api-key': SEARCH_KEY
}

def search_cobol(query, filter_expr=None, top=50):
    """Search the COBOL index"""
    
    search_body = {
        "search": query,
        "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
        "top": top,
        "count": True
    }
    
    if filter_expr:
        search_body["filter"] = filter_expr
    
    url = f"{SEARCH_ENDPOINT}/indexes/{SEARCH_INDEX}/docs/search"
    
    try:
        response = requests.post(
            url,
            headers=headers,
            params={"api-version": "2024-07-01"},
            json=search_body,
            timeout=30
        )
        
        if response.status_code == 200:
            return response.json()
        else:
            print(f"Search failed: {response.status_code}")
            print(response.text)
            return None
            
    except Exception as e:
        print(f"Search error: {e}")
        return None

def find_program_dependencies(program_name):
    """Find a program and its dependencies"""
    
    print(f"\n=== SEARCHING FOR PROGRAM: {program_name} ===")
    
    # 1. Search for the program itself
    results = search_cobol(program_name, top=100)
    
    if not results:
        print("No search results returned")
        return
    
    total_count = results.get("@odata.count", 0)
    documents = results.get("value", [])
    
    print(f"Found {total_count} total matches for '{program_name}'")
    
    if not documents:
        print("No documents found")
        return
    
    # Group by file
    files_found = {}
    call_statements = []
    copy_statements = []
    
    for doc in documents:
        file_path = doc.get("repo_path", "Unknown")
        line = doc.get("line", 0)
        code = doc.get("code", "").strip()
        calls = doc.get("calls", [])
        
        if file_path not in files_found:
            files_found[file_path] = []
        
        files_found[file_path].append({
            "line": line,
            "code": code,
            "calls": calls
        })
        
        # Look for CALL statements
        if "CALL" in code.upper() and program_name.upper() not in code.upper():
            call_statements.append({
                "file": file_path,
                "line": line,
                "code": code
            })
        
        # Look for COPY statements  
        if "COPY" in code.upper():
            copy_statements.append({
                "file": file_path,
                "line": line,
                "code": code
            })
        
        # Add calls from metadata
        if calls:
            for called_program in calls:
                call_statements.append({
                    "file": file_path,
                    "line": line,
                    "code": f"CALL '{called_program}' (from metadata)",
                    "called_program": called_program
                })
    
    # Show results
    print(f"\n=== FILES CONTAINING {program_name} ===")
    for file_path, entries in files_found.items():
        print(f"\nFile: {file_path}")
        for entry in entries[:10]:  # Limit to first 10 lines per file
            print(f"  Line {entry['line']}: {entry['code']}")
            if entry['calls']:
                print(f"    -> Calls: {entry['calls']}")
    
    # Show dependencies
    if call_statements:
        print(f"\n=== PROGRAM DEPENDENCIES (CALL Statements) ===")
        unique_calls = set()
        for call in call_statements:
            print(f"File: {call['file']}")
            print(f"  Line {call['line']}: {call['code']}")
            
            # Extract called program name
            if 'called_program' in call:
                unique_calls.add(call['called_program'])
        
        if unique_calls:
            print(f"\n=== SUMMARY: Programs called by {program_name} ===")
            for i, prog in enumerate(sorted(unique_calls), 1):
                print(f"{i}. {prog}")
    
    if copy_statements:
        print(f"\n=== COPYBOOK DEPENDENCIES (COPY Statements) ===")
        for copy in copy_statements[:20]:  # Limit output
            print(f"File: {copy['file']}")
            print(f"  Line {copy['line']}: {copy['code']}")
    
    return files_found, call_statements, copy_statements

if __name__ == "__main__":
    program_name = "LONPF2"
    
    if len(sys.argv) > 1:
        program_name = sys.argv[1]
    
    try:
        find_program_dependencies(program_name)
    except Exception as e:
        print(f"Error: {e}")
