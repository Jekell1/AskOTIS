#!/usr/bin/env python3
"""
Find all CALL statements in LONPF2 program
"""

import requests
import json
import re

# Configuration
SEARCH_ENDPOINT = "https://az-use1-ai-search.search.windows.net"
SEARCH_KEY = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
SEARCH_INDEX = "cobol-index"

headers = {
    'Content-Type': 'application/json',
    'api-key': SEARCH_KEY
}

def find_lonpf2_calls():
    """Find all programs called by LONPF2"""
    
    print("=== SEARCHING FOR CALL STATEMENTS IN LONPF2.CBL ===")
    
    # Search for all lines in LONPF2.CBL that contain CALL
    search_body = {
        "search": "CALL",
        "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
        "filter": "search.ismatch('LONPF2.CBL', 'repo_path')",
        "top": 100,
        "count": True
    }
    
    url = f"{SEARCH_ENDPOINT}/indexes/{SEARCH_INDEX}/docs/search"
    
    try:
        response = requests.post(
            url,
            headers=headers,
            params={"api-version": "2024-07-01"},
            json=search_body,
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"Search failed: {response.status_code}")
            print(response.text)
            return
            
        results = response.json()
        total_count = results.get("@odata.count", 0)
        documents = results.get("value", [])
        
        print(f"Found {total_count} CALL statements in LONPF2.CBL files")
        
        called_programs = set()
        copy_books = set()
        
        for doc in documents:
            line = doc.get("line", 0)
            code = doc.get("code", "").strip()
            calls = doc.get("calls", [])
            
            if "CALL" in code.upper():
                print(f"\nLine {line}: {code}")
                
                # Extract program name from CALL statement
                call_pattern = r"CALL\s+['\"]([^'\"]+)['\"]"
                matches = re.findall(call_pattern, code, re.IGNORECASE)
                for match in matches:
                    called_programs.add(match.strip())
            
            # Also check metadata calls
            if calls:
                for prog in calls:
                    called_programs.add(prog)
            
            # Look for COPY statements too
            if "COPY" in code.upper():
                copy_pattern = r"COPY\s+([^\s\.]+)"
                matches = re.findall(copy_pattern, code, re.IGNORECASE)
                for match in matches:
                    copy_books.add(match.strip())
        
        print(f"\n=== PROGRAMS CALLED BY LONPF2 ===")
        if called_programs:
            for i, prog in enumerate(sorted(called_programs), 1):
                print(f"{i}. {prog}")
        else:
            print("No program calls found in the search results")
        
        print(f"\n=== COPYBOOKS USED BY LONPF2 ===") 
        if copy_books:
            for i, book in enumerate(sorted(copy_books), 1):
                print(f"{i}. {book}")
        else:
            print("No copybooks found in the search results")
        
        return called_programs, copy_books
        
    except Exception as e:
        print(f"Search error: {e}")
        return None, None

def search_broader_lonpf2():
    """Search for all content in LONPF2 files"""
    
    print("\n=== BROADER SEARCH IN LONPF2 FILES ===")
    
    search_body = {
        "search": "*",
        "select": "repo_path,line,code,symbol_name,symbol_kind,calls",
        "filter": "search.ismatch('LONPF2', 'repo_path')",
        "top": 200,
        "count": True
    }
    
    url = f"{SEARCH_ENDPOINT}/indexes/{SEARCH_INDEX}/docs/search"
    
    try:
        response = requests.post(
            url,
            headers=headers,
            params={"api-version": "2024-07-01"},
            json=search_body,
            timeout=30
        )
        
        if response.status_code != 200:
            print(f"Search failed: {response.status_code}")
            return
            
        results = response.json()
        total_count = results.get("@odata.count", 0)
        documents = results.get("value", [])
        
        print(f"Found {total_count} total lines in LONPF2 files")
        
        called_programs = set()
        copy_books = set()
        
        for doc in documents:
            code = doc.get("code", "").strip().upper()
            calls = doc.get("calls", [])
            
            # Look for CALL statements
            if "CALL" in code:
                call_pattern = r"CALL\s+['\"]([^'\"]+)['\"]"
                matches = re.findall(call_pattern, code, re.IGNORECASE)
                for match in matches:
                    called_programs.add(match.strip())
                    
                if matches:
                    print(f"Line {doc.get('line', 0)}: {doc.get('code', '').strip()}")
            
            # Look for COPY statements
            if "COPY" in code:
                copy_pattern = r"COPY\s+([^\s\.]+)"
                matches = re.findall(copy_pattern, code, re.IGNORECASE)
                for match in matches:
                    copy_books.add(match.strip())
                    
                if matches:
                    print(f"Line {doc.get('line', 0)}: {doc.get('code', '').strip()}")
            
            # Add metadata calls
            if calls:
                for prog in calls:
                    called_programs.add(prog)
        
        print(f"\n=== ALL PROGRAMS CALLED BY LONPF2 ===")
        if called_programs:
            for i, prog in enumerate(sorted(called_programs), 1):
                print(f"{i}. {prog}")
        else:
            print("No program calls found")
        
        print(f"\n=== ALL COPYBOOKS USED BY LONPF2 ===") 
        if copy_books:
            for i, book in enumerate(sorted(copy_books), 1):
                print(f"{i}. {book}")
        
        return called_programs, copy_books
        
    except Exception as e:
        print(f"Search error: {e}")
        return None, None

if __name__ == "__main__":
    # First try targeted search
    calls1, copies1 = find_lonpf2_calls()
    
    # Then try broader search
    calls2, copies2 = search_broader_lonpf2()
    
    # Combine results
    all_calls = set()
    all_copies = set()
    
    if calls1:
        all_calls.update(calls1)
    if calls2:
        all_calls.update(calls2)
    if copies1:
        all_copies.update(copies1)
    if copies2:
        all_copies.update(copies2)
    
    print(f"\n{'='*60}")
    print("FINAL SUMMARY: LONPF2 DEPENDENCIES")
    print(f"{'='*60}")
    
    print(f"\n🔗 PROGRAM DEPENDENCIES ({len(all_calls)} total):")
    for i, prog in enumerate(sorted(all_calls), 1):
        print(f"  {i:2d}. {prog}")
    
    print(f"\n📚 COPYBOOK DEPENDENCIES ({len(all_copies)} total):")
    for i, book in enumerate(sorted(all_copies), 1):
        print(f"  {i:2d}. {book}")
    
    print(f"\n💡 NEXT STEPS:")
    print("  • Each called program may have its own dependencies")
    print("  • Run this script on each called program to get full dependency tree")
    print("  • Check copybooks for additional program references")
