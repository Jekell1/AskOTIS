#!/usr/bin/env python3
"""
Search for ACUMEM.CBL files in the Azure Search index
"""

import json
import requests
import sys

def main():
    print("🔍 Searching for ACUMEM.CBL files...")
    
    LOCAL_SETTINGS = {}
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            LOCAL_SETTINGS = settings.get('Values', {})
    except Exception as e:
        print(f"Could not load local.settings.json: {e}")
        return

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT', 'https://az-use1-ai-search.search.windows.net')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')

    if not SEARCH_KEY:
        print("❌ No search key available")
        return
    
    print(f"Using endpoint: {SEARCH_ENDPOINT}")
    
    url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'
    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    
    # Search for ACUMEM files with different variations
    queries = ['ACUMEM.CBL', 'acumem.cbl', 'ACUMEM', 'acumem']
    
    found_any = False
    
    for query in queries:
        print(f"\n📋 Query: '{query}'")
        body = {'search': query, 'top': 20}
        
        try:
            r = requests.post(url, headers=headers, json=body)
            print(f"   Status: {r.status_code}")
            
            if r.status_code == 200:
                results = r.json()
                total = len(results.get('value', []))
                print(f"   Total results: {total}")
                
                files = set()
                acumem_files = []
                
                for doc in results.get('value', []):
                    path = doc.get('repo_path', '')
                    files.add(path)
                    
                    if 'acumem' in path.lower():
                        acumem_files.append(path)
                
                if acumem_files:
                    print(f"   ✅ Found {len(acumem_files)} ACUMEM files:")
                    for f in sorted(acumem_files):
                        print(f"      - {f}")
                    found_any = True
                else:
                    print(f"   ❌ No ACUMEM files found")
                    if total > 0:
                        print(f"   📁 Other files found:")
                        for f in sorted(list(files)[:5]):
                            print(f"      - {f}")
                        if total > 5:
                            print(f"      ... and {total - 5} more")
                            
            else:
                print(f"   ❌ Error {r.status_code}: {r.text}")
                
        except Exception as e:
            print(f"   ❌ Exception: {e}")
    
    if not found_any:
        print("\n🔍 Trying broader search...")
        
        # Try searching for files with 'CL' in path (based on temp_sample.jsonl pattern)
        body = {'search': 'CL', 'top': 50}
        
        try:
            r = requests.post(url, headers=headers, json=body)
            if r.status_code == 200:
                results = r.json()
                cl_files = []
                
                for doc in results.get('value', []):
                    path = doc.get('repo_path', '')
                    if 'CL' in path and '.CBL' in path.upper():
                        cl_files.append(path)
                
                print(f"   📁 Found {len(cl_files)} CL/*.CBL files:")
                for f in sorted(cl_files)[:10]:
                    print(f"      - {f}")
                    if 'acumem' in f.lower():
                        print(f"         ⭐ This is an ACUMEM file!")
                        found_any = True
                        
                if len(cl_files) > 10:
                    print(f"      ... and {len(cl_files) - 10} more")
                    
        except Exception as e:
            print(f"   ❌ Broad search failed: {e}")
    
    print(f"\n{'✅' if found_any else '❌'} ACUMEM.CBL search complete")
    if found_any:
        print("   ACUMEM.CBL files exist in the index!")
    else:
        print("   ACUMEM.CBL files do not appear to be in the search index")
        print("   This confirms our earlier file-not-found detection was working correctly")

if __name__ == "__main__":
    main()
