#!/usr/bin/env python3
"""
Search for LONPF2 program specifically
"""

import json
import requests

def search_lonpf2():
    # Load settings
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)
        LOCAL_SETTINGS = settings.get('Values', {})

    SEARCH_ENDPOINT = LOCAL_SETTINGS.get('SEARCH_ENDPOINT')  
    SEARCH_KEY = LOCAL_SETTINGS.get('SEARCH_KEY')

    headers = {'Content-Type': 'application/json', 'api-key': SEARCH_KEY}
    search_url = f'{SEARCH_ENDPOINT}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview'

    print("=== SEARCHING FOR LONPF2 PROGRAM ===")
    
    # Search for LONPF2 program
    search_body = {
        'search': 'LONPF2',
        'top': 50,
        'select': 'repo_path,code'
    }

    try:
        response = requests.post(search_url, headers=headers, json=search_body, timeout=10)
        
        if response.status_code == 200:
            results = response.json()
            docs = results.get('value', [])
            total_count = results.get('@odata.count', 0)
            
            print(f'Found {len(docs)} results out of {total_count:,} total matches\\n')
            
            if not docs:
                print("❌ No LONPF2 program found in the codebase")
                return
            
            # Group by files
            files = {}
            for doc in docs:
                file_path = doc.get('repo_path', '')
                if file_path not in files:
                    files[file_path] = []
                files[file_path].append(doc)
            
            print(f"📊 LONPF2 found in {len(files)} file(s):")
            for file_path in files.keys():
                print(f"  • {file_path}")
            print()
            
            # Show detailed content from first file
            first_file = list(files.keys())[0]
            print(f"📄 DETAILED ANALYSIS - {first_file}:")
            print("=" * 60)
            
            for i, doc in enumerate(files[first_file][:5]):
                code = doc.get('code', '').strip()
                print(f"\\n--- Code Segment {i+1} ---")
                print(code[:1000] + ('\\n... (truncated)' if len(code) > 1000 else ''))
            
            if len(files[first_file]) > 5:
                print(f"\\n... and {len(files[first_file])-5} more code segments")
            
            # Look for program structure
            all_code = []
            for file_docs in files.values():
                for doc in file_docs:
                    all_code.append(doc.get('code', ''))
            
            full_text = '\\n'.join(all_code).upper()
            
            print("\\n🔍 PROGRAM STRUCTURE ANALYSIS:")
            print("-" * 40)
            
            if 'PROGRAM-ID' in full_text:
                print("✅ Contains PROGRAM-ID declaration")
            if 'IDENTIFICATION DIVISION' in full_text:
                print("✅ Has IDENTIFICATION DIVISION")
            if 'DATA DIVISION' in full_text:
                print("✅ Has DATA DIVISION")
            if 'PROCEDURE DIVISION' in full_text:
                print("✅ Has PROCEDURE DIVISION")
            if 'WORKING-STORAGE' in full_text:
                print("✅ Has WORKING-STORAGE SECTION")
            if 'CALL' in full_text:
                print("✅ Makes program calls")
            if 'PERFORM' in full_text:
                print("✅ Uses PERFORM statements")
            if 'READ' in full_text or 'WRITE' in full_text:
                print("✅ Performs file operations")
            
        else:
            print(f'❌ Search failed: HTTP {response.status_code}')
            
    except Exception as e:
        print(f'❌ Error: {e}')

if __name__ == "__main__":
    search_lonpf2()
