#!/usr/bin/env python3
"""
Fast LONPF2 Analysis with Streaming
===================================
Immediate response version for specific program queries
"""

import json
import requests
import time
from typing import List, Dict, Any

def load_local_settings():
    try:
        with open('local.settings.json', 'r') as f:
            settings = json.load(f)
            return settings.get('Values', {})
    except:
        return {}

def quick_lonpf2_analysis():
    """Ultra-fast LONPF2 analysis"""
    
    print("🚀 Ultra-Fast LONPF2 Analysis")
    print("=" * 40)
    
    settings = load_local_settings()
    
    # Step 1: Direct search for LONPF2 (< 1 second)
    print("🔍 Searching for LONPF2...")
    start = time.time()
    
    search_url = f"{settings.get('SEARCH_ENDPOINT')}/indexes/cobol-index/docs/search?api-version=2023-07-01-preview"
    headers = {'Content-Type': 'application/json', 'api-key': settings.get('SEARCH_KEY')}
    
    search_body = {
        'search': 'LONPF2',
        'select': 'repo_path,line,code,symbol_name,symbol_kind,calls',
        'top': 10,  # Just get the most relevant
        'count': True
    }
    
    try:
        response = requests.post(search_url, headers=headers, json=search_body, timeout=5)
        search_time = time.time() - start
        
        if response.status_code == 200:
            results = response.json()
            total_matches = results.get('@odata.count', 0)
            found_results = results.get('value', [])
            
            print(f"✅ Search completed in {search_time:.2f}s")
            print(f"📊 Found {total_matches} total matches, analyzing top {len(found_results)}")
            print()
            
            # Step 2: Immediate pattern-based analysis (< 1 second)
            print("⚡ Quick Analysis (Pattern-Based):")
            print("-" * 30)
            
            # Find the main LONPF2 file
            main_file = None
            for result in found_results:
                file_path = result.get('repo_path', '')
                if 'LONPF2.CBL' in file_path.upper():
                    main_file = result
                    break
            
            if main_file:
                file_path = main_file.get('repo_path', '')
                code = main_file.get('code', '')
                
                # Handle JSON array format
                if isinstance(code, list):
                    code = '\n'.join(code)
                
                print(f"📁 **Main File**: {file_path}")
                print(f"📝 **File Size**: {len(code):,} characters")
                
                # Quick pattern analysis
                patterns_found = []
                if 'PROGRAM-ID' in code.upper():
                    patterns_found.append("✓ Main COBOL program")
                if 'IDENTIFICATION DIVISION' in code.upper():
                    patterns_found.append("✓ Standard COBOL structure")
                if 'PROCEDURE DIVISION' in code.upper():
                    patterns_found.append("✓ Contains executable logic")
                if 'WORKING-STORAGE' in code.upper():
                    patterns_found.append("✓ Has data definitions")
                if 'CALL' in code.upper():
                    patterns_found.append("✓ Calls other programs")
                if 'COPY' in code.upper():
                    patterns_found.append("✓ Uses copybooks")
                    
                print(f"🔍 **Program Structure**:")
                for pattern in patterns_found:
                    print(f"   {pattern}")
                
                # Extract key sections
                lines = code.split('\n') if isinstance(code, str) else code
                key_sections = []
                
                for i, line in enumerate(lines[:50]):  # Check first 50 lines
                    line_upper = line.upper().strip()
                    if 'PROGRAM-ID' in line_upper:
                        key_sections.append(f"Line {i+1}: {line.strip()}")
                    elif any(keyword in line_upper for keyword in ['IDENTIFICATION', 'PROCEDURE', 'DATA DIVISION']):
                        key_sections.append(f"Line {i+1}: {line.strip()}")
                
                if key_sections:
                    print(f"\n📋 **Key Program Sections**:")
                    for section in key_sections[:5]:
                        print(f"   {section}")
                
                print(f"\n⚡ **Quick Analysis completed in {time.time() - start:.2f} seconds**")
                
                # Step 3: Offer deeper analysis option
                print(f"\n🤖 **For AI-Powered Deep Analysis**:")
                print(f"   • Run the full chatbot for detailed explanation")
                print(f"   • Ask specific questions about variables, procedures, or logic")
                print(f"   • Note: AI analysis takes ~20 seconds due to GPT-5 processing time")
                
            else:
                print("❓ LONPF2 main file not found in top results")
                print("Found references in other files:")
                for result in found_results[:3]:
                    file_path = result.get('repo_path', '')
                    line = result.get('line', 'N/A')
                    print(f"   • {file_path} (Line {line})")
        else:
            print(f"❌ Search failed: {response.status_code}")
            
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    quick_lonpf2_analysis()
