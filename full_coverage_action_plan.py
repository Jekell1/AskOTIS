#!/usr/bin/env python3
"""Action plan to achieve 100% coverage on all remaining COBOL indexes.

Priority order based on impact and complexity:
1. new_cobol_variable_usage (0% - likely detection issue)
2. new_cobol_program_inventory (1.8% - 9,503 missing programs)  
3. new_cobol_copybook_meta (80.8% - 1,861 missing copybooks)
4. new_cobol_menu_trees (16.2% - 8,106 missing programs)
5. new_cobol_screen_nodes (8.2% - 8,889 missing programs)
6. new_cobol_ui_paths (1.7% - 9,513 missing programs)
"""

import os
import json
import requests
from typing import Dict, List

API_VERSION = '2023-11-01'

def load_config():
    """Load Azure Search endpoint and key."""
    vals = {}
    if os.path.exists('local.settings.json'):
        try:
            with open('local.settings.json', 'r', encoding='utf-8') as f:
                vals = json.load(f).get('Values', {})
        except Exception:
            pass
    
    def first(*names):
        for n in names:
            v = os.getenv(n) or vals.get(n)
            if v:
                return v
        return None
    
    endpoint = first('AZURE_SEARCH_ENDPOINT', 'SearchEndpoint')
    key = first('AZURE_SEARCH_KEY', 'SearchKey')
    
    if not endpoint or not key:
        raise ValueError("Missing Azure Search configuration")
    
    return endpoint, key

def analyze_index_structure(endpoint: str, key: str, index_name: str):
    """Analyze the structure and data patterns of an index."""
    print(f"\n=== ANALYZING {index_name.upper()} ===")
    
    # Get sample documents
    url = f"{endpoint}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    body = {'search': '*', 'top': 10}
    
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    if r.status_code != 200:
        print(f"Error getting sample: {r.status_code}")
        return
    
    data = r.json()
    docs = data.get('value', [])
    
    if not docs:
        print("No documents found")
        return
    
    print(f"Sample document structure:")
    first_doc = docs[0]
    for field, value in first_doc.items():
        value_preview = str(value)[:50] + "..." if len(str(value)) > 50 else str(value)
        print(f"  {field}: {value_preview}")
    
    # Get total count
    count_url = f"{endpoint}/indexes/{index_name}/docs/$count?api-version={API_VERSION}"
    r = requests.get(count_url, headers={'api-key': key})
    total_docs = int(r.text) if r.status_code == 200 else 0
    print(f"Total documents: {total_docs:,}")
    
    return docs, total_docs

def main():
    endpoint, key = load_config()
    
    print("=== FULL COVERAGE ACTION PLAN ===")
    print("Analyzing remaining indexes to understand coverage gaps...")
    
    # Priority order for fixing
    indexes_to_fix = [
        'new_cobol_variable_usage',    # 0% - likely detection issue
        'new_cobol_program_inventory', # 1.8% - 9,503 missing  
        'new_cobol_copybook_meta',     # 80.8% - 1,861 missing
        'new_cobol_menu_trees',        # 16.2% - 8,106 missing
        'new_cobol_screen_nodes',      # 8.2% - 8,889 missing
        'new_cobol_ui_paths'           # 1.7% - 9,513 missing
    ]
    
    for index_name in indexes_to_fix:
        docs, total_docs = analyze_index_structure(endpoint, key, index_name)
        
        # Suggest next actions based on the structure
        if index_name == 'new_cobol_variable_usage':
            print("🔧 RECOMMENDATION: Check if this has variable usage data that needs program_id enrichment")
            print("   Action: Look for existing build script or create variable usage enrichment")
            
        elif index_name == 'new_cobol_program_inventory':
            print("🔧 RECOMMENDATION: Need to identify what makes a program qualify for 'inventory'")
            print("   Action: Compare existing 175 programs vs missing 9,503 to understand criteria")
            
        elif index_name == 'new_cobol_copybook_meta':
            print("🔧 RECOMMENDATION: This covers copybooks, not programs - different coverage metric")
            print("   Action: Verify all .CPY files are represented, not just .CBL programs")
            
        elif index_name == 'new_cobol_menu_trees':
            print("🔧 RECOMMENDATION: Only programs with menu structures should be here")
            print("   Action: Check if missing programs actually have menus or need zero-menu records")
            
        elif index_name == 'new_cobol_screen_nodes':
            print("🔧 RECOMMENDATION: Only programs with screen definitions should be here")
            print("   Action: Check if missing programs have screens or need zero-screen records")
            
        elif index_name == 'new_cobol_ui_paths':
            print("🔧 RECOMMENDATION: Only programs with UI navigation should be here")
            print("   Action: Check if missing programs have UI paths or need zero-path records")

    print(f"\n=== NEXT STEPS ===")
    print("1. Start with new_cobol_variable_usage (likely quick fix)")
    print("2. Tackle new_cobol_program_inventory (understand criteria)")
    print("3. Handle specialized indexes (menu, screen, ui) with zero-record approach")
    print("4. Verify copybook_meta covers all .CPY files")

if __name__ == '__main__':
    main()