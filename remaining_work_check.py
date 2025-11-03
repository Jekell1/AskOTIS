#!/usr/bin/env python3
"""Quick status check of remaining indexes that need work."""

import requests
import os
import json

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

def get_index_count(endpoint: str, key: str, index_name: str) -> int:
    """Get document count for an index using count endpoint."""
    try:
        url = f"{endpoint}/indexes/{index_name}/docs/$count?api-version=2023-11-01"
        r = requests.get(url, headers={'api-key': key}, timeout=30)
        if r.status_code == 200:
            return int(r.text.strip())
    except:
        pass
    
    # Fallback to search with count
    try:
        url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2023-11-01"
        body = {'search': '*', 'top': 0}
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body, timeout=30)
        if r.status_code == 200:
            data = r.json()
            return data.get('@odata.count', 0)
    except:
        pass
    
    return 0

def main():
    endpoint, key = load_config()
    
    print("=== REMAINING WORK STATUS CHECK ===")
    print("Target: 9,678 programs with full coverage\n")
    
    # Check the indexes we know need work
    indexes_to_check = [
        # Core indexes (should be 9,678)
        ('new_cobol_program_inventory', 9678, 'Program inventory records'),
        
        # Specialized indexes (need zero-record approach)  
        ('new_cobol_menu_trees', 9678, 'Menu tree records'),
        ('new_cobol_screen_nodes', 9678, 'Screen node records'),
        ('new_cobol_ui_paths', 9678, 'UI path records'),
        
        # Variable usage (should have program_id populated)
        ('new_cobol_variable_usage', 1, 'Variable usage with program_id'),
    ]
    
    remaining_work = []
    
    for index_name, expected, description in indexes_to_check:
        count = get_index_count(endpoint, key, index_name)
        
        if index_name == 'new_cobol_variable_usage':
            status = "✓ FIXED" if count > 100000 else "? CHECK"
            print(f"  {status} | {index_name:35} | {count:,} records")
            if count == 0:
                remaining_work.append(f"{index_name}: Needs program_id population")
        else:
            gap = max(0, expected - count)
            if count >= expected:
                status = "✓ COMPLETE"
                print(f"  {status} | {index_name:35} | {count:,}/{expected:,} records")
            else:
                status = "✗ PARTIAL"
                print(f"  {status} | {index_name:35} | {count:,}/{expected:,} records ({gap:,} missing)")
                remaining_work.append(f"{index_name}: {gap:,} records missing")
    
    print("\n" + "="*60)
    
    if remaining_work:
        print(f"REMAINING WORK ({len(remaining_work)} items):")
        for i, item in enumerate(remaining_work, 1):
            print(f"  {i}. {item}")
        
        print(f"\nNEXT PRIORITIES:")
        if any('menu_trees' in item for item in remaining_work):
            print("  → Fix new_cobol_menu_trees (add zero-menu records)")
        if any('screen_nodes' in item for item in remaining_work):
            print("  → Fix new_cobol_screen_nodes (add zero-screen records)")  
        if any('ui_paths' in item for item in remaining_work):
            print("  → Fix new_cobol_ui_paths (add zero-path records)")
        if any('variable_usage' in item for item in remaining_work):
            print("  → Check new_cobol_variable_usage program_id population")
    else:
        print("🎉 ALL PRIORITY INDEXES COMPLETE! 🎉")
        print("Full coverage achieved on all target indexes!")

if __name__ == '__main__':
    main()