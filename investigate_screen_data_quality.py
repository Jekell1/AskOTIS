#!/usr/bin/env python3
"""
Investigate data quality issues in the screen nodes index
"""

import os
import requests
from secrets_loader import load_secrets

def investigate_data_quality():
    """Investigate data quality issues in screen nodes."""
    print("🔍 INVESTIGATING SCREEN NODES DATA QUALITY")
    print("=" * 50)
    
    load_secrets()
    
    search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    
    headers = {
        'api-key': search_key,
        'Content-Type': 'application/json'
    }
    
    search_url = f"{search_endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
    # 1. Count records with null screen names
    print("\n📊 CHECKING FOR NULL SCREEN NAMES:")
    null_screen_names_body = {
        "search": "*",
        "filter": "screen_name eq null",
        "top": 0,
        "count": True
    }
    
    response = requests.post(search_url, headers=headers, json=null_screen_names_body)
    if response.status_code == 200:
        null_count = response.json().get('@odata.count', 0)
        print(f"   Records with null screen_name: {null_count}")
    
    # 2. Count records with empty screen names
    print("\n📊 CHECKING FOR EMPTY SCREEN NAMES:")
    empty_screen_names_body = {
        "search": "*",
        "filter": "screen_name eq ''",
        "top": 0,
        "count": True
    }
    
    response = requests.post(search_url, headers=headers, json=empty_screen_names_body)
    if response.status_code == 200:
        empty_count = response.json().get('@odata.count', 0)
        print(f"   Records with empty screen_name: {empty_count}")
    
    # 3. Get sample records with null/empty screen names
    print("\n🔍 SAMPLE RECORDS WITH MISSING SCREEN NAMES:")
    sample_body = {
        "search": "*",
        "filter": "screen_name eq null or screen_name eq ''",
        "top": 5,
        "select": "screen_id,screen_name,program_id,screen_type,summary_text,raw_span_text"
    }
    
    response = requests.post(search_url, headers=headers, json=sample_body)
    if response.status_code == 200:
        results = response.json().get('value', [])
        for i, result in enumerate(results, 1):
            print(f"\n   Sample {i}:")
            print(f"      screen_id: {result.get('screen_id')}")
            print(f"      screen_name: {repr(result.get('screen_name'))}")
            print(f"      program_id: {result.get('program_id')}")
            print(f"      screen_type: {result.get('screen_type')}")
            print(f"      summary_text: {result.get('summary_text')}")
            
            raw_text = result.get('raw_span_text', '')
            if raw_text:
                print(f"      raw_span_text (first 200 chars): {raw_text[:200]}...")
    
    # 4. Check overall data completeness
    print("\n📊 DATA COMPLETENESS ANALYSIS:")
    
    fields_to_check = ['screen_name', 'program_id', 'screen_type', 'summary_text']
    
    for field in fields_to_check:
        # Count non-null values
        non_null_body = {
            "search": "*",
            "filter": f"{field} ne null and {field} ne ''",
            "top": 0,
            "count": True
        }
        
        response = requests.post(search_url, headers=headers, json=non_null_body)
        if response.status_code == 200:
            non_null_count = response.json().get('@odata.count', 0)
            total_count = 5663  # We know the total
            completeness = (non_null_count / total_count) * 100
            print(f"   {field}: {non_null_count}/{total_count} ({completeness:.1f}% complete)")
    
    # 5. Check for patterns in problematic records
    print("\n🔍 ANALYZING PROBLEMATIC RECORDS PATTERNS:")
    
    # Get records with vectors but missing basic fields
    pattern_body = {
        "search": "*",
        "filter": "has_vector eq true and (screen_name eq null or screen_name eq '')",
        "top": 10,
        "select": "screen_id,screen_name,program_id,screen_type,summary_text,field_count"
    }
    
    response = requests.post(search_url, headers=headers, json=pattern_body)
    if response.status_code == 200:
        results = response.json().get('value', [])
        print(f"   Found {len(results)} records with vectors but missing screen_name")
        
        for i, result in enumerate(results[:3], 1):  # Show first 3
            print(f"\n   Pattern Sample {i}:")
            print(f"      screen_id: {result.get('screen_id')}")
            print(f"      field_count: {result.get('field_count')}")
            print(f"      summary_text: {result.get('summary_text')}")

if __name__ == "__main__":
    investigate_data_quality()