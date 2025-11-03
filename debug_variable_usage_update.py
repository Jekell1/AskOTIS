#!/usr/bin/env python3
"""Debug variable usage update issues and retry with more targeted approach."""

import os
import json
import requests
import time

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

def test_single_update(endpoint: str, key: str):
    """Test updating a single variable usage record."""
    print("=== Testing single record update ===")
    
    # Get one record with first_write_program populated
    url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
    body = {
        'search': '*',
        'filter': 'first_write_program ne null',
        'select': 'symbol_id_global,first_write_program,program_id',
        'top': 1
    }
    
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    if r.status_code != 200:
        print(f"Error getting test record: {r.status_code}")
        return
        
    data = r.json()
    records = data.get('value', [])
    
    if not records:
        print("No records found with first_write_program")
        return
    
    record = records[0]
    symbol_id = record.get('symbol_id_global')
    first_write_program = record.get('first_write_program')
    current_program_id = record.get('program_id')
    
    print(f"Test record:")
    print(f"  symbol_id_global: {symbol_id}")
    print(f"  first_write_program: {first_write_program}")
    print(f"  current program_id: {current_program_id}")
    
    if current_program_id:
        print("Record already has program_id")
        return
    
    # Try to update this record
    update_doc = {
        'symbol_id_global': symbol_id,
        'program_id': first_write_program,
        '@search.action': 'merge'
    }
    
    print(f"Attempting update: {update_doc}")
    
    upload_url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/index?api-version={API_VERSION}"
    payload = {'value': [update_doc]}
    
    r = requests.post(upload_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload)
    print(f"Update response: {r.status_code}")
    if r.status_code not in (200, 201):
        print(f"Update failed: {r.text}")
    else:
        print("Update successful")
        
        # Wait and check if it took
        time.sleep(2)
        
        check_body = {
            'search': f'symbol_id_global:{symbol_id}',
            'select': 'symbol_id_global,program_id,first_write_program'
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=check_body)
        if r.status_code == 200:
            check_data = r.json()
            check_records = check_data.get('value', [])
            if check_records:
                check_record = check_records[0]
                print(f"After update:")
                print(f"  program_id: {check_record.get('program_id')}")
                print(f"  first_write_program: {check_record.get('first_write_program')}")

def check_index_structure(endpoint: str, key: str):
    """Check the index structure to understand field configuration."""
    print("\n=== Checking index structure ===")
    
    url = f"{endpoint}/indexes/new_cobol_variable_usage?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    
    if r.status_code == 200:
        index_def = r.json()
        fields = index_def.get('fields', [])
        
        print("Index fields:")
        for field in fields:
            name = field.get('name')
            field_type = field.get('type')
            searchable = field.get('searchable', False)
            retrievable = field.get('retrievable', True)
            print(f"  {name}: {field_type} (searchable: {searchable}, retrievable: {retrievable})")
    else:
        print(f"Error getting index definition: {r.status_code}")

def main():
    endpoint, key = load_config()
    
    print("=== DEBUGGING VARIABLE USAGE UPDATE ISSUES ===")
    
    # Check index structure
    check_index_structure(endpoint, key)
    
    # Test single update
    test_single_update(endpoint, key)

if __name__ == '__main__':
    main()