#!/usr/bin/env python3
"""
Build complete menu trees index with 100% program coverage.
Creates zero-menu records for programs without menu structures.
"""

import requests
import os
import json
from typing import Set, Dict, Any

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

def get_all_programs(endpoint: str, key: str) -> Set[str]:
    """Get all program names from program_inventory index (complete set)."""
    programs = set()
    
    url = f"{endpoint}/indexes/new_cobol_program_inventory/docs/search?api-version=2023-11-01"
    
    skip = 0
    batch_size = 1000
    
    while True:
        body = {
            'search': '*',
            'select': 'program_id',
            'top': batch_size,
            'skip': skip
        }
        
        try:
            response = requests.post(
                url,
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json=body,
                timeout=30
            )
            
            if response.status_code != 200:
                print(f"Error fetching programs: {response.status_code}")
                break
                
            data = response.json()
            docs = data.get('value', [])
            
            if not docs:
                break
                
            for doc in docs:
                if 'program_id' in doc:
                    programs.add(doc['program_id'])
            
            print(f"Collected {len(programs)} programs so far...")
            
            if len(docs) < batch_size:
                break
                
            skip += batch_size
            
        except Exception as e:
            print(f"Error in batch starting at {skip}: {e}")
            break
    
    return programs

def get_existing_menu_programs(endpoint: str, key: str) -> Set[str]:
    """Get programs that already have menu tree records."""
    programs = set()
    
    url = f"{endpoint}/indexes/new_cobol_menu_trees/docs/search?api-version=2023-11-01"
    
    skip = 0
    batch_size = 1000
    
    while True:
        body = {
            'search': '*',
            'select': 'root_program_id',
            'top': batch_size,
            'skip': skip
        }
        
        try:
            response = requests.post(
                url,
                headers={'api-key': key, 'Content-Type': 'application/json'},
                json=body,
                timeout=30
            )
            
            if response.status_code != 200:
                print(f"Error fetching existing menu programs: {response.status_code}")
                break
                
            data = response.json()
            docs = data.get('value', [])
            
            if not docs:
                break
                
            for doc in docs:
                if 'root_program_id' in doc:
                    programs.add(doc['root_program_id'])
            
            if len(docs) < batch_size:
                break
                
            skip += batch_size
            
        except Exception as e:
            print(f"Error in batch starting at {skip}: {e}")
            break
    
    return programs

def create_zero_menu_record(program_name: str) -> Dict[str, Any]:
    """Create a zero-menu record for a program without menu structures."""
    return {
        '@search.action': 'upload',
        'root_program_id': program_name,
        'tree_json': '{"nodes": [], "message": "NO_MENU_STRUCTURE"}',
        'total_nodes': 0,
        'total_ui_nodes': 0,
        'ui_ratio': 0.0,
        'max_depth': 0,
        'build_params_json': '{"coverage_mode": "zero_record"}',
        'generated_at': '2025-10-08T00:00:00Z',
        'doc_type': 'zero_menu_tree'
    }

def upload_batch(endpoint: str, key: str, batch: list) -> bool:
    """Upload a batch of documents to the menu trees index."""
    if not batch:
        return True
        
    url = f"{endpoint}/indexes/new_cobol_menu_trees/docs/index?api-version=2023-11-01"
    
    body = {'value': batch}
    
    try:
        response = requests.post(
            url,
            headers={'api-key': key, 'Content-Type': 'application/json'},
            json=body,
            timeout=60
        )
        
        if response.status_code in [200, 201]:
            return True
        else:
            print(f"Upload error: {response.status_code} - {response.text}")
            return False
            
    except Exception as e:
        print(f"Upload exception: {e}")
        return False

def main():
    print("=== BUILDING COMPLETE MENU TREES INDEX ===")
    
    endpoint, key = load_config()
    
    # Step 1: Get all programs
    print("\n1. Getting all programs from program_meta...")
    all_programs = get_all_programs(endpoint, key)
    print(f"Found {len(all_programs)} total programs")
    
    # Step 2: Get programs with existing menu records
    print("\n2. Getting programs with existing menu trees...")
    existing_menu_programs = get_existing_menu_programs(endpoint, key)
    print(f"Found {len(existing_menu_programs)} programs with menu trees")
    
    # Step 3: Find programs needing zero-menu records
    programs_needing_zero_menus = all_programs - existing_menu_programs
    print(f"\n3. Programs needing zero-menu records: {len(programs_needing_zero_menus)}")
    
    if not programs_needing_zero_menus:
        print("✅ All programs already have menu tree records!")
        return
    
    # Step 4: Create and upload zero-menu records
    print("\n4. Creating zero-menu records...")
    
    batch = []
    batch_size = 100
    total_uploaded = 0
    
    for program_name in sorted(programs_needing_zero_menus):
        zero_record = create_zero_menu_record(program_name)
        batch.append(zero_record)
        
        if len(batch) >= batch_size:
            print(f"Uploading batch of {len(batch)} records...")
            if upload_batch(endpoint, key, batch):
                total_uploaded += len(batch)
                print(f"✓ Total uploaded: {total_uploaded}")
            else:
                print(f"✗ Failed to upload batch")
            batch = []
    
    # Upload final batch
    if batch:
        print(f"Uploading final batch of {len(batch)} records...")
        if upload_batch(endpoint, key, batch):
            total_uploaded += len(batch)
            print(f"✓ Final total uploaded: {total_uploaded}")
    
    print(f"\n=== COMPLETE ===")
    print(f"Zero-menu records created: {total_uploaded}")
    print(f"Expected total menu tree records: {len(all_programs)}")
    print(f"Programs with actual menus: {len(existing_menu_programs)}")
    print(f"Programs with zero-menu records: {total_uploaded}")

if __name__ == '__main__':
    main()