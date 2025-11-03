#!/usr/bin/env python3
"""
Build complete screen nodes index with 100% program coverage.
Creates zero-screen records for programs without screen structures.
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

def get_existing_screen_programs(endpoint: str, key: str) -> Set[str]:
    """Get programs that already have screen node records."""
    programs = set()
    
    url = f"{endpoint}/indexes/new_cobol_screen_nodes/docs/search?api-version=2023-11-01"
    
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
                print(f"Error fetching existing screen programs: {response.status_code}")
                break
                
            data = response.json()
            docs = data.get('value', [])
            
            if not docs:
                break
                
            for doc in docs:
                if 'program_id' in doc:
                    programs.add(doc['program_id'])
            
            if len(docs) < batch_size:
                break
                
            skip += batch_size
            
        except Exception as e:
            print(f"Error in batch starting at {skip}: {e}")
            break
    
    return programs

def create_zero_screen_record(program_id: str) -> Dict[str, Any]:
    """Create a zero-screen record for a program without screen structures."""
    return {
        '@search.action': 'upload',
        'screen_id': f"{program_id}_NO_SCREENS",
        'program_id': program_id,
        'screen_name': f"{program_id}_NO_SCREEN_STRUCTURE",
        'field_count': 0,
        'action_count': 0,
        'transition_count': 0,
        'fields_json': '[]',
        'actions_json': '[]',
        'transitions_json': '[]',
        'raw_span_text': 'NO_SCREEN_CONTENT',
        'summary_text': f'Zero-screen record for program {program_id}; fields=0; actions=0; transitions=0',
        'generated_at': '2025-10-08T00:00:00Z',
        'has_vector': False
    }

def upload_batch(endpoint: str, key: str, batch: list) -> bool:
    """Upload a batch of documents to the screen nodes index."""
    if not batch:
        return True
        
    url = f"{endpoint}/indexes/new_cobol_screen_nodes/docs/index?api-version=2023-11-01"
    
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
    print("=== BUILDING COMPLETE SCREEN NODES INDEX ===")
    
    endpoint, key = load_config()
    
    # Step 1: Get all programs
    print("\n1. Getting all programs from program_inventory...")
    all_programs = get_all_programs(endpoint, key)
    print(f"Found {len(all_programs)} total programs")
    
    # Step 2: Get programs with existing screen records
    print("\n2. Getting programs with existing screen nodes...")
    existing_screen_programs = get_existing_screen_programs(endpoint, key)
    print(f"Found {len(existing_screen_programs)} programs with screen nodes")
    
    # Step 3: Find programs needing zero-screen records
    programs_needing_zero_screens = all_programs - existing_screen_programs
    print(f"\n3. Programs needing zero-screen records: {len(programs_needing_zero_screens)}")
    
    if not programs_needing_zero_screens:
        print("✅ All programs already have screen node records!")
        return
    
    # Step 4: Create and upload zero-screen records
    print("\n4. Creating zero-screen records...")
    
    batch = []
    batch_size = 100
    total_uploaded = 0
    
    for program_id in sorted(programs_needing_zero_screens):
        zero_record = create_zero_screen_record(program_id)
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
    print(f"Zero-screen records created: {total_uploaded}")
    print(f"Expected total screen node records: {len(all_programs)}")
    print(f"Programs with actual screens: {len(existing_screen_programs)}")
    print(f"Programs with zero-screen records: {total_uploaded}")

if __name__ == '__main__':
    main()