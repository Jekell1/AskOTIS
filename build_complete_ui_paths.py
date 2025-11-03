#!/usr/bin/env python3
"""
Build complete UI paths index with 100% program coverage.
Creates zero-path records for programs without UI path structures.
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

def get_existing_ui_path_programs(endpoint: str, key: str) -> Set[str]:
    """Get programs that already appear in UI path records."""
    programs = set()
    
    url = f"{endpoint}/indexes/new_cobol_ui_paths/docs/search?api-version=2023-11-01"
    
    skip = 0
    batch_size = 1000
    
    while True:
        body = {
            'search': '*',
            'select': 'start_program_id,end_program_id,root_program_id,leaf_program_id',
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
                print(f"Error fetching existing UI path programs: {response.status_code}")
                break
                
            data = response.json()
            docs = data.get('value', [])
            
            if not docs:
                break
                
            for doc in docs:
                # UI paths can reference programs in multiple fields
                for field in ['start_program_id', 'end_program_id', 'root_program_id', 'leaf_program_id']:
                    if field in doc and doc[field]:
                        programs.add(doc[field])
            
            if len(docs) < batch_size:
                break
                
            skip += batch_size
            
        except Exception as e:
            print(f"Error in batch starting at {skip}: {e}")
            break
    
    return programs

def create_zero_ui_path_record(program_id: str) -> Dict[str, Any]:
    """Create a zero-UI path record for a program without UI paths."""
    return {
        '@search.action': 'upload',
        'path_id': f"{program_id}_NO_UI_PATHS",
        'start_program_id': program_id,
        'end_program_id': program_id,
        'path_json': f'{{"program_sequence": ["{program_id}"], "screen_ids": [], "guards": [], "length": 1, "ui_program_count": 0}}',
        'guard_summary': None,
        'frequency_score': 0.0,
        'frequency_score_norm': None,
        'hop_count': None,
        'updated_at': '2025-10-08T00:00:00Z',
        'root_program_id': program_id,
        'leaf_program_id': program_id,
        'program_sequence_json': f'["{program_id}"]',
        'screen_sequence_json': f'["{program_id}"]',
        'screen_names_json': None,
        'length': 1,
        'ui_program_count': 0,
        'branching_events_json': '[]',
        'guards_json': '[]',
        'edge_freqs_json': '[]',
        'avg_edge_freq': 0.0,
        'min_edge_freq': 0,
        'screen_ids_json': '[]',
        'has_vector': False,
        'loop_collapsed': False,
        'score': 0.0,
        'generated_at': '2025-10-08T00:00:00Z',
        'notes': None,
        'is_placeholder': None,
        'path_type': None,
        'generation_pass': None,
        'edge_origins_json': None,
        'transition_edge_count': None,
        'call_edge_count': None,
        'deepening_pass': None
    }

def upload_batch(endpoint: str, key: str, batch: list) -> bool:
    """Upload a batch of documents to the UI paths index."""
    if not batch:
        return True
        
    url = f"{endpoint}/indexes/new_cobol_ui_paths/docs/index?api-version=2023-11-01"
    
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
    print("=== BUILDING COMPLETE UI PATHS INDEX ===")
    
    endpoint, key = load_config()
    
    # Step 1: Get all programs
    print("\n1. Getting all programs from program_inventory...")
    all_programs = get_all_programs(endpoint, key)
    print(f"Found {len(all_programs)} total programs")
    
    # Step 2: Get programs with existing UI path records
    print("\n2. Getting programs with existing UI paths...")
    existing_ui_path_programs = get_existing_ui_path_programs(endpoint, key)
    print(f"Found {len(existing_ui_path_programs)} programs with UI paths")
    
    # Step 3: Find programs needing zero-UI path records
    programs_needing_zero_ui_paths = all_programs - existing_ui_path_programs
    print(f"\n3. Programs needing zero-UI path records: {len(programs_needing_zero_ui_paths)}")
    
    if not programs_needing_zero_ui_paths:
        print("✅ All programs already have UI path records!")
        return
    
    # Step 4: Create and upload zero-UI path records
    print("\n4. Creating zero-UI path records...")
    
    batch = []
    batch_size = 100
    total_uploaded = 0
    
    for program_id in sorted(programs_needing_zero_ui_paths):
        zero_record = create_zero_ui_path_record(program_id)
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
    print(f"Zero-UI path records created: {total_uploaded}")
    print(f"Expected total UI path records: {len(all_programs)}")
    print(f"Programs with actual UI paths: {len(existing_ui_path_programs)}")
    print(f"Programs with zero-UI path records: {total_uploaded}")

if __name__ == '__main__':
    main()