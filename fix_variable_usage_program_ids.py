#!/usr/bin/env python3
"""Fix variable usage program_id coverage by enriching from symbol_refs.

Strategy:
1. Get variable usage records with missing program_id
2. Look up corresponding symbol_refs by symbol_name or correlate by symbol_id_global  
3. Update program_id field in variable usage records
"""

import os
import json
import requests
import time
from typing import Dict, Set, List

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

def get_variable_usage_sample(endpoint: str, key: str) -> List[dict]:
    """Get sample variable usage records to understand structure."""
    url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
    body = {
        'search': '*',
        'top': 100
    }
    
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    if r.status_code == 200:
        return r.json().get('value', [])
    return []

def build_symbol_program_map(endpoint: str, key: str) -> Dict[str, Set[str]]:
    """Build map of symbol_name -> set of program_ids from symbol_refs."""
    print("Building symbol -> program mapping from symbol_refs...")
    
    symbol_programs = {}
    skip = 0
    page_size = 2000
    
    while True:
        url = f"{endpoint}/indexes/new_cobol_symbol_refs/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'select': 'symbol_name,program_id',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            print(f"Error fetching symbol_refs: {r.status_code}")
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
            
        for record in records:
            symbol_name = record.get('symbol_name')
            program_id = record.get('program_id')
            
            if symbol_name and program_id:
                if symbol_name not in symbol_programs:
                    symbol_programs[symbol_name] = set()
                symbol_programs[symbol_name].add(program_id)
        
        skip += len(records)
        print(f"  Processed {skip:,} symbol_refs, {len(symbol_programs):,} unique symbols...")
        
        if len(records) < page_size:
            break
    
    print(f"Built mapping for {len(symbol_programs):,} symbols")
    return symbol_programs

def enrich_variable_usage_program_ids(endpoint: str, key: str, symbol_programs: Dict[str, Set[str]]):
    """Enrich variable usage records with program_id from symbol mapping or first_write_program."""
    print("Enriching variable usage records with program_id...")
    
    skip = 0
    page_size = 1000
    total_updated = 0
    
    while True:
        # Get batch of variable usage records
        url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'select': 'symbol_id_global,symbol_name,first_write_program,program_id',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            print(f"Error fetching variable_usage: {r.status_code}")
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
        
        # Build updates for records missing program_id
        updates = []
        
        for record in records:
            symbol_id_global = record.get('symbol_id_global')
            symbol_name = record.get('symbol_name')
            first_write_program = record.get('first_write_program')
            current_program_id = record.get('program_id')
            
            # Skip if already has program_id
            if current_program_id:
                continue
                
            # Try to determine program_id
            inferred_program_id = None
            
            # Option 1: Use first_write_program if available
            if first_write_program:
                inferred_program_id = first_write_program
            
            # Option 2: Look up by symbol_name in symbol_refs
            elif symbol_name and symbol_name in symbol_programs:
                programs = symbol_programs[symbol_name]
                # If multiple programs use this symbol, pick the first one alphabetically for consistency
                inferred_program_id = sorted(programs)[0] if programs else None
            
            if inferred_program_id and symbol_id_global:
                updates.append({
                    'symbol_id_global': symbol_id_global,
                    'program_id': inferred_program_id,
                    '@search.action': 'merge'
                })
        
        # Upload updates
        if updates:
            upload_url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/index?api-version={API_VERSION}"
            payload = {'value': updates}
            
            r = requests.post(upload_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload)
            if r.status_code in (200, 201):
                total_updated += len(updates)
                print(f"  Updated {len(updates)} records (total: {total_updated:,})")
            else:
                print(f"  Upload failed: {r.status_code} - {r.text[:200]}")
        
        skip += len(records)
        print(f"  Processed {skip:,} variable usage records...")
        
        if len(records) < page_size:
            break
    
    print(f"Total records updated with program_id: {total_updated:,}")

def main():
    endpoint, key = load_config()
    
    print("=== FIXING VARIABLE USAGE PROGRAM_ID COVERAGE ===")
    
    # First understand the current structure
    print("Analyzing current variable usage structure...")
    sample_records = get_variable_usage_sample(endpoint, key)
    
    if sample_records:
        print(f"Sample record fields: {list(sample_records[0].keys())}")
        
        # Count how many have program_id
        with_program_id = sum(1 for r in sample_records if r.get('program_id'))
        print(f"Sample records with program_id: {with_program_id}/{len(sample_records)}")
    
    # Build symbol -> program mapping
    symbol_programs = build_symbol_program_map(endpoint, key)
    
    # Enrich variable usage records
    enrich_variable_usage_program_ids(endpoint, key, symbol_programs)
    
    print("✓ Variable usage program_id enrichment complete!")

if __name__ == '__main__':
    main()