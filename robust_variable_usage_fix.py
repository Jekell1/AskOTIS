#!/usr/bin/env python3
"""Robust variable usage program_id enrichment with proper error handling.

Continues from where the previous attempt left off and handles Azure Search pagination limits.
"""

import os
import json
import requests
import time
from typing import Dict, Set, List

API_VERSION = '2023-11-01'
MAX_SKIP = 100000  # Azure Search limit

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

def get_completion_status(endpoint: str, key: str):
    """Check how many records already have program_id vs total."""
    print("=== Checking completion status ===")
    
    # Get total count
    url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    total_count = int(r.text) if r.status_code == 200 else 0
    
    # Count records with program_id using filter
    search_url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
    body = {
        'search': '*',
        'filter': 'program_id ne null',
        'select': 'symbol_id_global',
        'top': 0
    }
    
    r = requests.post(search_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    with_program_id = 0
    if r.status_code == 200:
        data = r.json()
        with_program_id = data.get('@odata.count', 0)
    
    print(f"Total records: {total_count:,}")
    print(f"Records with program_id: {with_program_id:,}")
    print(f"Records missing program_id: {total_count - with_program_id:,}")
    print(f"Completion: {(with_program_id / total_count * 100):.1f}%")
    
    return total_count, with_program_id

def enrich_from_first_write_program(endpoint: str, key: str):
    """Enrich records using first_write_program field."""
    print("\n=== Enriching from first_write_program ===")
    
    updated_count = 0
    skip = 0
    page_size = 1000
    
    while skip < MAX_SKIP:
        # Find records with first_write_program but no program_id
        url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'filter': 'first_write_program ne null and program_id eq null',
            'select': 'symbol_id_global,first_write_program',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            print(f"Error fetching records: {r.status_code}")
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            print(f"No more records found at skip={skip}")
            break
        
        # Build updates
        updates = []
        for record in records:
            symbol_id = record.get('symbol_id_global')
            first_write_program = record.get('first_write_program')
            
            if symbol_id and first_write_program:
                updates.append({
                    'symbol_id_global': symbol_id,
                    'program_id': first_write_program,
                    '@search.action': 'merge'
                })
        
        # Upload updates
        if updates:
            upload_url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/index?api-version={API_VERSION}"
            payload = {'value': updates}
            
            r = requests.post(upload_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload)
            if r.status_code in (200, 201):
                updated_count += len(updates)
                print(f"  Updated {len(updates)} records (total: {updated_count:,}) from first_write_program")
            else:
                print(f"  Upload failed: {r.status_code} - {r.text[:200]}")
        
        skip += len(records)
        
        if len(records) < page_size:
            break
    
    print(f"Completed first_write_program enrichment: {updated_count:,} records")
    return updated_count

def build_reduced_symbol_map(endpoint: str, key: str) -> Dict[str, str]:
    """Build a focused symbol->program map for missing records only."""
    print("\n=== Building symbol map for missing records ===")
    
    # Get symbol names that still need program_id
    missing_symbols = set()
    skip = 0
    page_size = 2000
    
    print("Finding symbols that still need program_id...")
    while skip < MAX_SKIP:
        url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'filter': 'program_id eq null and symbol_name ne null',
            'select': 'symbol_name',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
            
        for record in records:
            symbol_name = record.get('symbol_name')
            if symbol_name:
                missing_symbols.add(symbol_name)
        
        skip += len(records)
        print(f"  Found {len(missing_symbols):,} unique symbols needing program_id...")
        
        if len(records) < page_size:
            break
    
    print(f"Need to resolve {len(missing_symbols):,} symbols")
    
    if not missing_symbols:
        return {}
    
    # Now build symbol->program map only for these symbols
    symbol_programs = {}
    skip = 0
    page_size = 2000
    
    print("Building focused symbol->program map...")
    while skip < MAX_SKIP:
        url = f"{endpoint}/indexes/new_cobol_symbol_refs/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'select': 'symbol_name,program_id',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
            
        for record in records:
            symbol_name = record.get('symbol_name')
            program_id = record.get('program_id')
            
            if symbol_name in missing_symbols and program_id:
                # Take first program for each symbol for consistency
                if symbol_name not in symbol_programs:
                    symbol_programs[symbol_name] = program_id
        
        skip += len(records)
        
        if len(records) < page_size or skip >= MAX_SKIP:
            break
    
    print(f"Resolved {len(symbol_programs):,} symbols to programs")
    return symbol_programs

def enrich_from_symbol_refs(endpoint: str, key: str, symbol_programs: Dict[str, str]):
    """Enrich remaining records using symbol_refs lookup."""
    print("\n=== Enriching from symbol_refs ===")
    
    if not symbol_programs:
        print("No symbol mappings available")
        return 0
    
    updated_count = 0
    skip = 0
    page_size = 1000
    
    while skip < MAX_SKIP:
        # Find records with symbol_name but no program_id
        url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'filter': 'program_id eq null and symbol_name ne null',
            'select': 'symbol_id_global,symbol_name',
            'skip': skip,
            'top': page_size
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            print(f"Error fetching records: {r.status_code}")
            break
            
        data = r.json()
        records = data.get('value', [])
        
        if not records:
            break
        
        # Build updates
        updates = []
        for record in records:
            symbol_id = record.get('symbol_id_global')
            symbol_name = record.get('symbol_name')
            
            if symbol_id and symbol_name and symbol_name in symbol_programs:
                program_id = symbol_programs[symbol_name]
                updates.append({
                    'symbol_id_global': symbol_id,
                    'program_id': program_id,
                    '@search.action': 'merge'
                })
        
        # Upload updates
        if updates:
            upload_url = f"{endpoint}/indexes/new_cobol_variable_usage/docs/index?api-version={API_VERSION}"
            payload = {'value': updates}
            
            r = requests.post(upload_url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=payload)
            if r.status_code in (200, 201):
                updated_count += len(updates)
                print(f"  Updated {len(updates)} records (total: {updated_count:,}) from symbol_refs")
            else:
                print(f"  Upload failed: {r.status_code} - {r.text[:200]}")
        
        skip += len(records)
        
        if len(records) < page_size:
            break
    
    print(f"Completed symbol_refs enrichment: {updated_count:,} records")
    return updated_count

def main():
    endpoint, key = load_config()
    
    print("=== ROBUST VARIABLE USAGE PROGRAM_ID ENRICHMENT ===")
    
    # Check current status
    total_count, with_program_id = get_completion_status(endpoint, key)
    
    if with_program_id >= total_count * 0.95:
        print("✓ Already 95%+ complete!")
        return
    
    # Strategy 1: Enrich from first_write_program (fastest)
    updated_1 = enrich_from_first_write_program(endpoint, key)
    
    # Check progress
    total_count, with_program_id = get_completion_status(endpoint, key)
    
    if with_program_id >= total_count * 0.95:
        print("✓ Reached 95%+ completion!")
        return
    
    # Strategy 2: Build focused symbol map and enrich
    symbol_programs = build_reduced_symbol_map(endpoint, key)
    updated_2 = enrich_from_symbol_refs(endpoint, key, symbol_programs)
    
    # Final status
    total_count, with_program_id = get_completion_status(endpoint, key)
    
    total_updated = updated_1 + updated_2
    print(f"\n=== COMPLETION SUMMARY ===")
    print(f"Total records updated: {total_updated:,}")
    print(f"Final completion: {(with_program_id / total_count * 100):.1f}%")
    print("✓ Variable usage program_id enrichment complete!")

if __name__ == '__main__':
    main()