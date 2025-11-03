#!/usr/bin/env python3
"""
Analyze why data_items is at 17.3% coverage.
Sample "missing" programs to determine if structural limit or extraction issue.
"""
import json
import random
import os
import requests
from pathlib import Path

def load_azure_config():
    """Load Azure Search credentials from local.settings.json"""
    with open('local.settings.json', 'r', encoding='utf-8') as f:
        settings = json.load(f)
    
    values = settings.get('Values', {})
    endpoint = (values.get('AZURE_SEARCH_ENDPOINT') or values.get('SEARCH_ENDPOINT')).rstrip('/')
    key = values.get('AZURE_SEARCH_KEY') or values.get('SEARCH_KEY')
    
    return endpoint, key

def get_programs_with_data_items(endpoint, key):
    """Get list of programs that have data_items"""
    url = f"{endpoint}/indexes/new_cobol_data_items/docs/search?api-version=2025-08-01-preview"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    programs = set()
    payload = {
        'search': '*',
        'select': 'program_id',
        'top': 1000,
        'skip': 0
    }
    
    print("Fetching programs with data_items... ", end='', flush=True)
    
    while True:
        r = requests.post(url, headers=headers, json=payload)
        results = r.json().get('value', [])
        
        if not results:
            break
        
        for doc in results:
            programs.add(doc['program_id'])
        
        payload['skip'] += 1000
        print(f"{len(programs)}... ", end='', flush=True)
        
        if len(results) < 1000:
            break
    
    print(f"done! Found {len(programs)} programs")
    return programs

def get_all_programs(endpoint, key):
    """Get list of all programs from program_meta"""
    url = f"{endpoint}/indexes/new_cobol_program_meta/docs/search?api-version=2025-08-01-preview"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    programs = []
    payload = {
        'search': '*',
        'select': 'program_id',
        'top': 1000
    }
    
    print("Fetching all programs... ", end='', flush=True)
    r = requests.post(url, headers=headers, json=payload)
    programs = [doc['program_id'] for doc in r.json().get('value', [])]
    print(f"done! Found {len(programs)} programs")
    return programs

def analyze_data_items_gap():
    print("DATA ITEMS GAP ANALYSIS")
    print("=" * 70)
    
    # Load Azure config
    endpoint, key = load_azure_config()
    
    # Load coverage data for display
    with open('comprehensive_partial_coverage_analysis.json') as f:
        data = json.load(f)
    
    # Find data_items stats
    data_items_stats = None
    for r in data['results']:
        if 'data_items' in r['index']:
            data_items_stats = r
            break
    
    if not data_items_stats:
        print("ERROR: data_items not found in coverage analysis")
        return
    
    print(f"Total documents: {data_items_stats['total_docs']:,}")
    print(f"Programs with data: {data_items_stats['programs_with_data']:,} (17.3%)")
    print(f"Programs missing: {data_items_stats['programs_missing']:,} (82.7%)")
    print(f"Avg items/program: {data_items_stats['total_docs'] // data_items_stats['programs_with_data']:.1f}")
    print()
    
    # Get missing programs dynamically
    print("FETCHING MISSING PROGRAMS:")
    print("-" * 70)
    
    programs_with_data = get_programs_with_data_items(endpoint, key)
    all_programs = get_all_programs(endpoint, key)
    missing_programs = [p for p in all_programs if p not in programs_with_data]
    
    print(f"Missing programs: {len(missing_programs)}")
    print()
    
    # Sample missing programs
    print("SAMPLING 'MISSING' PROGRAMS:")
    print("-" * 70)
    
    # Sample 10 random missing programs
    sample = random.sample(missing_programs, min(10, len(missing_programs)))
    
    cobol_root = Path('cobol_src')
    found_count = 0
    not_found_count = 0
    
    for prog_id in sample:
        # Try to find the file
        possible_paths = list(cobol_root.rglob(f"{prog_id}.*"))
        
        if not possible_paths:
            print(f"✗ {prog_id} - FILE NOT FOUND")
            not_found_count += 1
            continue
        
        file_path = possible_paths[0]
        found_count += 1
        
        # Quick check: does it have DATA DIVISION?
        try:
            with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
                content = f.read().upper()
                
            has_data_div = 'DATA DIVISION' in content or 'DATA-DIVISION' in content
            has_level_items = any(f'\n       {level:02d} ' in content for level in range(1, 50))
            
            print(f"{'✓' if has_data_div else '✗'} {prog_id}")
            print(f"    DATA DIVISION: {'YES' if has_data_div else 'NO'}")
            print(f"    Level items: {'YES' if has_level_items else 'NO'}")
            print(f"    File: {file_path}")
            print()
            
        except Exception as e:
            print(f"✗ {prog_id} - ERROR: {e}")
            not_found_count += 1
    
    print()
    print("SUMMARY:")
    print("-" * 70)
    print(f"Sample size: {len(sample)}")
    print(f"Files found: {found_count}")
    print(f"Files not found: {not_found_count}")
    print()
    
    print("HYPOTHESIS:")
    print("-" * 70)
    print("If most sampled files have NO DATA DIVISION or NO level items,")
    print("then 17.3% is likely the STRUCTURAL MAXIMUM (like paragraphs at 41.1%)")
    print()
    print("If most sampled files HAVE DATA DIVISION and level items,")
    print("then there's an extraction or ID collision issue to investigate.")

if __name__ == '__main__':
    analyze_data_items_gap()
