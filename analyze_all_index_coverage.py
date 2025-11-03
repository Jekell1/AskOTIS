#!/usr/bin/env python3
"""Comprehensive source file coverage analysis across all COBOL indexes.

Checks which indexes have full coverage of the 9,678 expected source files.
"""

import os
import json
import requests
from typing import Dict, List, Tuple

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

def get_all_indexes(endpoint: str, key: str) -> List[str]:
    """Get all index names."""
    url = f"{endpoint}/indexes?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise SystemExit(f"Failed to get indexes: {r.status_code}")
    
    data = r.json()
    indexes = [idx['name'] for idx in data.get('value', [])]
    cobol_indexes = [idx for idx in indexes if 'cobol' in idx.lower() and 'new_' in idx]
    return sorted(cobol_indexes)

def get_index_doc_count(endpoint: str, key: str, index_name: str) -> int:
    """Get document count for an index."""
    url = f"{endpoint}/indexes/{index_name}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code == 200:
        return int(r.text)
    return 0

def analyze_program_coverage(endpoint: str, key: str, index_name: str) -> Tuple[int, float]:
    """Analyze how many unique programs are covered in an index."""
    try:
        # Try to get distinct program count using a search with program_id field
        url = f"{endpoint}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
        body = {
            'search': '*',
            'select': 'program_id',
            'top': 1000
        }
        
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
        if r.status_code != 200:
            return 0, 0.0
            
        data = r.json()
        sample_records = data.get('value', [])
        
        if not sample_records:
            return 0, 0.0
            
        # Check if this index has program_id field
        first_record = sample_records[0]
        if 'program_id' not in first_record:
            return 0, 0.0
            
        # Sample approach: collect unique program_ids from multiple pages
        unique_programs = set()
        skip = 0
        page_size = 1000
        max_samples = 10000  # Limit sampling to avoid long runs
        
        while skip < max_samples:
            body = {
                'search': '*',
                'select': 'program_id',
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
                program_id = record.get('program_id')
                if program_id:
                    unique_programs.add(program_id)
            
            skip += len(records)
            
            if len(records) < page_size:
                break
        
        program_count = len(unique_programs)
        coverage_pct = (program_count / 9678) * 100 if program_count > 0 else 0.0
        
        return program_count, coverage_pct
        
    except Exception as e:
        return 0, 0.0

def main():
    endpoint, key = load_config()
    
    print("=== COMPREHENSIVE SOURCE FILE COVERAGE ANALYSIS ===")
    print(f"Expected total programs: 9,678")
    print()
    
    # Get all COBOL indexes
    cobol_indexes = get_all_indexes(endpoint, key)
    
    results = []
    
    for index_name in cobol_indexes:
        print(f"Analyzing {index_name}...")
        
        # Get total document count
        doc_count = get_index_doc_count(endpoint, key, index_name)
        
        # Analyze program coverage
        program_count, coverage_pct = analyze_program_coverage(endpoint, key, index_name)
        
        results.append({
            'index': index_name,
            'total_docs': doc_count,
            'unique_programs': program_count,
            'coverage_pct': coverage_pct,
            'has_full_coverage': coverage_pct >= 99.0 and program_count >= 9500
        })
    
    print("\n" + "="*80)
    print("COVERAGE ANALYSIS RESULTS")
    print("="*80)
    
    # Sort by coverage percentage (descending)
    results.sort(key=lambda x: x['coverage_pct'], reverse=True)
    
    full_coverage_indexes = []
    partial_coverage_indexes = []
    no_program_coverage_indexes = []
    
    for result in results:
        index_name = result['index']
        total_docs = result['total_docs']
        unique_programs = result['unique_programs']
        coverage_pct = result['coverage_pct']
        has_full_coverage = result['has_full_coverage']
        
        status = "✓ FULL" if has_full_coverage else ("~ PARTIAL" if unique_programs > 0 else "✗ NONE")
        
        print(f"{status:8} | {index_name:35} | {total_docs:8,} docs | {unique_programs:5,} programs | {coverage_pct:5.1f}%")
        
        if has_full_coverage:
            full_coverage_indexes.append(index_name)
        elif unique_programs > 0:
            partial_coverage_indexes.append(index_name)
        else:
            no_program_coverage_indexes.append(index_name)
    
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    
    print(f"✓ FULL COVERAGE ({len(full_coverage_indexes)} indexes):")
    for idx in full_coverage_indexes:
        print(f"  - {idx}")
    
    if partial_coverage_indexes:
        print(f"\n~ PARTIAL COVERAGE ({len(partial_coverage_indexes)} indexes):")
        for idx in partial_coverage_indexes:
            result = next(r for r in results if r['index'] == idx)
            gap = 9678 - result['unique_programs']
            print(f"  - {idx} (missing {gap:,} programs)")
    
    if no_program_coverage_indexes:
        print(f"\n✗ NO PROGRAM COVERAGE ({len(no_program_coverage_indexes)} indexes):")
        for idx in no_program_coverage_indexes:
            print(f"  - {idx}")
    
    print(f"\nTotal indexes analyzed: {len(results)}")
    print(f"Indexes with full coverage: {len(full_coverage_indexes)}")
    print(f"Indexes needing attention: {len(partial_coverage_indexes) + len(no_program_coverage_indexes)}")

if __name__ == '__main__':
    main()