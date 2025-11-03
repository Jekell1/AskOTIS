#!/usr/bin/env python3
"""Accurate source file coverage analysis with proper field detection.

Analyzes coverage by detecting the actual program identifier fields in each index.
"""

import os
import json
import requests
from typing import Dict, List, Tuple, Set

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

def get_cobol_indexes(endpoint: str, key: str) -> List[str]:
    """Get all COBOL index names."""
    url = f"{endpoint}/indexes?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code != 200:
        raise SystemExit(f"Failed to get indexes: {r.status_code}")
    
    data = r.json()
    indexes = [idx['name'] for idx in data.get('value', [])]
    cobol_indexes = [idx for idx in indexes if 'cobol' in idx.lower() and 'new_' in idx]
    return sorted(cobol_indexes)

def get_index_sample(endpoint: str, key: str, index_name: str) -> List[dict]:
    """Get sample documents from an index."""
    url = f"{endpoint}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    body = {
        'search': '*',
        'top': 5
    }
    
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, json=body)
    if r.status_code == 200:
        data = r.json()
        return data.get('value', [])
    return []

def get_doc_count(endpoint: str, key: str, index_name: str) -> int:
    """Get total document count for an index."""
    url = f"{endpoint}/indexes/{index_name}/docs/$count?api-version={API_VERSION}"
    r = requests.get(url, headers={'api-key': key})
    if r.status_code == 200:
        return int(r.text)
    return 0

def detect_program_field(sample_docs: List[dict]) -> str:
    """Detect which field contains program identifiers."""
    if not sample_docs:
        return None
        
    # Common program identifier field names
    program_fields = [
        'program_id', 'program_name', 'source_file', 'file_name', 
        'filename', 'name', 'id', 'program'
    ]
    
    first_doc = sample_docs[0]
    
    # Look for exact matches first
    for field in program_fields:
        if field in first_doc:
            return field
    
    # Look for fields that might contain program names
    for field_name, value in first_doc.items():
        if isinstance(value, str):
            # Check if value looks like a program name (ends with .CBL, .CPY, or is uppercase)
            if (value.upper().endswith('.CBL') or 
                value.upper().endswith('.CPY') or 
                (value.isupper() and len(value) > 2)):
                return field_name
    
    return None

def analyze_index_coverage(endpoint: str, key: str, index_name: str) -> Dict:
    """Analyze coverage for a specific index."""
    result = {
        'index': index_name,
        'total_docs': 0,
        'program_field': None,
        'unique_programs': 0,
        'coverage_pct': 0.0,
        'sample_programs': [],
        'error': None
    }
    
    try:
        # Get basic info
        result['total_docs'] = get_doc_count(endpoint, key, index_name)
        
        # Get sample to detect structure
        sample_docs = get_index_sample(endpoint, key, index_name)
        if not sample_docs:
            result['error'] = 'No documents found'
            return result
        
        # Detect program field
        program_field = detect_program_field(sample_docs)
        result['program_field'] = program_field
        
        if not program_field:
            result['error'] = 'No program identifier field detected'
            return result
        
        # Collect sample program values
        for doc in sample_docs:
            prog_value = doc.get(program_field)
            if prog_value:
                result['sample_programs'].append(str(prog_value))
        
        # Count unique programs (sample approach for performance)
        unique_programs = set()
        skip = 0
        page_size = 1000
        max_docs = 10000  # Limit for performance
        
        while skip < max_docs and skip < result['total_docs']:
            url = f"{endpoint}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
            body = {
                'search': '*',
                'select': program_field,
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
                prog_value = record.get(program_field)
                if prog_value:
                    # Normalize program name (remove .CBL/.CPY extension, convert to uppercase)
                    prog_normalized = str(prog_value).upper()
                    if prog_normalized.endswith('.CBL') or prog_normalized.endswith('.CPY'):
                        prog_normalized = prog_normalized[:-4]
                    unique_programs.add(prog_normalized)
            
            skip += len(records)
            
            if len(records) < page_size:
                break
        
        result['unique_programs'] = len(unique_programs)
        result['coverage_pct'] = (len(unique_programs) / 9678) * 100 if len(unique_programs) > 0 else 0.0
        
        # If we hit the limit, extrapolate
        if skip >= max_docs and result['total_docs'] > max_docs:
            estimated_total = (len(unique_programs) / skip) * result['total_docs']
            result['unique_programs'] = int(estimated_total)
            result['coverage_pct'] = (estimated_total / 9678) * 100
            result['estimated'] = True
        else:
            result['estimated'] = False
            
    except Exception as e:
        result['error'] = str(e)
    
    return result

def main():
    endpoint, key = load_config()
    
    print("=== ACCURATE SOURCE FILE COVERAGE ANALYSIS ===")
    print(f"Expected total programs: 9,678")
    print()
    
    cobol_indexes = get_cobol_indexes(endpoint, key)
    results = []
    
    for index_name in cobol_indexes:
        print(f"Analyzing {index_name}...")
        result = analyze_index_coverage(endpoint, key, index_name)
        results.append(result)
    
    print("\n" + "="*90)
    print("DETAILED COVERAGE ANALYSIS")
    print("="*90)
    
    # Sort by coverage percentage (descending)
    results.sort(key=lambda x: x['coverage_pct'], reverse=True)
    
    full_coverage_indexes = []
    partial_coverage_indexes = []
    no_coverage_indexes = []
    error_indexes = []
    
    for result in results:
        index_name = result['index']
        total_docs = result['total_docs']
        program_field = result['program_field']
        unique_programs = result['unique_programs']
        coverage_pct = result['coverage_pct']
        error = result['error']
        estimated = result.get('estimated', False)
        
        if error:
            status = "✗ ERROR"
            error_indexes.append(index_name)
            field_info = f"Error: {error}"
        elif coverage_pct >= 95.0:
            status = "✓ FULL"
            full_coverage_indexes.append(index_name)
            field_info = f"Field: {program_field}"
        elif unique_programs > 0:
            status = "~ PARTIAL"
            partial_coverage_indexes.append(index_name)
            field_info = f"Field: {program_field}"
        else:
            status = "✗ NONE"
            no_coverage_indexes.append(index_name)
            field_info = f"Field: {program_field or 'Unknown'}"
        
        est_marker = " (est)" if estimated else ""
        
        print(f"{status:9} | {index_name:35} | {total_docs:8,} docs | {unique_programs:5,} programs{est_marker} | {coverage_pct:5.1f}% | {field_info}")
        
        # Show sample programs for partial/no coverage indexes
        if result['sample_programs'] and (status == "~ PARTIAL" or status == "✗ NONE"):
            samples = result['sample_programs'][:3]
            print(f"{'':11} | {'Sample values:':<35} | {', '.join(samples)}")
    
    print("\n" + "="*90)
    print("SUMMARY BY COVERAGE STATUS")
    print("="*90)
    
    print(f"✓ FULL COVERAGE ({len(full_coverage_indexes)} indexes - 95%+ coverage):")
    for idx in full_coverage_indexes:
        result = next(r for r in results if r['index'] == idx)
        print(f"  - {idx:35} | {result['unique_programs']:5,} programs | {result['coverage_pct']:5.1f}%")
    
    if partial_coverage_indexes:
        print(f"\n~ PARTIAL COVERAGE ({len(partial_coverage_indexes)} indexes):")
        for idx in partial_coverage_indexes:
            result = next(r for r in results if r['index'] == idx)
            gap = 9678 - result['unique_programs']
            print(f"  - {idx:35} | {result['unique_programs']:5,} programs | Missing {gap:,}")
    
    if no_coverage_indexes:
        print(f"\n✗ NO PROGRAM COVERAGE ({len(no_coverage_indexes)} indexes):")
        for idx in no_coverage_indexes:
            result = next(r for r in results if r['index'] == idx)
            print(f"  - {idx:35} | {result['total_docs']:8,} docs | Field: {result['program_field'] or 'Unknown'}")
    
    if error_indexes:
        print(f"\n✗ ANALYSIS ERRORS ({len(error_indexes)} indexes):")
        for idx in error_indexes:
            result = next(r for r in results if r['index'] == idx)
            print(f"  - {idx:35} | Error: {result['error']}")
    
    print(f"\nTotal indexes analyzed: {len(results)}")
    print(f"Indexes with full coverage (95%+): {len(full_coverage_indexes)}")
    print(f"Indexes with partial coverage: {len(partial_coverage_indexes)}")
    print(f"Indexes with no coverage: {len(no_coverage_indexes)}")
    print(f"Indexes with analysis errors: {len(error_indexes)}")

if __name__ == '__main__':
    main()