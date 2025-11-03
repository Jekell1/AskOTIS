#!/usr/bin/env python3
"""
Validate which indexes are at structural limits vs genuinely expandable.
Sample "missing" programs for each low-coverage index to determine if they
actually contain the expected content type.
"""
import json
import random
import os
import requests
from pathlib import Path
from collections import defaultdict

def load_azure_config():
    """Load Azure Search credentials from local.settings.json"""
    with open('local.settings.json', 'r', encoding='utf-8') as f:
        settings = json.load(f)
    
    values = settings.get('Values', {})
    endpoint = (values.get('AZURE_SEARCH_ENDPOINT') or values.get('SEARCH_ENDPOINT')).rstrip('/')
    key = values.get('AZURE_SEARCH_KEY') or values.get('SEARCH_KEY')
    
    return endpoint, key

def get_programs_in_index(endpoint, key, index_name, limit=2000):
    """Get unique program_ids from an index"""
    url = f"{endpoint}/indexes/{index_name}/docs/search?api-version=2025-08-01-preview"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    programs = set()
    payload = {
        'search': '*',
        'select': 'program_id',
        'top': 1000,
        'skip': 0
    }
    
    while True:
        try:
            r = requests.post(url, headers=headers, json=payload)
            results = r.json().get('value', [])
            
            if not results:
                break
            
            for doc in results:
                programs.add(doc.get('program_id', ''))
            
            payload['skip'] += 1000
            
            if len(results) < 1000 or len(programs) >= limit:
                break
        except Exception as e:
            print(f"    Error fetching from {index_name}: {e}")
            break
    
    return programs

def get_all_programs(endpoint, key):
    """Get list of all programs from program_meta"""
    url = f"{endpoint}/indexes/new_cobol_program_meta/docs/search?api-version=2025-08-01-preview"
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    
    payload = {
        'search': '*',
        'select': 'program_id',
        'top': 1000
    }
    
    r = requests.post(url, headers=headers, json=payload)
    return [doc['program_id'] for doc in r.json().get('value', [])]

def check_file_for_content(file_path, content_checks):
    """
    Check if a file contains expected content patterns.
    
    content_checks: dict with keys like 'data_division', 'procedure_division', 
                    'screen_section', 'copy_statements', etc.
    """
    try:
        with open(file_path, 'r', encoding='latin-1', errors='ignore') as f:
            content = f.read().upper()
        
        results = {}
        
        # Check for various COBOL sections/patterns
        if 'data_division' in content_checks:
            results['has_data_division'] = 'DATA DIVISION' in content or 'DATA-DIVISION' in content
            results['has_level_items'] = any(f'\n       {level:02d} ' in content for level in range(1, 50))
        
        if 'procedure_division' in content_checks:
            results['has_procedure_division'] = 'PROCEDURE DIVISION' in content or 'PROCEDURE-DIVISION' in content
            results['has_paragraphs'] = bool([line for line in content.split('\n') 
                                             if line.strip() and not line.strip().startswith('*') 
                                             and '.' in line and 'SECTION' not in line])
        
        if 'screen_section' in content_checks:
            results['has_screen_section'] = 'SCREEN SECTION' in content
            results['has_bms'] = 'DFHMSD' in content or 'DFHMDI' in content or 'DFHMDF' in content
        
        if 'variable_usage' in content_checks:
            # Check for MOVE, COMPUTE, ADD, SUBTRACT, etc.
            results['has_variable_ops'] = any(op in content for op in 
                                             ['MOVE ', 'COMPUTE ', 'ADD ', 'SUBTRACT ', 'MULTIPLY ', 'DIVIDE '])
        
        if 'flow_edges' in content_checks:
            # Check for PERFORM, CALL, GO TO, etc.
            results['has_flow_ops'] = any(op in content for op in 
                                         ['PERFORM ', 'CALL ', 'GO TO ', 'GOTO ', 'EVALUATE '])
        
        if 'copybook_usage' in content_checks:
            results['has_copy_statements'] = 'COPY ' in content
        
        results['is_copybook'] = file_path.suffix.upper() in ['.CPY', '.COPY']
        results['is_program'] = file_path.suffix.upper() in ['.CBL', '.COB', '.COBOL']
        
        return results
    
    except Exception as e:
        return {'error': str(e)}

def analyze_index_structural_limit(index_name, coverage_pct, endpoint, key, cobol_root):
    """
    Analyze if an index is at structural limit or genuinely expandable.
    Returns: (is_structural_limit, evidence_dict)
    """
    
    # Define what content to check for each index
    content_checks_map = {
        'data_items': {'data_division': True},
        'paragraphs': {'procedure_division': True},
        'screen_nodes': {'screen_section': True},
        'variable_usage': {'variable_usage': True},
        'flow_edges': {'flow_edges': True},
        'flow_edges_v2': {'flow_edges': True},
        'copybook_usage': {'copybook_usage': True},
    }
    
    # Find matching content check
    content_checks = None
    for key_pattern, checks in content_checks_map.items():
        if key_pattern in index_name:
            content_checks = checks
            break
    
    if not content_checks:
        return None, {'reason': 'No content check pattern defined'}
    
    print(f"\n{'='*70}")
    print(f"ANALYZING: {index_name} ({coverage_pct:.1f}% coverage)")
    print(f"{'='*70}")
    
    # Get programs in this index
    print(f"  Fetching programs in {index_name}... ", end='', flush=True)
    programs_in_index = get_programs_in_index(endpoint, key, index_name, limit=2000)
    print(f"found {len(programs_in_index)}")
    
    # Get all programs
    print(f"  Fetching all programs... ", end='', flush=True)
    all_programs = get_all_programs(endpoint, key)
    print(f"found {len(all_programs)}")
    
    # Find missing programs
    missing_programs = [p for p in all_programs if p not in programs_in_index]
    print(f"  Missing programs: {len(missing_programs)}")
    
    if not missing_programs:
        return True, {'reason': '100% coverage - complete'}
    
    # Sample up to 15 missing programs
    sample_size = min(15, len(missing_programs))
    sample = random.sample(missing_programs, sample_size)
    
    print(f"\n  Sampling {sample_size} 'missing' programs:")
    print(f"  {'-'*66}")
    
    results = {
        'has_expected_content': 0,
        'missing_expected_content': 0,
        'copybooks': 0,
        'programs': 0,
        'not_found': 0,
        'details': []
    }
    
    for prog_id in sample:
        # Try to find the file
        possible_paths = list(cobol_root.rglob(f"{prog_id}.*"))
        
        if not possible_paths:
            results['not_found'] += 1
            results['details'].append({'program': prog_id, 'status': 'NOT_FOUND'})
            continue
        
        file_path = possible_paths[0]
        check_results = check_file_for_content(file_path, content_checks)
        
        # Determine if file has expected content
        has_content = False
        status_parts = []
        
        if 'data_division' in content_checks:
            has_content = check_results.get('has_data_division', False) and check_results.get('has_level_items', False)
            status_parts.append(f"DATA DIV: {'YES' if check_results.get('has_data_division') else 'NO'}")
            status_parts.append(f"Level items: {'YES' if check_results.get('has_level_items') else 'NO'}")
        
        if 'procedure_division' in content_checks:
            has_content = check_results.get('has_procedure_division', False)
            status_parts.append(f"PROC DIV: {'YES' if check_results.get('has_procedure_division') else 'NO'}")
        
        if 'screen_section' in content_checks:
            has_content = check_results.get('has_screen_section', False) or check_results.get('has_bms', False)
            status_parts.append(f"SCREEN/BMS: {'YES' if has_content else 'NO'}")
        
        if 'variable_usage' in content_checks:
            has_content = check_results.get('has_variable_ops', False)
            status_parts.append(f"VAR OPS: {'YES' if has_content else 'NO'}")
        
        if 'flow_edges' in content_checks:
            has_content = check_results.get('has_flow_ops', False)
            status_parts.append(f"FLOW OPS: {'YES' if has_content else 'NO'}")
        
        if 'copybook_usage' in content_checks:
            has_content = check_results.get('has_copy_statements', False)
            status_parts.append(f"COPY: {'YES' if has_content else 'NO'}")
        
        # Track file types
        if check_results.get('is_copybook'):
            results['copybooks'] += 1
            status_parts.append('[COPYBOOK]')
        elif check_results.get('is_program'):
            results['programs'] += 1
            status_parts.append('[PROGRAM]')
        
        # Update counts
        if has_content:
            results['has_expected_content'] += 1
            marker = '✓'
        else:
            results['missing_expected_content'] += 1
            marker = '✗'
        
        print(f"  {marker} {prog_id:20s} {' | '.join(status_parts)}")
        
        results['details'].append({
            'program': prog_id,
            'has_content': has_content,
            'checks': check_results
        })
    
    # Calculate percentages
    total_checked = results['has_expected_content'] + results['missing_expected_content']
    if total_checked > 0:
        pct_missing = (results['missing_expected_content'] / total_checked) * 100
    else:
        pct_missing = 0
    
    print(f"\n  SUMMARY:")
    print(f"  {'-'*66}")
    print(f"  Sample size: {sample_size}")
    print(f"  Has expected content: {results['has_expected_content']} ({100-pct_missing:.1f}%)")
    print(f"  Missing expected content: {results['missing_expected_content']} ({pct_missing:.1f}%)")
    print(f"  Copybooks: {results['copybooks']}")
    print(f"  Programs: {results['programs']}")
    print(f"  Not found: {results['not_found']}")
    
    # Determine if structural limit
    is_structural_limit = pct_missing >= 70  # If 70%+ missing expected content, it's structural
    
    print(f"\n  VERDICT: ", end='')
    if is_structural_limit:
        print(f"✅ STRUCTURAL LIMIT at {coverage_pct:.1f}%")
    else:
        print(f"⚠️  POTENTIALLY EXPANDABLE from {coverage_pct:.1f}%")
    
    results['is_structural_limit'] = is_structural_limit
    results['missing_content_pct'] = pct_missing
    results['coverage_pct'] = coverage_pct
    
    return is_structural_limit, results

def main():
    print("STRUCTURAL LIMIT VALIDATION ANALYSIS")
    print("="*70)
    print("Checking which low-coverage indexes are at structural limits")
    print("vs genuinely expandable by sampling 'missing' programs.\n")
    
    # Load coverage data
    with open('comprehensive_partial_coverage_analysis.json') as f:
        coverage_data = json.load(f)
    
    # Load Azure config
    endpoint, key = load_azure_config()
    cobol_root = Path('cobol_src')
    
    # Find indexes with low coverage (< 50%)
    low_coverage_indexes = []
    for result in coverage_data['results']:
        index_name = result['index']
        coverage_pct = result['coverage_pct']
        
        # Skip 100% coverage indexes
        if coverage_pct >= 99:
            continue
        
        # Focus on indexes we care about
        if any(pattern in index_name for pattern in 
               ['data_items', 'paragraphs', 'screen_nodes', 'variable_usage', 
                'flow_edges', 'copybook_usage']):
            low_coverage_indexes.append((index_name, coverage_pct))
    
    # Sort by coverage percentage
    low_coverage_indexes.sort(key=lambda x: x[1])
    
    print(f"Found {len(low_coverage_indexes)} indexes to analyze:\n")
    for idx, (name, pct) in enumerate(low_coverage_indexes, 1):
        print(f"  {idx}. {name:35s} {pct:5.1f}%")
    
    # Analyze each index
    summary = {
        'structural_limits': [],
        'potentially_expandable': [],
        'details': {}
    }
    
    for index_name, coverage_pct in low_coverage_indexes:
        is_limit, evidence = analyze_index_structural_limit(
            index_name, coverage_pct, endpoint, key, cobol_root
        )
        
        if is_limit is None:
            continue
        
        summary['details'][index_name] = evidence
        
        if is_limit:
            summary['structural_limits'].append((index_name, coverage_pct))
        else:
            summary['potentially_expandable'].append((index_name, coverage_pct))
    
    # Print final summary
    print(f"\n\n{'='*70}")
    print("FINAL SUMMARY")
    print(f"{'='*70}\n")
    
    print(f"✅ STRUCTURAL LIMITS (Accept current coverage as maximum):")
    print(f"   {'-'*66}")
    if summary['structural_limits']:
        for name, pct in summary['structural_limits']:
            details = summary['details'][name]
            missing_pct = details.get('missing_content_pct', 0)
            print(f"   {name:35s} {pct:5.1f}% ({missing_pct:.0f}% of samples lack content)")
    else:
        print("   (None found)")
    
    print(f"\n⚠️  POTENTIALLY EXPANDABLE (Worth investigating further):")
    print(f"   {'-'*66}")
    if summary['potentially_expandable']:
        for name, pct in summary['potentially_expandable']:
            details = summary['details'][name]
            has_content = details.get('has_expected_content', 0)
            total = has_content + details.get('missing_expected_content', 0)
            print(f"   {name:35s} {pct:5.1f}% ({has_content}/{total} samples have content)")
    else:
        print("   (None found)")
    
    # Save results
    output_file = 'structural_limits_validation.json'
    with open(output_file, 'w') as f:
        json.dump(summary, f, indent=2)
    
    print(f"\n\nDetailed results saved to: {output_file}")
    print("\nNext steps:")
    print("  1. Accept structural limits - update documentation")
    print("  2. Investigate expandable indexes - check extraction scripts")
    print("  3. Focus on truly incomplete indexes (symbol_refs 4.8%)\n")

if __name__ == '__main__':
    main()
