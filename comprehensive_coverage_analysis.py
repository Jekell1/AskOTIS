"""Comprehensive analysis of program coverage and vector coverage issues.

Identifies:
1. Indexes with incomplete vector coverage (< 99%)
2. Program indexes with insufficient program coverage (< 95% of expected 9,678)
3. Empty or low-document indexes that might need attention
"""

import os
import sys
import json
import requests
from typing import Dict, List

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
    
    ep = first('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT')
    key = first('AZURE_SEARCH_KEY', 'SEARCH_KEY')
    
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    
    return ep.rstrip('/'), key

def count_docs(ep: str, key: str, index: str, filter_expr: str = None) -> int:
    """Count documents in an index with optional filter."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    search_url = f"{ep}/indexes/{index}/docs/search?api-version={API_VERSION}"
    
    body = {'search': '*', 'top': 0, 'count': True}
    if filter_expr:
        body['filter'] = filter_expr
    
    try:
        r = requests.post(search_url, headers=headers, json=body, timeout=15)
        if r.status_code == 200:
            return r.json().get('@odata.count', 0)
    except Exception:
        pass
    return 0

def get_index_list(ep: str, key: str) -> List[str]:
    """Get list of all indexes."""
    headers = {'api-key': key}
    try:
        r = requests.get(f"{ep}/indexes?api-version={API_VERSION}", headers=headers, timeout=15)
        if r.status_code == 200:
            return [idx['name'] for idx in r.json().get('value', [])]
    except Exception:
        pass
    return []

def analyze_index(ep: str, key: str, index_name: str, expected_programs: int = 9678) -> Dict:
    """Analyze an index for various coverage metrics."""
    result = {
        'name': index_name,
        'total_docs': 0,
        'vector_docs': 0,
        'vector_percentage': 0.0,
        'is_program_index': False,
        'program_coverage_percentage': 0.0,
        'issues': [],
        'status': 'unknown'
    }
    
    # Get total document count
    result['total_docs'] = count_docs(ep, key, index_name)
    
    if result['total_docs'] == 0:
        result['status'] = 'empty'
        result['issues'].append("Empty index")
        return result
    
    # Check vector coverage (has_vector field)
    result['vector_docs'] = count_docs(ep, key, index_name, 'has_vector eq true')
    if result['vector_docs'] > 0:
        result['vector_percentage'] = (result['vector_docs'] / result['total_docs']) * 100
    
    # Determine if this is a program-related index
    program_indicators = ['program', 'flow', 'call', 'dep']
    result['is_program_index'] = any(indicator in index_name.lower() for indicator in program_indicators)
    
    if result['is_program_index']:
        result['program_coverage_percentage'] = (result['total_docs'] / expected_programs) * 100
    
    # Identify issues
    if result['vector_docs'] == 0:
        result['issues'].append("No vector embeddings")
        result['status'] = 'no_vectors'
    elif result['vector_percentage'] < 99:
        result['issues'].append(f"Incomplete vectors: {result['vector_percentage']:.1f}%")
        result['status'] = 'partial_vectors'
    else:
        result['status'] = 'good_vectors'
    
    if result['is_program_index'] and result['program_coverage_percentage'] < 95:
        result['issues'].append(f"Low program coverage: {result['program_coverage_percentage']:.1f}%")
        if result['status'] == 'good_vectors':
            result['status'] = 'low_coverage'
    
    if not result['issues']:
        result['status'] = 'healthy'
    
    return result

def main():
    ep, key = load_config()
    indexes = get_index_list(ep, key)
    
    if not indexes:
        print("❌ Failed to retrieve index list")
        return
    
    print("🔍 COMPREHENSIVE COVERAGE & VECTOR ANALYSIS")
    print("=" * 80)
    print()
    
    # Analyze all indexes
    results = []
    for idx in sorted(indexes):
        result = analyze_index(ep, key, idx)
        results.append(result)
    
    # Categorize results
    healthy = [r for r in results if r['status'] == 'healthy']
    no_vectors = [r for r in results if r['status'] == 'no_vectors']
    partial_vectors = [r for r in results if r['status'] == 'partial_vectors']
    low_coverage = [r for r in results if r['status'] == 'low_coverage']
    empty = [r for r in results if r['status'] == 'empty']
    
    print("✅ HEALTHY INDEXES:")
    print("-" * 50)
    for r in healthy:
        print(f"   {r['name']:<35} {r['total_docs']:>8,} docs, {r['vector_percentage']:>5.1f}% vectors")
    
    if no_vectors:
        print(f"\n❌ INDEXES WITHOUT VECTORS ({len(no_vectors)}):")
        print("-" * 50)
        for r in no_vectors:
            program_note = f" [Program index: {r['program_coverage_percentage']:.1f}%]" if r['is_program_index'] else ""
            print(f"   {r['name']:<35} {r['total_docs']:>8,} docs{program_note}")
    
    if partial_vectors:
        print(f"\n⚠️  INDEXES WITH INCOMPLETE VECTORS ({len(partial_vectors)}):")
        print("-" * 50)
        for r in partial_vectors:
            print(f"   {r['name']:<35} {r['total_docs']:>8,} docs, {r['vector_percentage']:>5.1f}% vectors")
    
    if low_coverage:
        print(f"\n📉 INDEXES WITH LOW PROGRAM COVERAGE ({len(low_coverage)}):")
        print("-" * 50)
        for r in low_coverage:
            print(f"   {r['name']:<35} {r['total_docs']:>8,} docs, {r['program_coverage_percentage']:>5.1f}% coverage")
    
    if empty:
        print(f"\n⚪ EMPTY INDEXES ({len(empty)}):")
        print("-" * 50)
        for r in empty:
            print(f"   {r['name']}")
    
    print()
    print("🎯 PRIORITY ACTIONS NEEDED:")
    print("=" * 50)
    
    # Focus on critical issues first
    critical_vector_issues = [r for r in no_vectors + partial_vectors if r['total_docs'] > 1000]
    critical_coverage_issues = [r for r in results if r['is_program_index'] and r['program_coverage_percentage'] < 50]
    
    if critical_vector_issues:
        print("\n🔥 HIGH PRIORITY - Large indexes without vectors:")
        for r in sorted(critical_vector_issues, key=lambda x: x['total_docs'], reverse=True)[:5]:
            print(f"   • {r['name']}: {r['total_docs']:,} docs need vectors")
    
    if critical_coverage_issues:
        print("\n🔥 HIGH PRIORITY - Program indexes with very low coverage:")
        for r in critical_coverage_issues:
            expected = int(9678 * 0.95)  # 95% threshold
            missing = expected - r['total_docs']
            print(f"   • {r['name']}: Missing {missing:,} programs ({r['program_coverage_percentage']:.1f}% coverage)")
    
    # Specific findings
    program_deps_issue = next((r for r in results if r['name'] == 'new_cobol_program_deps'), None)
    program_flows_issue = next((r for r in results if r['name'] == 'new_cobol_program_flows'), None)
    
    if program_deps_issue and program_deps_issue['vector_percentage'] < 50:
        print(f"\n⚠️  new_cobol_program_deps: Only {program_deps_issue['vector_percentage']:.1f}% vectors")
        print("   → Run: python backfill_embeddings_program_deps.py --batch 64")
    
    if program_flows_issue and program_flows_issue['vector_percentage'] < 50:
        print(f"\n⚠️  new_cobol_program_flows: Only {program_flows_issue['vector_percentage']:.1f}% vectors")
        print("   → Run: python build_program_flows.py --all --batch 300 --push")
    
    print()
    print("📊 SUMMARY:")
    total_issues = len(no_vectors) + len(partial_vectors) + len(low_coverage)
    print(f"   • Total indexes: {len(results)}")
    print(f"   • Healthy: {len(healthy)}")
    print(f"   • Need attention: {total_issues}")
    
    if total_issues == 0:
        print("\n🎉 All indexes have good coverage!")
    else:
        print(f"\n💡 Focus on the {len(critical_vector_issues + critical_coverage_issues)} high-priority issues first")

if __name__ == '__main__':
    main()