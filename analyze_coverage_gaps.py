"""Analyze program coverage and vector coverage across all indexes.

Identifies indexes that lack:
1. Full program coverage (should have ~9,678 programs)
2. Full vector coverage (100% of docs should have vectors)
"""

import os
import sys
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
    
    ep = first('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT')
    key = first('AZURE_SEARCH_KEY', 'SEARCH_KEY')
    
    if not ep or not key:
        print('Missing AZURE_SEARCH_ENDPOINT / AZURE_SEARCH_KEY', file=sys.stderr)
        sys.exit(1)
    
    return ep.rstrip('/'), key

def get_index_list(ep: str, key: str) -> List[str]:
    """Get list of all indexes."""
    headers = {'api-key': key}
    r = requests.get(f"{ep}/indexes?api-version={API_VERSION}", headers=headers)
    if r.status_code == 200:
        return [idx['name'] for idx in r.json().get('value', [])]
    return []

def analyze_index(ep: str, key: str, index_name: str) -> Dict:
    """Analyze an index for document count and vector coverage."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    search_url = f"{ep}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    
    result = {
        'name': index_name,
        'total_docs': 0,
        'vector_docs': 0,
        'vector_percentage': 0.0,
        'has_program_coverage': False,
        'program_coverage_percentage': 0.0,
        'issues': []
    }
    
    try:
        # Get total document count
        search_body = {'search': '*', 'top': 0, 'count': True}
        r = requests.post(search_url, headers=headers, json=search_body, timeout=10)
        if r.status_code == 200:
            result['total_docs'] = r.json().get('@odata.count', 0)
        else:
            result['issues'].append(f"Failed to get doc count: {r.status_code}")
            return result
        
        if result['total_docs'] == 0:
            result['issues'].append("Empty index")
            return result
        
        # Check for vector coverage (look for common vector field patterns)
        vector_fields = ['vector', 'content_vector', 'code_vector', 'dependency_blob_vector', 
                        'flow_vector', 'path_vector', 'edge_vector', 'description_vector']
        
        for vector_field in vector_fields:
            search_body = {'search': '*', 'filter': f'{vector_field} ne null', 'top': 0, 'count': True}
            r = requests.post(search_url, headers=headers, json=search_body, timeout=10)
            if r.status_code == 200:
                vector_count = r.json().get('@odata.count', 0)
                if vector_count > 0:
                    result['vector_docs'] = vector_count
                    result['vector_percentage'] = (vector_count / result['total_docs']) * 100
                    break
        
        # Check for program coverage (program-related indexes)
        expected_programs = 9678
        if 'program' in index_name.lower() and 'program_id' in get_sample_fields(ep, key, index_name):
            result['has_program_coverage'] = True
            result['program_coverage_percentage'] = (result['total_docs'] / expected_programs) * 100
            if result['program_coverage_percentage'] < 95:
                result['issues'].append(f"Low program coverage: {result['program_coverage_percentage']:.1f}%")
        
        # Identify issues
        if result['vector_docs'] == 0:
            result['issues'].append("No vector embeddings")
        elif result['vector_percentage'] < 99:
            result['issues'].append(f"Incomplete vectors: {result['vector_percentage']:.1f}%")
        
    except Exception as e:
        result['issues'].append(f"Analysis error: {str(e)}")
    
    return result

def get_sample_fields(ep: str, key: str, index_name: str) -> List[str]:
    """Get field names from a sample document."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    search_url = f"{ep}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
    
    try:
        search_body = {'search': '*', 'top': 1}
        r = requests.post(search_url, headers=headers, json=search_body, timeout=5)
        if r.status_code == 200:
            docs = r.json().get('value', [])
            if docs:
                return list(docs[0].keys())
    except:
        pass
    return []

def main():
    ep, key = load_config()
    indexes = get_index_list(ep, key)
    
    print("🔍 PROGRAM COVERAGE & VECTOR COVERAGE ANALYSIS")
    print("=" * 70)
    print()
    
    # Focus on key indexes that should have program coverage
    program_indexes = [idx for idx in indexes if 'program' in idx.lower() or 'cobol' in idx.lower()]
    other_indexes = [idx for idx in indexes if idx not in program_indexes]
    
    print("📊 PROGRAM-RELATED INDEXES:")
    print("-" * 70)
    print(f"{'Index Name':<35} {'Docs':<8} {'Vector%':<8} {'Issues'}")
    print("-" * 70)
    
    program_issues = []
    for idx in sorted(program_indexes):
        result = analyze_index(ep, key, idx)
        issues_str = '; '.join(result['issues'][:2]) if result['issues'] else "✅ OK"
        print(f"{idx:<35} {result['total_docs']:<8,} {result['vector_percentage']:<7.1f}% {issues_str}")
        
        if result['issues']:
            program_issues.append((idx, result))
    
    print()
    print("📊 OTHER INDEXES:")
    print("-" * 70)
    print(f"{'Index Name':<35} {'Docs':<8} {'Vector%':<8} {'Issues'}")
    print("-" * 70)
    
    other_issues = []
    for idx in sorted(other_indexes):
        result = analyze_index(ep, key, idx)
        issues_str = '; '.join(result['issues'][:2]) if result['issues'] else "✅ OK"
        print(f"{idx:<35} {result['total_docs']:<8,} {result['vector_percentage']:<7.1f}% {issues_str}")
        
        if result['issues']:
            other_issues.append((idx, result))
    
    print()
    print("🚨 INDEXES NEEDING ATTENTION:")
    print("=" * 50)
    
    all_issues = program_issues + other_issues
    if not all_issues:
        print("✅ All indexes have good coverage!")
        return
    
    for idx, result in all_issues:
        print(f"\n🔴 {idx}:")
        for issue in result['issues']:
            print(f"   • {issue}")
        print(f"   📊 Docs: {result['total_docs']:,}, Vectors: {result['vector_percentage']:.1f}%")
    
    print()
    print("💡 RECOMMENDATIONS:")
    
    no_vectors = [idx for idx, result in all_issues if result['vector_docs'] == 0]
    partial_vectors = [idx for idx, result in all_issues if 0 < result['vector_percentage'] < 99]
    low_coverage = [idx for idx, result in all_issues if result.get('program_coverage_percentage', 100) < 95]
    
    if no_vectors:
        print(f"   🔹 Add vector fields: {', '.join(no_vectors[:3])}")
    if partial_vectors:
        print(f"   🔹 Complete vector backfill: {', '.join(partial_vectors[:3])}")
    if low_coverage:
        print(f"   🔹 Improve program coverage: {', '.join(low_coverage[:3])}")

if __name__ == '__main__':
    main()