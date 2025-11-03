"""Comprehensive coverage check for all COBOL indexes.

This script checks document counts and vector coverage across all major COBOL indexes
to ensure complete CBL/CPY file coverage and embeddings.
"""
import requests
import os
from secrets_loader import load_secrets

API_VERSION = '2023-11-01'

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def get_index_info(ep, key, index_name):
    """Get document count and vector coverage for an index."""
    try:
        # Get document count with vector coverage
        url = f"{ep}/indexes/{index_name}/docs/search?api-version={API_VERSION}"
        
        # Total documents
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                         json={'search': '*', 'top': 0, 'count': True})
        if r.status_code != 200:
            return {'error': f'Failed to get count: {r.status_code}'}
        
        total_docs = r.json().get('@odata.count', 0)
        
        # Vector coverage (look for has_vector field)
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                         json={'search': '*', 'top': 0, 'count': True, 
                               'filter': 'has_vector eq true'})
        
        if r.status_code == 200:
            vector_docs = r.json().get('@odata.count', 0)
            vector_pct = (vector_docs / total_docs * 100) if total_docs > 0 else 0
        else:
            # Index might not have has_vector field
            vector_docs = None
            vector_pct = None
        
        return {
            'total_docs': total_docs,
            'vector_docs': vector_docs,
            'vector_pct': vector_pct
        }
    except Exception as e:
        return {'error': str(e)}

def get_all_indexes(ep, key):
    """Get all indexes from the search service."""
    try:
        url = f"{ep}/indexes?api-version={API_VERSION}"
        r = requests.get(url, headers={'api-key': key})
        if r.status_code == 200:
            data = r.json()
            return [idx['name'] for idx in data.get('value', [])]
        return []
    except Exception:
        return []

def main():
    load_secrets()
    ep, key = resolve()
    
    # Get all indexes dynamically
    all_indexes = get_all_indexes(ep, key)
    
    if not all_indexes:
        print("❌ Failed to get indexes list")
        return
    
    print("=== COMPREHENSIVE COBOL INDEX COVERAGE REPORT ===\n")
    print(f"Found {len(all_indexes)} total indexes")
    print(f"{'Index Name':<35} {'Total Docs':<12} {'Vector Docs':<12} {'Vector %':<10} {'Status'}")
    print("=" * 85)
    
    total_coverage = {}
    
    for index in sorted(all_indexes):
        info = get_index_info(ep, key, index)
        
        if 'error' in info:
            print(f"{index:<35} {'ERROR':<12} {info['error']:<12}")
            continue
            
        total_docs = info['total_docs']
        vector_docs = info['vector_docs']
        vector_pct = info['vector_pct']
        
        if vector_docs is not None:
            status = "✅ COMPLETE" if vector_pct >= 99.0 else "⚠️  PARTIAL" if vector_pct >= 90.0 else "❌ LOW"
            vector_str = f"{vector_docs}"
            pct_str = f"{vector_pct:.1f}%"
        else:
            status = "📝 NO VECTORS" if total_docs > 0 else "📭 EMPTY"
            vector_str = "N/A"
            pct_str = "N/A"
        
        print(f"{index:<35} {total_docs:<12} {vector_str:<12} {pct_str:<10} {status}")
        total_coverage[index] = info
    
    print("\n" + "=" * 85)
    
    # Summary analysis
    print("\n=== COVERAGE ANALYSIS ===")
    
    # File universe reference
    print(f"📁 Total CBL/CPY files in filesystem: 9,951")
    print(f"📁 Unique program IDs expected: ~9,536")
    
    # Key metrics
    prog_meta = total_coverage.get('new_cobol_program_meta', {})
    copybook_meta = total_coverage.get('new_cobol_copybook_meta', {})
    files_index = total_coverage.get('new-cobol-files', {})
    
    print(f"\n📊 Program Coverage:")
    print(f"   • Program Meta: {prog_meta.get('total_docs', 0):,} programs")
    print(f"   • Copybook Meta: {copybook_meta.get('total_docs', 0):,} copybooks") 
    print(f"   • File Index: {files_index.get('total_docs', 0):,} files")
    
    # Vector completion status
    complete_vectors = sum(1 for idx, info in total_coverage.items() 
                          if info.get('vector_pct') is not None and info.get('vector_pct', 0) >= 99.0)
    partial_vectors = sum(1 for idx, info in total_coverage.items() 
                         if info.get('vector_pct') is not None and 90.0 <= info.get('vector_pct', 0) < 99.0)
    
    print(f"\n🎯 Vector Embedding Status:")
    print(f"   • Complete (99%+): {complete_vectors} indexes")
    print(f"   • Partial (90-99%): {partial_vectors} indexes")
    print(f"   • Total checked: {len([i for i in total_coverage.values() if 'error' not in i])} indexes")
    
    # Overall assessment
    if complete_vectors >= 8:  # Most important indexes
        print(f"\n✅ OVERALL STATUS: EXCELLENT COVERAGE")
        print(f"   Most critical indexes have complete embeddings")
    elif complete_vectors >= 5:
        print(f"\n⚠️  OVERALL STATUS: GOOD COVERAGE") 
        print(f"   Some key indexes may need attention")
    else:
        print(f"\n❌ OVERALL STATUS: NEEDS IMPROVEMENT")
        print(f"   Multiple indexes require embedding completion")

if __name__ == '__main__':
    main()