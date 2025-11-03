"""Comprehensive analysis of ALL partial coverage indexes.

Analyzes source and vector coverage for all 8 TIER-2 indexes:
- new_cobol_paragraphs
- new_cobol_program_copybook_edges
- new_cobol_screen_nodes
- new_cobol_copybook_usage
- new_cobol_variable_usage
- new_cobol_data_items
- new_cobol_flow_edges_v2
- new_cobol_symbol_refs
"""
from __future__ import annotations
import os, json, requests
from datetime import datetime

API = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
EXPECTED_PROGRAMS = 9678

# Index configurations
INDEXES = [
    {'name': 'new_cobol_paragraphs', 'vector_field': 'para_vector', 'has_vector': True},
    {'name': 'new_cobol_program_copybook_edges', 'vector_field': None, 'has_vector': False},
    {'name': 'new_cobol_screen_nodes', 'vector_field': 'summary_vector', 'has_vector': True},
    {'name': 'new_cobol_copybook_usage', 'vector_field': 'context_vector', 'has_vector': True},
    {'name': 'new_cobol_variable_usage', 'vector_field': None, 'has_vector': False},
    {'name': 'new_cobol_data_items', 'vector_field': 'vector', 'has_vector': True},
    {'name': 'new_cobol_flow_edges_v2', 'vector_field': 'edge_vector', 'has_vector': True},
    {'name': 'new_cobol_symbol_refs', 'vector_field': 'excerpt_vector', 'has_vector': True},
]

def load_settings():
    try:
        vals = json.load(open('local.settings.json', 'r', encoding='utf-8')).get('Values', {})
        for k in ('AZURE_SEARCH_ENDPOINT', 'SEARCH_ENDPOINT', 'AZURE_SEARCH_KEY', 'SEARCH_KEY'):
            if k in vals and k not in os.environ:
                os.environ[k] = vals[k]
    except Exception:
        pass

def resolve():
    ep = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
    key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
    if not ep or not key:
        raise SystemExit('Missing endpoint/key')
    return ep.rstrip('/'), key

def get_total_docs(ep, key, index_name):
    """Get total document count."""
    url = f"{ep}/indexes/{index_name}/docs/$count?api-version={API}"
    r = requests.get(url, headers={'api-key': key}, timeout=30)
    if r.status_code != 200:
        return 0
    return int(r.text)

def get_unique_programs(ep, key, index_name):
    """Get unique program_id count via facets."""
    body = {
        'search': '*',
        'facets': ['program_id,count:0'],
        'top': 0
    }
    url = f"{ep}/indexes/{index_name}/docs/search?api-version={API}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                     json=body, timeout=90)
    if r.status_code != 200:
        return None
    
    facets = r.json().get('@search.facets', {}).get('program_id', [])
    return len(facets)

def get_embedding_coverage(ep, key, index_name):
    """Check has_vector flag coverage."""
    body = {
        'search': '*',
        'filter': 'has_vector eq true',
        'count': True,
        'top': 0
    }
    url = f"{ep}/indexes/{index_name}/docs/search?api-version={API}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'},
                     json=body, timeout=90)
    if r.status_code != 200:
        return None, None
    
    with_vector = r.json().get('@odata.count', 0)
    total = get_total_docs(ep, key, index_name)
    
    return with_vector, total

def analyze_index(ep, key, config):
    """Analyze a single index."""
    index_name = config['name']
    vector_field = config['vector_field']
    has_vector_flag = config['has_vector']
    
    result = {
        'index': index_name,
        'vector_field': vector_field,
        'has_vector_flag': has_vector_flag
    }
    
    # Get total docs
    total_docs = get_total_docs(ep, key, index_name)
    result['total_docs'] = total_docs
    
    # Get source coverage
    prog_count = get_unique_programs(ep, key, index_name)
    if prog_count is not None:
        result['programs_with_data'] = prog_count
        result['programs_missing'] = EXPECTED_PROGRAMS - prog_count
        result['coverage_pct'] = (prog_count / EXPECTED_PROGRAMS * 100) if prog_count > 0 else 0
    else:
        result['programs_with_data'] = None
        result['programs_missing'] = None
        result['coverage_pct'] = None
    
    # Get embedding coverage
    if has_vector_flag and vector_field:
        with_vector, total = get_embedding_coverage(ep, key, index_name)
        if with_vector is not None:
            result['docs_with_embeddings'] = with_vector
            result['docs_without_embeddings'] = total - with_vector
            result['embedding_pct'] = (with_vector / total * 100) if total > 0 else 0
        else:
            result['docs_with_embeddings'] = None
            result['docs_without_embeddings'] = None
            result['embedding_pct'] = None
    else:
        result['docs_with_embeddings'] = None
        result['docs_without_embeddings'] = None
        result['embedding_pct'] = None
    
    return result

def main():
    load_settings()
    ep, key = resolve()
    
    print("=" * 100)
    print("COMPREHENSIVE PARTIAL COVERAGE ANALYSIS - ALL TIER-2 INDEXES")
    print("=" * 100)
    print(f"Analysis started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Expected programs: {EXPECTED_PROGRAMS:,}")
    print(f"Analyzing {len(INDEXES)} indexes...")
    print()
    
    results = []
    
    for i, config in enumerate(INDEXES, 1):
        index_name = config['name']
        print(f"[{i}/{len(INDEXES)}] Analyzing {index_name}...", end=' ', flush=True)
        
        try:
            result = analyze_index(ep, key, config)
            results.append(result)
            print("DONE")
        except Exception as e:
            print(f"ERROR: {e}")
            results.append({
                'index': index_name,
                'error': str(e)
            })
    
    print()
    print("=" * 100)
    print("RESULTS BY INDEX")
    print("=" * 100)
    print()
    
    for result in results:
        if 'error' in result:
            print(f"X {result['index']}")
            print(f"  ERROR: {result['error']}")
            print()
            continue
        
        idx = result['index']
        total_docs = result['total_docs']
        
        print(f">>> {idx}")
        print(f"    Total documents: {total_docs:,}")
        print()
        
        # Source coverage
        print("    SOURCE COVERAGE:")
        if result['programs_with_data'] is not None:
            prog_count = result['programs_with_data']
            missing = result['programs_missing']
            cov_pct = result['coverage_pct']
            
            if cov_pct >= 95:
                status = "[OK] FULL"
            elif cov_pct > 0:
                status = "[WARN] PARTIAL"
            else:
                status = "[ERROR] NONE"
            
            print(f"      {status} - {prog_count:,}/{EXPECTED_PROGRAMS:,} programs ({cov_pct:.1f}%)")
            print(f"      Missing: {missing:,} programs ({100-cov_pct:.1f}%)")
        else:
            print("      [INFO] No program_id field or not facetable")
        print()
        
        # Embedding coverage
        if result['vector_field']:
            print(f"    EMBEDDING COVERAGE ({result['vector_field']}):")
            if result['docs_with_embeddings'] is not None:
                with_emb = result['docs_with_embeddings']
                without_emb = result['docs_without_embeddings']
                emb_pct = result['embedding_pct']
                
                if emb_pct >= 99.9:
                    emb_status = "[OK] COMPLETE"
                elif emb_pct > 0:
                    emb_status = "[WARN] PARTIAL"
                else:
                    emb_status = "[ERROR] MISSING"
                
                print(f"      {emb_status} - {with_emb:,}/{total_docs:,} docs ({emb_pct:.1f}%)")
                print(f"      Need embeddings: {without_emb:,} docs ({100-emb_pct:.1f}%)")
            else:
                print("      [INFO] Could not determine embedding status")
        else:
            print("    EMBEDDING COVERAGE:")
            print("      [INFO] No vector field (no embeddings expected)")
        print()
    
    # Summary tables
    print("=" * 100)
    print("SUMMARY: SOURCE COVERAGE")
    print("=" * 100)
    print()
    print(f"{'Index':<40} {'Programs':<15} {'Missing':<15} {'Coverage':<10} {'Status'}")
    print("-" * 100)
    
    for result in results:
        if 'error' in result or result['programs_with_data'] is None:
            continue
        
        idx = result['index']
        prog_count = result['programs_with_data']
        missing = result['programs_missing']
        cov_pct = result['coverage_pct']
        
        if cov_pct >= 95:
            status = "OK FULL"
        elif cov_pct >= 50:
            status = "WARN PARTIAL"
        elif cov_pct > 0:
            status = "WARN LOW"
        else:
            status = "ERROR NONE"
        
        print(f"{idx:<40} {prog_count:>6,}/{EXPECTED_PROGRAMS:<6,} {missing:>6,} ({100-cov_pct:>4.1f}%) {cov_pct:>5.1f}%     {status}")
    
    print()
    print("=" * 100)
    print("SUMMARY: EMBEDDING COVERAGE")
    print("=" * 100)
    print()
    print(f"{'Index':<40} {'Vector Field':<25} {'Embedded':<15} {'Missing':<15} {'%':<10}")
    print("-" * 100)
    
    for result in results:
        if 'error' in result or not result['vector_field']:
            continue
        
        idx = result['index']
        vec_field = result['vector_field']
        
        if result['docs_with_embeddings'] is not None:
            with_emb = result['docs_with_embeddings']
            without_emb = result['docs_without_embeddings']
            emb_pct = result['embedding_pct']
            total = result['total_docs']
            
            print(f"{idx:<40} {vec_field:<25} {with_emb:>12,} {without_emb:>12,}   {emb_pct:>5.1f}%")
    
    print()
    print("=" * 100)
    print("ACTION ITEMS")
    print("=" * 100)
    print()
    
    # Source expansion needed
    need_expansion = [r for r in results if 'error' not in r and r.get('programs_with_data') and r['coverage_pct'] < 95]
    if need_expansion:
        print("SOURCE COVERAGE EXPANSION NEEDED:")
        print("-" * 100)
        for r in sorted(need_expansion, key=lambda x: x['coverage_pct']):
            print(f"  - {r['index']:<40} {r['programs_missing']:>6,} programs missing ({100-r['coverage_pct']:.1f}%)")
        print()
    
    # Embeddings needed
    need_embeddings = [r for r in results if 'error' not in r and r.get('docs_without_embeddings') and r['docs_without_embeddings'] > 0]
    if need_embeddings:
        print("EMBEDDING BACKFILL NEEDED:")
        print("-" * 100)
        for r in sorted(need_embeddings, key=lambda x: x['docs_without_embeddings']):
            docs = r['docs_without_embeddings']
            print(f"  - {r['index']:<40} {docs:>10,} docs need {r['vector_field']}")
        print()
        
        total_embeddings_needed = sum(r['docs_without_embeddings'] for r in need_embeddings)
        print(f"TOTAL EMBEDDINGS NEEDED: {total_embeddings_needed:,} documents")
    
    # Save results
    output = {
        'analysis_date': datetime.now().isoformat(),
        'expected_programs': EXPECTED_PROGRAMS,
        'results': results
    }
    
    with open('comprehensive_partial_coverage_analysis.json', 'w', encoding='utf-8') as f:
        json.dump(output, f, indent=2)
    
    print()
    print("Full results saved to: comprehensive_partial_coverage_analysis.json")
    print()

if __name__ == '__main__':
    main()
