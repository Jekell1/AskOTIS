"""Identify indexes with FULL source coverage AND FULL embedding coverage.

Analyzes all indexes to find which ones are 100% complete and ready for production.
"""
from __future__ import annotations
import os, json, requests

API = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
EXPECTED_PROGRAMS = 9678

# All indexes to check
ALL_INDEXES = [
    # TIER-1
    {'name': 'new_cobol_program_meta', 'vector_field': 'summary_vector', 'tier': 'TIER-1'},
    {'name': 'new_cobol_program_inventory', 'vector_field': None, 'tier': 'TIER-1'},
    {'name': 'new_cobol_program_deps', 'vector_field': 'dependency_blob_vector', 'tier': 'TIER-1'},
    {'name': 'new_cobol_program_flows', 'vector_field': 'flow_vector', 'tier': 'TIER-1'},
    # TIER-2
    {'name': 'new_cobol_paragraphs', 'vector_field': 'para_vector', 'tier': 'TIER-2'},
    {'name': 'new_cobol_program_copybook_edges', 'vector_field': None, 'tier': 'TIER-2'},
    {'name': 'new_cobol_screen_nodes', 'vector_field': 'summary_vector', 'tier': 'TIER-2'},
    {'name': 'new_cobol_copybook_usage', 'vector_field': 'context_vector', 'tier': 'TIER-2'},
    {'name': 'new_cobol_variable_usage', 'vector_field': None, 'tier': 'TIER-2'},
    {'name': 'new_cobol_data_items', 'vector_field': 'vector', 'tier': 'TIER-2'},
    {'name': 'new_cobol_flow_edges_v2', 'vector_field': 'edge_vector', 'tier': 'TIER-2'},
    {'name': 'new_cobol_symbol_refs', 'vector_field': 'excerpt_vector', 'tier': 'TIER-2'},
    # TIER-3
    {'name': 'new_cobol_calls', 'vector_field': 'snippet_vector', 'tier': 'TIER-3'},
    {'name': 'new_cobol_copybook_meta', 'vector_field': 'summary_vector', 'tier': 'TIER-3'},
    {'name': 'new_cobol_menu_trees', 'vector_field': None, 'tier': 'TIER-3'},
    {'name': 'new_cobol_ui_paths', 'vector_field': 'path_vector', 'tier': 'TIER-3'},
    {'name': 'new_cobol_name_aliases', 'vector_field': None, 'tier': 'TIER-3'},
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
    url = f"{ep}/indexes/{index_name}/docs/$count?api-version={API}"
    r = requests.get(url, headers={'api-key': key}, timeout=30)
    if r.status_code != 200:
        return 0
    return int(r.text)

def get_unique_programs(ep, key, index_name):
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

def main():
    load_settings()
    ep, key = resolve()
    
    print("=" * 100)
    print("FINDING INDEXES WITH FULL SOURCE + EMBEDDING COVERAGE")
    print("=" * 100)
    print()
    
    fully_complete = []
    source_only = []
    embedding_only = []
    partial = []
    no_vectors = []
    
    for config in ALL_INDEXES:
        index_name = config['name']
        vector_field = config['vector_field']
        tier = config['tier']
        
        print(f"Checking {index_name}...", end=' ', flush=True)
        
        # Check source coverage
        total_docs = get_total_docs(ep, key, index_name)
        prog_count = get_unique_programs(ep, key, index_name)
        
        has_full_source = False
        if prog_count is not None:
            coverage_pct = (prog_count / EXPECTED_PROGRAMS * 100)
            has_full_source = coverage_pct >= 99.0  # Allow 1% tolerance
        
        # Check embedding coverage
        has_full_embeddings = True  # Assume true if no vectors
        if vector_field:
            with_vector, total = get_embedding_coverage(ep, key, index_name)
            if with_vector is not None and total > 0:
                emb_pct = (with_vector / total * 100)
                has_full_embeddings = emb_pct >= 99.9
            else:
                has_full_embeddings = False
        
        result = {
            'index': index_name,
            'tier': tier,
            'total_docs': total_docs,
            'programs': prog_count,
            'coverage_pct': (prog_count / EXPECTED_PROGRAMS * 100) if prog_count else None,
            'vector_field': vector_field,
            'has_full_source': has_full_source,
            'has_full_embeddings': has_full_embeddings
        }
        
        # Categorize
        if not vector_field:
            no_vectors.append(result)
            print("NO VECTORS")
        elif has_full_source and has_full_embeddings:
            fully_complete.append(result)
            print("FULL ✓✓")
        elif has_full_source and not has_full_embeddings:
            source_only.append(result)
            print("SOURCE ✓")
        elif not has_full_source and has_full_embeddings:
            embedding_only.append(result)
            print("EMBEDDING ✓")
        else:
            partial.append(result)
            print("PARTIAL")
    
    print()
    print("=" * 100)
    print("✓✓ FULLY COMPLETE: Source 100% + Embeddings 100%")
    print("=" * 100)
    print()
    
    if fully_complete:
        print(f"{'Index':<40} {'Tier':<10} {'Docs':<12} {'Programs':<10} {'Vector Field':<25}")
        print("-" * 100)
        for r in fully_complete:
            print(f"{r['index']:<40} {r['tier']:<10} {r['total_docs']:>10,} {r['programs']:>8,} {r['vector_field']:<25}")
        print()
        print(f"TOTAL: {len(fully_complete)} indexes fully ready for production")
    else:
        print("NONE - No indexes have both full source and embedding coverage")
    
    print()
    print("=" * 100)
    print("✓ SOURCE ONLY: Source 100%, But Embeddings Incomplete")
    print("=" * 100)
    print()
    
    if source_only:
        print(f"{'Index':<40} {'Tier':<10} {'Docs':<12} {'Programs':<10} {'Vector Field':<25}")
        print("-" * 100)
        for r in source_only:
            print(f"{r['index']:<40} {r['tier']:<10} {r['total_docs']:>10,} {r['programs']:>8,} {r['vector_field']:<25}")
        print()
        print(f"TOTAL: {len(source_only)} indexes - Need embedding backfill only")
    else:
        print("NONE")
    
    print()
    print("=" * 100)
    print("✓ EMBEDDING ONLY: Embeddings 100%, But Source Incomplete")
    print("=" * 100)
    print()
    
    if embedding_only:
        print(f"{'Index':<40} {'Tier':<10} {'Coverage':<10} {'Programs':<15} {'Vector Field':<25}")
        print("-" * 100)
        for r in embedding_only:
            cov = f"{r['coverage_pct']:.1f}%" if r['coverage_pct'] else "N/A"
            progs = f"{r['programs']}/{EXPECTED_PROGRAMS}" if r['programs'] else "N/A"
            print(f"{r['index']:<40} {r['tier']:<10} {cov:<10} {progs:<15} {r['vector_field']:<25}")
        print()
        print(f"TOTAL: {len(embedding_only)} indexes - Need source expansion only")
    else:
        print("NONE")
    
    print()
    print("=" * 100)
    print("⚪ NO VECTORS: Indexes without vector fields (structural data)")
    print("=" * 100)
    print()
    
    if no_vectors:
        print(f"{'Index':<40} {'Tier':<10} {'Docs':<12} {'Programs':<10} {'Status':<15}")
        print("-" * 100)
        for r in no_vectors:
            status = "COMPLETE" if r['has_full_source'] else f"{r['coverage_pct']:.1f}% cov" if r['coverage_pct'] else "N/A"
            print(f"{r['index']:<40} {r['tier']:<10} {r['total_docs']:>10,} {r['programs'] if r['programs'] else 'N/A':>8} {status:<15}")
        print()
        print(f"TOTAL: {len(no_vectors)} indexes (no embeddings needed by design)")
    
    print()
    print("=" * 100)
    print("CHATBOT READINESS SUMMARY")
    print("=" * 100)
    print()
    print(f"✓✓ Fully ready:        {len(fully_complete)} indexes")
    print(f"✓  Need embeddings:    {len(source_only)} indexes (source complete)")
    print(f"✓  Need source data:   {len(embedding_only)} indexes (embeddings complete)")
    print(f"⚪ No vectors needed:  {len(no_vectors)} indexes (structural)")
    print(f"⚠  Need both:          {len(partial)} indexes (partial)")
    print()
    
    # Check for existing scripts
    print("=" * 100)
    print("CHECKING FOR EXISTING BACKFILL SCRIPTS")
    print("=" * 100)
    print()
    
    import glob
    backfill_scripts = glob.glob("backfill_*.py")
    
    if backfill_scripts:
        print("Found backfill scripts:")
        for script in sorted(backfill_scripts):
            print(f"  - {script}")
        print()
    else:
        print("No backfill_*.py scripts found")
        print()
    
    # Recommendations
    if source_only:
        print("RECOMMENDED ACTIONS:")
        print("-" * 100)
        print("Create/run embedding backfill scripts for:")
        for r in source_only:
            script_name = f"backfill_{r['index'].replace('new_cobol_', '')}_vectors.py"
            exists = script_name in backfill_scripts
            status = "EXISTS ✓" if exists else "NEEDED"
            print(f"  - {script_name:<50} {status}")
        print()

if __name__ == '__main__':
    main()
