"""Comprehensive analysis of new_cobol_paragraphs index - source and vector coverage.

Analyzes:
1. Source coverage: How many of 9,678 programs have paragraph data
2. Vector coverage: How many paragraphs have embeddings
3. Identifies missing programs
"""
from __future__ import annotations
import os, json, requests

API = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')
INDEX = 'new_cobol_paragraphs'
EXPECTED_PROGRAMS = 9678

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

def get_total_docs(ep, key):
    """Get total document count."""
    url = f"{ep}/indexes/{INDEX}/docs/$count?api-version={API}"
    r = requests.get(url, headers={'api-key': key}, timeout=30)
    if r.status_code != 200:
        raise SystemExit(f"Failed to get count: {r.text[:500]}")
    return int(r.text)

def get_unique_programs(ep, key):
    """Get unique program_id count via facets."""
    body = {
        'search': '*',
        'facets': ['program_id,count:0'],
        'top': 0
    }
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                     json=body, timeout=60)
    if r.status_code != 200:
        raise SystemExit(f"Failed to get facets: {r.text[:500]}")
    
    facets = r.json().get('@search.facets', {}).get('program_id', [])
    programs = [f['value'] for f in facets]
    return programs

def get_embedding_coverage(ep, key):
    """Check has_vector flag coverage."""
    # Count with has_vector = true
    body_true = {
        'search': '*',
        'filter': 'has_vector eq true',
        'count': True,
        'top': 0
    }
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'},
                     json=body_true, timeout=60)
    if r.status_code != 200:
        return None, None
    
    with_vector = r.json().get('@odata.count', 0)
    
    # Get total
    total = get_total_docs(ep, key)
    
    return with_vector, total

def get_sample_docs(ep, key, has_vector=True, count=5):
    """Get sample documents."""
    filter_clause = f'has_vector eq {"true" if has_vector else "false"}'
    body = {
        'search': '*',
        'filter': filter_clause,
        'top': count,
        'select': 'para_id,program_id,paragraph_name,kind,length_lines,has_vector'
    }
    url = f"{ep}/indexes/{INDEX}/docs/search?api-version={API}"
    r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'},
                     json=body, timeout=60)
    if r.status_code != 200:
        return []
    return r.json().get('value', [])

def main():
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print(f"COMPREHENSIVE ANALYSIS: {INDEX}")
    print("=" * 80)
    print()
    
    # 1. Total documents
    print("[1/5] Getting total document count...")
    total_docs = get_total_docs(ep, key)
    print(f"      Total paragraphs: {total_docs:,}")
    print()
    
    # 2. Source coverage
    print("[2/5] Analyzing source coverage (unique programs)...")
    programs = get_unique_programs(ep, key)
    prog_count = len(programs)
    coverage_pct = (prog_count / EXPECTED_PROGRAMS * 100) if prog_count > 0 else 0
    missing = EXPECTED_PROGRAMS - prog_count
    
    print(f"      Programs with paragraphs: {prog_count:,}/{EXPECTED_PROGRAMS:,} ({coverage_pct:.1f}%)")
    print(f"      Programs missing: {missing:,} ({100-coverage_pct:.1f}%)")
    print()
    
    # 3. Embedding coverage
    print("[3/5] Analyzing embedding coverage (has_vector flag)...")
    with_vector, total = get_embedding_coverage(ep, key)
    if with_vector is not None:
        emb_pct = (with_vector / total * 100) if total > 0 else 0
        without_vector = total - with_vector
        print(f"      Paragraphs with embeddings: {with_vector:,}/{total:,} ({emb_pct:.1f}%)")
        print(f"      Paragraphs without embeddings: {without_vector:,} ({100-emb_pct:.1f}%)")
    else:
        print("      ERROR: Could not retrieve embedding coverage")
    print()
    
    # 4. Sample documents with embeddings
    print("[4/5] Sampling documents WITH embeddings...")
    samples_with = get_sample_docs(ep, key, has_vector=True, count=3)
    if samples_with:
        for i, doc in enumerate(samples_with, 1):
            print(f"      Sample {i}:")
            print(f"        Program: {doc.get('program_id')}")
            print(f"        Paragraph: {doc.get('paragraph_name')}")
            print(f"        Lines: {doc.get('length_lines')}")
            print(f"        has_vector: {doc.get('has_vector')}")
    else:
        print("      No documents found with embeddings")
    print()
    
    # 5. Sample documents without embeddings
    print("[5/5] Sampling documents WITHOUT embeddings...")
    samples_without = get_sample_docs(ep, key, has_vector=False, count=3)
    if samples_without:
        for i, doc in enumerate(samples_without, 1):
            print(f"      Sample {i}:")
            print(f"        Program: {doc.get('program_id')}")
            print(f"        Paragraph: {doc.get('paragraph_name')}")
            print(f"        Lines: {doc.get('length_lines')}")
            print(f"        has_vector: {doc.get('has_vector')}")
    else:
        print("      All documents have embeddings!")
    print()
    
    # Summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print()
    print(f"Index: {INDEX}")
    print(f"Total documents: {total_docs:,}")
    print()
    print("SOURCE COVERAGE:")
    if coverage_pct >= 95:
        status = "[OK] FULL"
    elif coverage_pct > 0:
        status = "[WARN] PARTIAL"
    else:
        status = "[ERROR] NONE"
    print(f"  {status} - {prog_count:,}/{EXPECTED_PROGRAMS:,} programs ({coverage_pct:.1f}%)")
    print(f"  Missing {missing:,} programs ({100-coverage_pct:.1f}%)")
    print()
    
    print("EMBEDDING COVERAGE:")
    if with_vector is not None:
        if emb_pct >= 99.9:
            emb_status = "[OK] COMPLETE"
        elif emb_pct > 0:
            emb_status = "[WARN] PARTIAL"
        else:
            emb_status = "[ERROR] MISSING"
        print(f"  {emb_status} - {with_vector:,}/{total:,} paragraphs ({emb_pct:.1f}%)")
        print(f"  Need to embed {without_vector:,} paragraphs ({100-emb_pct:.1f}%)")
    print()
    
    # Action items
    print("ACTION ITEMS:")
    if missing > 0:
        print(f"  1. Expand source coverage: Process {missing:,} missing programs")
        print(f"     - Re-run paragraph extraction for missing programs")
    if with_vector is not None and without_vector > 0:
        print(f"  2. Backfill embeddings: Generate {without_vector:,} para_vector embeddings")
        print(f"     - Run: python backfill_paragraphs_vectors.py --batch 256")
    
    if missing == 0 and (with_vector is not None and without_vector == 0):
        print("  [OK] Index is complete!")
    print()

if __name__ == '__main__':
    main()
