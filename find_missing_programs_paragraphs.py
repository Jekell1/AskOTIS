"""Identify which programs are missing from new_cobol_paragraphs index.

Compares programs in new_cobol_program_inventory (baseline) with
programs in new_cobol_paragraphs to find the gap.
"""
from __future__ import annotations
import os, json, requests

API = os.getenv('AZURE_SEARCH_API_VERSION', '2025-08-01-preview')

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

def get_programs_from_index(ep, key, index_name, program_field='program_id', use_facets=True):
    """Get all unique program IDs from an index."""
    if use_facets:
        # Try facets first (faster)
        body = {
            'search': '*',
            'facets': [f'{program_field},count:0'],
            'top': 0
        }
        url = f"{ep}/indexes/{index_name}/docs/search?api-version={API}"
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'}, 
                         json=body, timeout=120)
        if r.status_code != 200:
            print(f"      Facets failed, using pagination instead...")
            use_facets = False
        else:
            facets = r.json().get('@search.facets', {}).get(program_field, [])
            return set(f['value'] for f in facets)
    
    # Fallback: paginate through all documents
    programs = set()
    skip = 0
    page_size = 1000
    
    while True:
        body = {
            'search': '*',
            'top': page_size,
            'skip': skip,
            'select': program_field
        }
        url = f"{ep}/indexes/{index_name}/docs/search?api-version={API}"
        r = requests.post(url, headers={'api-key': key, 'Content-Type': 'application/json'},
                         json=body, timeout=120)
        if r.status_code != 200:
            raise SystemExit(f"Failed to get programs from {index_name}: {r.text[:500]}")
        
        docs = r.json().get('value', [])
        if not docs:
            break
        
        for doc in docs:
            if program_field in doc and doc[program_field]:
                programs.add(doc[program_field])
        
        if len(docs) < page_size:
            break
        
        skip += page_size
        if skip % 5000 == 0:
            print(f"      ... processed {skip:,} documents, found {len(programs):,} unique programs")
    
    return programs

def main():
    load_settings()
    ep, key = resolve()
    
    print("=" * 80)
    print("IDENTIFYING MISSING PROGRAMS IN new_cobol_paragraphs")
    print("=" * 80)
    print()
    
    # Get baseline (all programs)
    print("[1/3] Fetching all programs from new_cobol_program_inventory...")
    all_programs = get_programs_from_index(ep, key, 'new_cobol_program_inventory')
    print(f"      Found {len(all_programs):,} programs in inventory")
    print()
    
    # Get programs with paragraph data
    print("[2/3] Fetching programs from new_cobol_paragraphs...")
    programs_with_paras = get_programs_from_index(ep, key, 'new_cobol_paragraphs')
    print(f"      Found {len(programs_with_paras):,} programs with paragraphs")
    print()
    
    # Calculate missing
    print("[3/3] Calculating missing programs...")
    missing = all_programs - programs_with_paras
    missing_sorted = sorted(missing)
    
    print(f"      Missing {len(missing):,} programs from paragraphs index")
    print()
    
    # Summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print()
    print(f"Total programs: {len(all_programs):,}")
    print(f"Programs with paragraphs: {len(programs_with_paras):,} ({len(programs_with_paras)/len(all_programs)*100:.1f}%)")
    print(f"Programs missing paragraphs: {len(missing):,} ({len(missing)/len(all_programs)*100:.1f}%)")
    print()
    
    # Show sample of missing programs
    if missing_sorted:
        print("SAMPLE OF MISSING PROGRAMS (first 50):")
        print("-" * 80)
        for i, prog in enumerate(missing_sorted[:50], 1):
            print(f"  {i:3}. {prog}")
        
        if len(missing_sorted) > 50:
            print(f"  ... and {len(missing_sorted) - 50:,} more")
        print()
        
        # Save to file
        output_file = 'missing_programs_paragraphs.json'
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump({
                'total_missing': len(missing),
                'missing_programs': missing_sorted,
                'coverage_pct': len(programs_with_paras)/len(all_programs)*100,
                'programs_with_data': len(programs_with_paras),
                'total_programs': len(all_programs)
            }, f, indent=2)
        
        print(f"Full list saved to: {output_file}")
        print()
    
    # Recommendations
    print("RECOMMENDATIONS:")
    print("-" * 80)
    print("1. Re-run paragraph extraction pipeline for missing programs")
    print("2. Check if missing programs actually have paragraph definitions")
    print("3. Some programs may be copybooks (no paragraphs expected)")
    print()

if __name__ == '__main__':
    main()
