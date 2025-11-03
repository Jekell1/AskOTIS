"""Identify indexes that don't cover all 9,500+ source files.

Analyzes all indexes to find which ones have insufficient coverage of:
1. All source files (9,951 total CBL+CPY)
2. All program files (9,678 CBL files) 
3. All copybook files (7,817 CPY files)
"""

import os
import requests
import json

def load_config():
    """Load Azure Search config."""
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
        print('Missing Azure Search credentials')
        return None, None
    
    return ep.rstrip('/'), key

def get_index_count(ep, key, index_name):
    """Get document count for an index."""
    headers = {'api-key': key, 'Content-Type': 'application/json'}
    search_url = f"{ep}/indexes/{index_name}/docs/search?api-version=2023-11-01"
    
    try:
        body = {'search': '*', 'top': 0, 'count': True}
        r = requests.post(search_url, headers=headers, json=body, timeout=10)
        if r.status_code == 200:
            return r.json().get('@odata.count', 0)
    except Exception:
        pass
    return 0

def get_all_indexes(ep, key):
    """Get list of all indexes."""
    headers = {'api-key': key}
    try:
        r = requests.get(f"{ep}/indexes?api-version=2023-11-01", headers=headers)
        if r.status_code == 200:
            return [idx['name'] for idx in r.json().get('value', [])]
    except Exception:
        pass
    return []

def classify_index(index_name):
    """Classify what type of coverage an index should have."""
    name_lower = index_name.lower()
    
    if 'file' in name_lower and 'cobol' in name_lower:
        return 'all_files', 9951, 'All source files (CBL+CPY)'
    elif 'program' in name_lower and 'copybook' not in name_lower:
        return 'programs', 9678, 'Program files (CBL)'
    elif 'copybook' in name_lower and 'program' not in name_lower:
        return 'copybooks', 7817, 'Copybook files (CPY)'
    elif 'call' in name_lower:
        return 'programs', 9678, 'Programs (should include all for full coverage)'
    elif any(x in name_lower for x in ['flow', 'edge', 'paragraph', 'symbol', 'data']):
        return 'programs', 9678, 'Program-related data'
    elif 'chunk' in name_lower:
        return 'all_files', 9951, 'Code chunks from all files'
    else:
        return 'unknown', 0, 'Unknown coverage expectation'

def main():
    ep, key = load_config()
    if not ep:
        return
    
    indexes = get_all_indexes(ep, key)
    if not indexes:
        print("Failed to get index list")
        return
    
    print("🔍 SOURCE FILE COVERAGE ANALYSIS")
    print("=" * 80)
    print()
    
    print("📊 REFERENCE COUNTS:")
    print("   • Total source files (CBL+CPY): 9,951")
    print("   • Program files (CBL): 9,678") 
    print("   • Copybook files (CPY): 7,817")
    print()
    
    # Analyze each index
    results = []
    for index in sorted(indexes):
        count = get_index_count(ep, key, index)
        coverage_type, expected, description = classify_index(index)
        
        if expected > 0:
            gap = expected - count
            coverage_pct = (count / expected) * 100
            status = '✅' if coverage_pct >= 95 else '❌'
            
            results.append({
                'name': index,
                'count': count,
                'expected': expected,
                'gap': gap,
                'coverage_pct': coverage_pct,
                'coverage_type': coverage_type,
                'description': description,
                'status': status
            })
    
    # Group by coverage type
    all_files_indexes = [r for r in results if r['coverage_type'] == 'all_files']
    program_indexes = [r for r in results if r['coverage_type'] == 'programs']
    copybook_indexes = [r for r in results if r['coverage_type'] == 'copybooks']
    
    def print_category(category_name, indexes, show_all=False):
        print(f"📋 {category_name}:")
        print("-" * 80)
        print(f"{'Index':<35} {'Current':<8} {'Expected':<8} {'Gap':<8} {'Coverage':<8} {'Status'}")
        print("-" * 80)
        
        gaps_found = []
        for r in sorted(indexes, key=lambda x: x['coverage_pct']):
            if show_all or r['coverage_pct'] < 95:
                print(f"{r['name']:<35} {r['count']:<8,} {r['expected']:<8,} {r['gap']:<8,} {r['coverage_pct']:<7.1f}% {r['status']}")
                if r['coverage_pct'] < 95:
                    gaps_found.append(r)
        
        if not gaps_found and not show_all:
            print("   ✅ All indexes in this category have 95%+ coverage")
        
        print()
        return gaps_found
    
    # Show results by category
    all_gaps = []
    
    print("🔍 ANALYSIS BY COVERAGE TYPE:")
    print("=" * 80)
    print()
    
    all_gaps.extend(print_category("ALL SOURCE FILES (9,951)", all_files_indexes))
    all_gaps.extend(print_category("PROGRAM FILES (9,678)", program_indexes))  
    all_gaps.extend(print_category("COPYBOOK FILES (7,817)", copybook_indexes))
    
    # Summary of gaps
    if all_gaps:
        print("🚨 INDEXES WITH INSUFFICIENT COVERAGE:")
        print("=" * 60)
        
        # Sort by severity (largest gaps first)
        severe_gaps = sorted(all_gaps, key=lambda x: x['gap'], reverse=True)
        
        for r in severe_gaps:
            severity = "🔥 CRITICAL" if r['gap'] > 5000 else "⚠️  MODERATE" if r['gap'] > 1000 else "📋 MINOR"
            print(f"{severity} - {r['name']}:")
            print(f"   • Missing: {r['gap']:,} files ({100-r['coverage_pct']:.1f}% gap)")
            print(f"   • Current: {r['count']:,} / {r['expected']:,} {r['description']}")
            print()
        
        print("📊 SUMMARY:")
        critical = len([r for r in all_gaps if r['gap'] > 5000])
        moderate = len([r for r in all_gaps if 1000 < r['gap'] <= 5000])
        minor = len([r for r in all_gaps if r['gap'] <= 1000])
        
        print(f"   • Critical gaps (5000+ missing): {critical}")
        print(f"   • Moderate gaps (1000-5000 missing): {moderate}")
        print(f"   • Minor gaps (<1000 missing): {minor}")
        print(f"   • Total indexes with gaps: {len(all_gaps)}")
        
        print()
        print("💡 RECOMMENDATIONS:")
        if critical > 0:
            print("   🔥 Address critical gaps first (these indexes are severely incomplete)")
        if moderate > 0:
            print("   ⚠️  Address moderate gaps for better coverage")
        print("   📋 Review if minor gaps are intentional (specialized indexes)")
        
    else:
        print("🎉 ALL INDEXES HAVE SUFFICIENT SOURCE FILE COVERAGE!")
        print("   Every index covers 95%+ of its expected file universe.")

if __name__ == '__main__':
    main()