#!/usr/bin/env python3
"""
Analyze which indexes may have structural limits similar to paragraphs (41.1%).
Based on the insight that not all programs contain all types of content.
"""
import json

def analyze_structural_limits():
    with open('comprehensive_partial_coverage_analysis.json', 'r') as f:
        data = json.load(f)
    
    print('=' * 80)
    print('STRUCTURAL LIMIT ANALYSIS - Which Indexes Are Naturally Limited?')
    print('=' * 80)
    print()
    print('Based on COBOL program structure, some indexes cannot reach 100% coverage:')
    print()
    
    # Category 1: Proven structural limits
    print('CATEGORY 1: ✅ PROVEN STRUCTURAL LIMITS')
    print('-' * 80)
    for r in data['results']:
        idx = r['index']
        pct = r['coverage_pct']
        
        if 'paragraphs' in idx:
            have = r['programs_with_data']
            missing = r['programs_missing']
            print(f"✅ {idx:30s} {pct:5.1f}% MAX")
            print(f"   Programs with data: {have:,}")
            print(f"   Programs without:   {missing:,}")
            print(f"   Reason: Only programs with PROCEDURE DIVISION paragraphs")
            print(f"   Proof: Re-processing 9,951 files found ZERO new paragraphs")
            print()
    
    # Category 2: Likely structural limits (similar coverage to paragraphs)
    print('CATEGORY 2: ⚠️  LIKELY STRUCTURAL LIMITS (Similar to Paragraphs)')
    print('-' * 80)
    print('Indexes with 35-50% coverage may be structurally limited:')
    print()
    
    for r in data['results']:
        idx = r['index']
        pct = r['coverage_pct']
        
        # Similar coverage to paragraphs suggests structural limit
        if 35 <= pct <= 50 and 'paragraphs' not in idx:
            have = r['programs_with_data']
            missing = r['programs_missing']
            print(f"⚠️  {idx:30s} {pct:5.1f}%")
            print(f"   Programs with data: {have:,}")
            print(f"   Programs without:   {missing:,}")
            
            # Hypothesis about why it's limited
            if 'screen' in idx:
                print(f"   Hypothesis: Only programs with BMS/screen definitions")
                print(f"   ~60% of programs may have NO screen/UI components")
            
            print()
    
    # Category 3: Moderate coverage (15-35%) - Could be limits or gaps
    print('CATEGORY 3: 🤔 MODERATE COVERAGE (15-35%) - Unclear if Limited or Gap')
    print('-' * 80)
    print('These might be structural limits OR expansion opportunities:')
    print()
    
    for r in data['results']:
        idx = r['index']
        pct = r['coverage_pct']
        
        if 15 <= pct < 35:
            have = r['programs_with_data']
            missing = r['programs_missing']
            print(f"🤔 {idx:30s} {pct:5.1f}%")
            print(f"   Programs with data: {have:,}")
            print(f"   Programs without:   {missing:,}")
            
            # Analysis
            if 'copybook_usage' in idx:
                print(f"   Expected: Should be ~100% (all programs use copybooks)")
                print(f"   Status: LIKELY A GAP - should expand")
            elif 'data_items' in idx:
                print(f"   Expected: Should be ~100% (all programs have DATA DIVISION)")
                print(f"   Status: LIKELY A GAP - should expand")
            elif 'variable_usage' in idx:
                print(f"   Expected: Should be ~100% (all programs use variables)")
                print(f"   Status: LIKELY A GAP - should expand")
            elif 'flow_edges' in idx:
                print(f"   Expected: Should be ~100% (all programs have control flow)")
                print(f"   Status: LIKELY A GAP - should expand")
            
            print()
    
    # Category 4: Very low coverage (< 15%)
    print('CATEGORY 4: 🔥 VERY LOW COVERAGE (<15%) - Definitely Gaps')
    print('-' * 80)
    
    for r in data['results']:
        idx = r['index']
        pct = r['coverage_pct']
        
        if 0 < pct < 15:
            have = r['programs_with_data']
            missing = r['programs_missing']
            print(f"🔥 {idx:30s} {pct:5.1f}%")
            print(f"   Programs with data: {have:,}")
            print(f"   Programs without:   {missing:,}")
            print(f"   Status: CRITICAL GAP - definitely should expand")
            print()
    
    print()
    print('=' * 80)
    print('KEY INSIGHT FROM EMBEDDING SESSION (October 13, 2025):')
    print('=' * 80)
    print()
    print('We achieved 100% EMBEDDING coverage on all 13 indexes by:')
    print('  1. flow_edges_v2: 95.4% → 100.0% (+17,885 embeddings)')
    print('  2. variable_usage: 57.3% → 100.0% (+45,530 embeddings)')
    print('  3. name_aliases: 0% → 100.0% (+55,636 embeddings)')
    print()
    print('Total: 119,051 new embeddings added in ~71.6 minutes')
    print()
    print('BUT: 100% embedding ≠ 100% program coverage!')
    print()
    print('Embedding coverage = % of existing docs with vectors')
    print('Program coverage = % of programs represented in index')
    print()
    print('Our embedding work ensured all EXISTING documents have vectors.')
    print('But some indexes may be at their STRUCTURAL MAXIMUM for program coverage.')
    print()
    
    print('=' * 80)
    print('RECOMMENDATIONS:')
    print('=' * 80)
    print()
    print('✅ Accept as COMPLETE (structural limits):')
    print('   • paragraphs (41.1%) - PROVEN via re-processing')
    print('   • screen_nodes (40.6%) - LIKELY (similar pattern)')
    print()
    print('🤔 INVESTIGATE before expanding:')
    print('   • Any index with 35-50% coverage')
    print('   • Sample missing programs to check for content')
    print()
    print('📊 EXPAND (genuine gaps):')
    print('   • copybook_usage (20.1% → ~100%)')
    print('   • data_items (17.3% → ~100%)')
    print('   • variable_usage (17.0% → ~100%)')
    print('   • flow_edges_v2 (16.6% → ~100%)')
    print('   • symbol_refs (4.8% → ~100%)')
    print()

if __name__ == '__main__':
    analyze_structural_limits()
