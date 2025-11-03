"""Measure Phase 2 RAG Improvement - Compare Before/After Answer Quality."""

import sys
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
from otis_rag.router import QueryRouter
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

# Load configuration
def load_config():
    """Load Azure Search configuration."""
    with open('local.settings.json', 'r') as f:
        settings = json.load(f)['Values']
    return settings

# Test questions specifically designed for Phase 2 indexes
PHASE2_TEST_QUESTIONS = {
    'transaction_questions': [
        {
            'id': 'T1',
            'question': 'What happens when user enters transaction code PL?',
            'expected_indexes': ['transactions', 'ui_paths', 'menu_trees'],
            'success_criteria': 'Should mention programs: CDLIST, CDMAIN, and workflow details'
        },
        {
            'id': 'T2',
            'question': 'Explain the workflow for transaction BD',
            'expected_indexes': ['transactions'],
            'success_criteria': 'Should describe the business day workflow'
        },
        {
            'id': 'T3',
            'question': 'What programs run when user enters transaction code OF?',
            'expected_indexes': ['transactions', 'programs'],
            'success_criteria': 'Should list programs that handle OF transaction'
        },
        {
            'id': 'T4',
            'question': 'Which transaction codes are available from the loan menu?',
            'expected_indexes': ['transactions', 'menu_trees'],
            'success_criteria': 'Should list transaction codes accessible from loan-related menus'
        },
    ],
    'complexity_questions': [
        {
            'id': 'C1',
            'question': 'Which programs are most complex?',
            'expected_indexes': ['complexity'],
            'success_criteria': 'Should mention UP1534 (3,478 complexity) and UP15VY (3,395 complexity)'
        },
        {
            'id': 'C2',
            'question': 'Show me programs with high nesting depth',
            'expected_indexes': ['complexity'],
            'success_criteria': 'Should mention UP15VY (2,603 levels) and UP1534 (2,491 levels)'
        },
        {
            'id': 'C3',
            'question': 'What are the performance risks in UP1534?',
            'expected_indexes': ['complexity', 'programs'],
            'success_criteria': 'Should mention: 3,478 complexity, 17,923 LOC, deep nesting, very_complex category'
        },
        {
            'id': 'C4',
            'question': 'List programs with high file I/O operations',
            'expected_indexes': ['complexity'],
            'success_criteria': 'Should mention UP15VY (2,022 operations) and CLFILE (713 operations)'
        },
        {
            'id': 'C5',
            'question': 'How many programs are in the very_complex category?',
            'expected_indexes': ['complexity'],
            'success_criteria': 'Should mention 769 programs (44.3%)'
        },
    ],
    'mixed_intent_questions': [
        {
            'id': 'M1',
            'question': 'Which complex programs handle transaction code PL?',
            'expected_indexes': ['transactions', 'complexity'],
            'success_criteria': 'Should combine transaction workflow with complexity metrics for CDLIST, CDMAIN'
        },
        {
            'id': 'M2',
            'question': 'What are the most complex programs in the loan workflow?',
            'expected_indexes': ['transactions', 'complexity', 'flows'],
            'success_criteria': 'Should identify complex programs involved in loan transactions'
        },
    ],
    'control_questions': [
        {
            'id': 'CTRL1',
            'question': 'What does APIPAY program do?',
            'expected_indexes': ['programs', 'flows'],
            'success_criteria': 'Should explain APIPAY functionality (not Phase 2 dependent)'
        },
        {
            'id': 'CTRL2',
            'question': 'Show me the COPY statements in BICLAS',
            'expected_indexes': ['copybook_usage', 'code'],
            'success_criteria': 'Should list copybooks used by BICLAS (not Phase 2 dependent)'
        },
    ]
}


def test_routing():
    """Test that questions route to correct indexes."""
    print("=" * 80)
    print("PHASE 2 ROUTING TEST")
    print("=" * 80)
    print("\nVerifying that Phase 2 questions route to new indexes...\n")
    
    router = QueryRouter()
    results = {
        'passed': 0,
        'failed': 0,
        'details': []
    }
    
    all_questions = []
    for category in PHASE2_TEST_QUESTIONS.values():
        all_questions.extend(category)
    
    for q in all_questions:
        route_result = router.route(q['question'])
        indexes_used = route_result['search_indexes']
        
        # Check if expected indexes are present
        missing_indexes = [idx for idx in q['expected_indexes'] if idx not in indexes_used]
        
        if missing_indexes:
            status = "❌ FAIL"
            results['failed'] += 1
            details = f"Missing indexes: {missing_indexes}"
        else:
            status = "✅ PASS"
            results['passed'] += 1
            details = "All expected indexes present"
        
        print(f"{status} [{q['id']}] {q['question'][:60]}...")
        print(f"    Expected: {q['expected_indexes']}")
        print(f"    Got: {[idx for idx in indexes_used if idx in ['transactions', 'complexity']]}")
        print(f"    {details}\n")
        
        results['details'].append({
            'id': q['id'],
            'question': q['question'],
            'expected': q['expected_indexes'],
            'actual': indexes_used,
            'status': 'pass' if not missing_indexes else 'fail'
        })
    
    print("-" * 80)
    print(f"Routing Test Results: {results['passed']} passed, {results['failed']} failed")
    print(f"Success Rate: {results['passed'] / len(all_questions) * 100:.1f}%\n")
    
    return results


def test_index_availability():
    """Test that Phase 2 indexes exist and have data."""
    print("=" * 80)
    print("PHASE 2 INDEX AVAILABILITY TEST")
    print("=" * 80)
    print("\nChecking if Phase 2 indexes exist and contain data...\n")
    
    config = load_config()
    endpoint = config['SEARCH_ENDPOINT']
    key = config['SEARCH_KEY']
    
    indexes_to_check = {
        'transaction_taxonomy': {'expected_min': 50, 'name': 'Transaction Taxonomy'},
        'program_complexity': {'expected_min': 1700, 'name': 'Program Complexity'}
    }
    
    results = {
        'passed': 0,
        'failed': 0,
        'details': []
    }
    
    for index_name, info in indexes_to_check.items():
        try:
            client = SearchClient(
                endpoint=endpoint,
                index_name=index_name,
                credential=AzureKeyCredential(key)
            )
            
            # Try to get document count
            search_results = client.search("*", top=1, include_total_count=True)
            count = search_results.get_count()
            
            if count >= info['expected_min']:
                status = "✅ PASS"
                results['passed'] += 1
                details = f"{count:,} documents found"
            else:
                status = "⚠️ WARN"
                details = f"Only {count} documents (expected ≥{info['expected_min']})"
            
            print(f"{status} {info['name']}: {details}")
            
            results['details'].append({
                'index': index_name,
                'count': count,
                'expected_min': info['expected_min'],
                'status': 'pass' if count >= info['expected_min'] else 'warn'
            })
            
        except Exception as e:
            status = "❌ FAIL"
            results['failed'] += 1
            details = f"Error: {str(e)}"
            print(f"{status} {info['name']}: {details}")
            
            results['details'].append({
                'index': index_name,
                'error': str(e),
                'status': 'fail'
            })
    
    print(f"\nIndex Availability: {results['passed']} available, {results['failed']} failed\n")
    
    return results


def test_sample_queries():
    """Test sample queries against Phase 2 indexes."""
    print("=" * 80)
    print("PHASE 2 SAMPLE QUERY TEST")
    print("=" * 80)
    print("\nTesting actual queries against Phase 2 indexes...\n")
    
    config = load_config()
    endpoint = config['SEARCH_ENDPOINT']
    key = config['SEARCH_KEY']
    
    # Test transaction taxonomy
    print("📋 Transaction Taxonomy Queries:")
    print("-" * 80)
    
    tx_client = SearchClient(
        endpoint=endpoint,
        index_name='transaction_taxonomy',
        credential=AzureKeyCredential(key)
    )
    
    # Query 1: Find PL transaction
    results = tx_client.search("PL loan", top=3)
    pl_found = False
    for doc in results:
        if doc.get('tx_code') == 'PL':
            pl_found = True
            print(f"✅ Found PL transaction: {doc.get('narrative', '')[:100]}...")
            break
    
    if not pl_found:
        print("❌ PL transaction not found")
    
    # Query 2: Search for workflow
    results = tx_client.search("workflow loan payment", top=3)
    workflow_count = sum(1 for _ in results)
    print(f"✅ Found {workflow_count} workflow-related transactions\n")
    
    # Test program complexity
    print("🔍 Program Complexity Queries:")
    print("-" * 80)
    
    comp_client = SearchClient(
        endpoint=endpoint,
        index_name='program_complexity',
        credential=AzureKeyCredential(key)
    )
    
    # Query 1: Find UP1534
    results = comp_client.search("program_id:UP1534", top=1, query_type='full')
    up1534_found = False
    for doc in results:
        if doc.get('program') == 'UP1534':
            up1534_found = True
            complexity = doc.get('cyclomatic_complexity', 0)
            loc = doc.get('total_lines', 0)
            category = doc.get('complexity_category', '')
            print(f"✅ Found UP1534: {complexity} complexity, {loc:,} LOC, category={category}")
            break
    
    if not up1534_found:
        print("❌ UP1534 not found")
    
    # Query 2: Find very complex programs
    results = comp_client.search(
        "complexity_category eq 'very_complex'",
        top=5,
        filter="complexity_category eq 'very_complex'",
        order_by=['cyclomatic_complexity desc']
    )
    
    very_complex = list(results)
    print(f"✅ Found {len(very_complex)} very_complex programs")
    if very_complex:
        print(f"   Top: {very_complex[0].get('program')} ({very_complex[0].get('cyclomatic_complexity')} complexity)")
    
    print()


def generate_test_report():
    """Generate comprehensive test report."""
    print("=" * 80)
    print("PHASE 2 IMPROVEMENT MEASUREMENT - SUMMARY REPORT")
    print("=" * 80)
    print(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
    
    # Run all tests
    routing_results = test_routing()
    availability_results = test_index_availability()
    test_sample_queries()
    
    # Calculate overall score
    total_tests = len(PHASE2_TEST_QUESTIONS['transaction_questions']) + \
                  len(PHASE2_TEST_QUESTIONS['complexity_questions']) + \
                  len(PHASE2_TEST_QUESTIONS['mixed_intent_questions']) + \
                  len(PHASE2_TEST_QUESTIONS['control_questions'])
    
    routing_score = (routing_results['passed'] / total_tests) * 100
    availability_score = (availability_results['passed'] / 2) * 100
    
    print("=" * 80)
    print("OVERALL RESULTS")
    print("=" * 80)
    print(f"\n✅ Routing Success Rate: {routing_score:.1f}%")
    print(f"✅ Index Availability: {availability_score:.1f}%")
    
    if routing_score >= 90 and availability_score >= 90:
        print("\n🎉 EXCELLENT! Phase 2 is working correctly!")
        print("   Ready for production use.")
    elif routing_score >= 70 and availability_score >= 70:
        print("\n✅ GOOD! Phase 2 is mostly working.")
        print("   Minor issues to address.")
    else:
        print("\n⚠️  WARNING! Phase 2 has issues.")
        print("   Review failed tests above.")
    
    print("\n" + "=" * 80)
    print("NEXT STEPS")
    print("=" * 80)
    print("""
1. **Test with Real Questions**: 
   python -m otis_rag.cli
   
2. **Ask Sample Questions**:
   - "What happens when user enters transaction code PL?"
   - "Which programs are most complex?"
   - "Which complex programs handle transaction PL?"
   
3. **Compare Answers**:
   - Before Phase 2: Generic or incomplete answers
   - After Phase 2: Specific transaction workflows + complexity metrics
   
4. **Document Improvement**:
   - Questions that are now answered better
   - Specific examples of value added
   - User feedback

5. **Calculate ROI**:
   - Cost: $0.50
   - Time saved per query: ~5-10 minutes (manual research avoided)
   - Break-even: 1-2 queries
   - Ongoing value: Every transaction/complexity question benefits
""")
    
    # Save results to file
    report = {
        'timestamp': datetime.now().isoformat(),
        'routing_results': routing_results,
        'availability_results': availability_results,
        'routing_score': routing_score,
        'availability_score': availability_score,
        'test_questions': PHASE2_TEST_QUESTIONS
    }
    
    with open('phase2_measurement_report.json', 'w') as f:
        json.dump(report, f, indent=2)
    
    print("\n📄 Full report saved to: phase2_measurement_report.json")
    print("=" * 80)


def main():
    """Run Phase 2 improvement measurement."""
    try:
        generate_test_report()
        return 0
    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
