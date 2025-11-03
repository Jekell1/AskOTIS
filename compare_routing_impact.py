"""Compare routing profiles impact: Before vs After index counts."""

from otis_rag.router import QueryRouter

def compare_routing():
    """Show before vs after comparison of index selection."""
    
    router = QueryRouter()
    
    # Test cases with their old index counts (from previous _select_indexes logic)
    test_cases = [
        {
            "query": "What does program LONPF2 do?",
            "type": "explain_program",
            "old_count": 11,  # code, code_new + 9 more
            "description": "Program explanation"
        },
        {
            "query": "Trace the execution flow from MAINMENU",
            "type": "trace_flow",
            "old_count": 13,  # code, code_new + 11 more
            "description": "Flow tracing"
        },
        {
            "query": "What menu options are displayed?",
            "type": "menu",
            "old_count": 12,  # Would fall into general with many indexes
            "description": "Menu question"
        },
        {
            "query": "What does transaction code RP do?",
            "type": "transaction",
            "old_count": 9,  # code, code_new + 7 more
            "description": "Transaction question"
        },
        {
            "query": "Find all programs in the system",
            "type": "general",
            "old_count": 12,  # code, code_new + 10 more
            "description": "General question"
        }
    ]
    
    print("\n" + "=" * 80)
    print("ROUTING PROFILES IMPACT ANALYSIS")
    print("=" * 80)
    print()
    print("Comparison of index counts BEFORE and AFTER routing profile implementation:")
    print()
    
    total_old = 0
    total_new = 0
    
    for i, test in enumerate(test_cases, 1):
        query = test["query"]
        old_count = test["old_count"]
        description = test["description"]
        
        # Get current routing result
        result = router.route(query)
        new_count = len(result["search_indexes"])
        
        # Calculate reduction
        reduction = old_count - new_count
        reduction_pct = (reduction / old_count) * 100
        
        total_old += old_count
        total_new += new_count
        
        print(f"{i}. {description}")
        print(f"   Query: \"{query}\"")
        print(f"   Before: {old_count} indexes")
        print(f"   After:  {new_count} indexes")
        print(f"   Reduction: {reduction} indexes ({reduction_pct:.1f}%)")
        print(f"   Indexes: {', '.join(result['search_indexes'])}")
        print()
    
    # Calculate overall statistics
    total_reduction = total_old - total_new
    total_reduction_pct = (total_reduction / total_old) * 100
    avg_old = total_old / len(test_cases)
    avg_new = total_new / len(test_cases)
    
    print("=" * 80)
    print("OVERALL IMPACT:")
    print(f"  Total indexes (all queries):")
    print(f"    Before: {total_old}")
    print(f"    After:  {total_new}")
    print(f"    Reduction: {total_reduction} indexes ({total_reduction_pct:.1f}%)")
    print()
    print(f"  Average indexes per query:")
    print(f"    Before: {avg_old:.1f}")
    print(f"    After:  {avg_new:.1f}")
    print()
    print("EXPECTED BENEFITS:")
    print(f"  ⚡ ~{total_reduction_pct:.0f}% reduction in search operations")
    print(f"  ⚡ ~{total_reduction_pct:.0f}% faster retrieval latency")
    print(f"  🎯 Improved precision (less noise from irrelevant indexes)")
    print(f"  💰 Lower Azure Search API costs")
    print("=" * 80)
    print()


if __name__ == "__main__":
    compare_routing()
