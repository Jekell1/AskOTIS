"""Recommend next index after paragraphs."""
import json

data = json.load(open('comprehensive_partial_coverage_analysis.json'))

print("=" * 80)
print("NEXT INDEX RECOMMENDATION (AFTER PARAGRAPHS)")
print("=" * 80)

# Remaining indexes by chatbot value
remaining = [
    ("screen_nodes", 40.6, "Answer 'Show user screen flow' questions", "⚠️ HAD SCHEMA ERROR"),
    ("data_items", 17.3, "Explain data structures, variables, fields", "✅ Ready"),
    ("flow_edges_v2", 16.6, "Trace program control flow, call chains", "✅ Ready"),
    ("variable_usage", 17.0, "Show where variables are used/modified", "⚠️ No embeddings"),
    ("symbol_refs", 4.8, "Find all references to symbols", "✅ Ready but low value"),
]

print("\nREMAINING INDEXES (sorted by current coverage):\n")

for idx_name, cov, purpose, status in remaining:
    print(f"• {idx_name:20} - {cov:5.1f}% | {status}")
    print(f"  Purpose: {purpose}")
    print()

print("=" * 80)
print("RECOMMENDATION: Skip screen_nodes (schema issue), work on DATA_ITEMS")
print("=" * 80)

print("\nWhy data_items?")
print("✓ Critical for chatbot questions about data structures")
print("  - 'What fields are in CUSTOMER-RECORD?'")
print("  - 'Show me the structure of INVOICE-DATA'")
print("  - 'What's the definition of ACCOUNT-BALANCE?'")
print("✓ 267,769 docs already with 100% embeddings")
print("✓ 17.3% coverage - room to expand to more programs")
print("✓ No known schema issues")

print("\nAlternative: FLOW_EDGES_V2 (16.6% coverage)")
print("✓ Essential for 'show me the flow' questions")
print("✓ 366,650 edges with 100% embeddings")
print("✓ Helps trace PERFORM chains and program flow")
