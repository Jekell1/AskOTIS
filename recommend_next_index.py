"""Recommend next index to work on based on chatbot value."""
import json

data = json.load(open('comprehensive_partial_coverage_analysis.json'))

print("=" * 80)
print("NEXT INDEX RECOMMENDATION - CHATBOT VALUE PRIORITY")
print("=" * 80)

# Indexes ranked by importance for chatbot questions
indexes = [
    ("paragraphs", "Help understand program structure, sections, logic flow"),
    ("screen_nodes", "Answer 'Show user screen flow' questions"),
    ("data_items", "Explain data structures, variables, fields"),
    ("flow_edges_v2", "Trace program control flow, call chains"),
    ("variable_usage", "Show where variables are used/modified"),
    ("symbol_refs", "Find all references to symbols")
]

print("\nREMAINING PARTIAL INDEXES (sorted by current coverage):\n")

for idx_name, purpose in indexes:
    for r in data['results']:
        if idx_name in r['index']:
            src_cov = r['coverage_pct']
            total_docs = r['total_docs']
            emb_pct = r.get('embedding_pct', 'N/A')
            emb_status = f"{emb_pct}%" if emb_pct else "No vectors"
            
            print(f"• {idx_name:20} - {src_cov:5.1f}% coverage | {total_docs:,} docs | {emb_status}")
            print(f"  Purpose: {purpose}")
            print()
            break

print("=" * 80)
print("RECOMMENDATION: Work on PARAGRAPHS (highest coverage at 41.1%)")
print("=" * 80)
print("\nWhy paragraphs?")
print("✓ Already 41% coverage - closest to completion")
print("✓ 100% embeddings already done (224,655 docs)")
print("✓ Critical for questions like:")
print("  - 'Explain how APIPAY.CBL works'")
print("  - 'Show me the logic flow of DAILY.CBL'")
print("  - 'What does the PROCESS-RECORDS paragraph do?'")
print("\nAction: Run build script to expand from 3,974 to all 9,678 programs")
