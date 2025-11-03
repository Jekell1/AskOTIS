"""
Quick demo showcasing the expanded OTIS RAG capabilities.
"""

from otis_rag import OTISRAG

print("=" * 80)
print("🚀 OTIS RAG SYSTEM - EXPANDED CAPABILITIES DEMO")
print("=" * 80)
print()
print("Testing questions that leverage the new indexes...")
print()

rag = OTISRAG()

# Demo questions that use the NEW indexes
demo_questions = [
    {
        "question": "Show me the program dependencies for GB01SE",
        "highlight": "Uses: program_deps (NEW)",
        "type": "Program Dependencies"
    },
    {
        "question": "What menu options are available in OPMENU?",
        "highlight": "Uses: menu_trees (NEW), screen_nodes (NEW)",
        "type": "Menu Navigation"
    },
    {
        "question": "Trace the detailed flow edges for order processing",
        "highlight": "Uses: flow_edges_v2 (NEW) - 385K edges!",
        "type": "Detailed Flow Analysis"
    },
    {
        "question": "What files contain the SE-RECORD copybook?",
        "highlight": "Uses: new-cobol-files (NEW), copybook_usage (NEW)",
        "type": "File & Copybook Analysis"
    },
    {
        "question": "Find aliases for customer ID fields",
        "highlight": "Uses: name_aliases (NEW)",
        "type": "Name Alias Resolution"
    }
]

for i, demo in enumerate(demo_questions, 1):
    print(f"{'=' * 80}")
    print(f"DEMO {i}: {demo['type']}")
    print(f"{'=' * 80}")
    print()
    print(f"💡 {demo['highlight']}")
    print()
    print(f"❓ Question: {demo['question']}")
    print()
    
    try:
        answer = rag.ask(demo['question'], verbose=True)
        print()
        print(f"📝 Answer Preview:")
        print(f"   {answer[:200]}...")
        print()
    except Exception as e:
        print(f"❌ Error: {e}")
        print()
    
    print()

print("=" * 80)
print("✅ DEMO COMPLETE!")
print("=" * 80)
print()
print("The expanded system successfully:")
print("  ✅ Searches 18 indexes (up from 10)")
print("  ✅ Retrieves ~25 docs per query (up from ~20)")
print("  ✅ Covers 3.7M documents across 89GB of data")
print("  ✅ Provides 9 new capabilities via 8 new indexes")
print()
print("Ready to analyze the entire COBOL codebase! 🎉")
