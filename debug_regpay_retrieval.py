"""
Debug: Check what documents are being retrieved for REGPAY question
"""
from otis_rag.rag import OTISRAG

rag = OTISRAG()

# Do a detailed retrieval test
query = "What fields can a user modify in REGPAY?"

print("="*80)
print("Debugging Document Retrieval")
print("="*80)

# Test retrieval directly
from otis_rag.retriever import HybridRetriever
retriever = HybridRetriever(rag.config)

# Get routing
routing = rag.router.route(query)
indexes = routing['search_indexes']

print(f"\nQuery: {query}")
print(f"Indexes to search: {indexes}")

# Retrieve with small max to see top results
results = retriever.retrieve(query, indexes, max_results=100)  # Increased to get more results

print(f"\nTotal retrieved: {len(results)}")

print("\n" + "="*80)
print("TOP 10 RESULTS:")
print("="*80)

for i, doc in enumerate(results[:10], 1):
    print(f"\n--- Result #{i} ---")
    print(f"Index: {doc.get('_index_type', 'unknown')}")
    print(f"Score: {doc.get('@search.score', 0):.4f}")
    
    # Show key fields based on index type
    index_type = doc.get('_index_type', '')
    
    if index_type == 'code' or index_type == 'code_new':
        program = doc.get('program_id', 'N/A')
        name = doc.get('name', 'N/A')
        scope = doc.get('scope', 'N/A')
        print(f"Program: {program}")
        print(f"Name: {name}")
        print(f"Scope: {scope}")
        
        text = doc.get('text', '')
        # Check for MAX definition
        if 'MAX' in text and 'VALUE 3' in text:
            print("*** CONTAINS MAX=3 DEFINITION ***")
            lines = text.split('\n')
            for j, line in enumerate(lines):
                if 'MAX' in line or 'SPEC-TABLE' in line or 'ENTER NEW' in line:
                    print(f"  Line: {line.strip()[:100]}")
        elif 'MAX' in text:
            print("*** Contains MAX reference (but not definition) ***")
        
        # Show first few lines
        preview = text[:300]
        print(f"Preview: {preview}...")
    
    elif index_type == 'paragraphs':
        print(f"Program: {doc.get('program_id', 'N/A')}")
        print(f"Paragraph: {doc.get('paragraph_name', 'N/A')}")
    
    elif index_type == 'files':
        print(f"File: {doc.get('name', 'N/A')}")
        summary = doc.get('summary', '')[:200]
        print(f"Summary: {summary}...")

print("\n" + "="*80)
print("ANALYSIS")
print("="*80)

# Check if MAX=3 definition is in top 10
has_max_definition_top10 = False
for doc in results[:10]:
    text = doc.get('text', '')
    if 'MAX' in text and 'VALUE 3' in text and 'PIC 99' in text:
        has_max_definition_top10 = True
        break

# Check if MAX=3 definition is anywhere in ALL results
has_max_definition_all = False
max_def_position = None
for i, doc in enumerate(results):
    text = doc.get('text', '')
    if 'MAX' in text and 'VALUE 3' in text and 'PIC 99' in text:
        has_max_definition_all = True
        max_def_position = i + 1
        print(f"\n✓ Found MAX=3 definition at position #{max_def_position}")
        print(f"  Index: {doc.get('_index_type')}")
        print(f"  Score: {doc.get('@search.score', 0):.4f}")
        print(f"  Program: {doc.get('program_id', 'N/A')}")
        break

if has_max_definition_top10:
    print("\n✓ MAX=3 definition IS in top 10 results")
    print("  Issue: LLM may not be seeing it or prioritizing it correctly")
elif has_max_definition_all:
    print(f"\n⚠ MAX=3 definition found at position #{max_def_position} (not in top 10)")
    print("  Issue: Retrieval ranking needs adjustment")
else:
    print("\n✗ MAX=3 definition NOT in any results")
    print("  Issue: Not being retrieved at all - check indexing or search terms")

print("\nNext steps:")
if max_def_position and max_def_position > 10:
    print(f"1. Increase max_results_per_index in config (currently results in top {len(results)})")
    print("2. Adjust scoring/ranking to prioritize business logic chunks")
    print("3. Consider dedicated query expansion for ELEMENT SPECIFICATIONS sections")
elif has_max_definition_top10:
    print("1. Enhance LLM system prompt to explicitly look for MAX definitions")
    print("2. Add MAX extraction logic in generator")
else:
    print("1. Verify chunk is indexed correctly")
    print("2. Check if search terms match chunk content")
