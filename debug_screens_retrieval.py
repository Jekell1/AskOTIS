"""Debug what's being retrieved from screens index"""
import sys
sys.path.append('.')

from otis_rag.rag import OTISRAG
import json

# Initialize RAG
print("Initializing RAG...")
rag = OTISRAG()

# Test query
query = "What menu options are shown on the LPCPMU screen?"
print(f"\nQuery: {query}\n")

# Get routing
routing = rag.router.route(query)
print(f"Routing: {json.dumps(routing, indent=2)}\n")

# Get retrieval
print("Retrieving documents...")
context_docs = rag.retriever.retrieve(
    query=routing['clean_query'],
    indexes=routing['search_indexes'],
    max_results=20,
    index_weights=routing.get('index_weights', {}),
    question_type=routing.get('question_type')
)

print(f"\nRetrieved {len(context_docs)} documents\n")

# Show first result from screens index
for doc in context_docs:
    if doc.get('_index_type') == 'screens':
        print("="*80)
        print("FIRST SCREENS DOCUMENT:")
        print("="*80)
        print(f"Screen name: {doc.get('screen_name', 'N/A')}")
        print(f"SCN filename: {doc.get('scn_filename', 'N/A')}")
        print(f"AI Description: {doc.get('ai_description', 'N/A')[:200]}...")
        print(f"Menu Info: {doc.get('ai_menu_screen_info', 'N/A')[:500]}...")
        print(f"\nAll fields: {list(doc.keys())}")
        break
