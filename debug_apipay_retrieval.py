#!/usr/bin/env python3
"""Test APIPAY calls - with detailed debugging."""

from otis_rag import OTISRAG

rag = OTISRAG()

question = "What programs and copybooks does APIPAY call or reference?"
print(f"Question: {question}\n")

# Get the routing
routing = rag.router.route(question)
print(f"Routing:")
print(f"  Type: {routing['question_type']}")
print(f"  Indexes: {routing['search_indexes']}\n")

# Do the retrieval
context_docs = rag.retriever.retrieve(
    query=routing['clean_query'],
    indexes=routing['search_indexes'],
    max_results=rag.config.max_results_per_index
)

print(f"Retrieved {len(context_docs)} documents\n")

# Show which indexes contributed
index_counts = {}
for doc in context_docs:
    idx = doc.get('_index_type', 'unknown')
    index_counts[idx] = index_counts.get(idx, 0) + 1

print("Documents by index:")
for idx, count in sorted(index_counts.items(), key=lambda x: -x[1]):
    print(f"  {idx}: {count}")

# Show sample from 'calls' index specifically
print("\n" + "="*80)
print("Sample documents from 'calls' index:")
print("="*80)
calls_docs = [d for d in context_docs if d.get('_index_type') == 'calls']
print(f"Found {len(calls_docs)} documents from calls index\n")

for i, doc in enumerate(calls_docs[:10], 1):
    print(f"{i}. Caller: {doc.get('caller_program', 'N/A')}")
    print(f"   Callee: {doc.get('callee_program', 'N/A')}")
    print(f"   Type: {doc.get('reference_type', 'N/A')}")
    print(f"   Category: {doc.get('category', 'N/A')}")
    print()
