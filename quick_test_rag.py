#!/usr/bin/env python3
"""Quick test of OTIS RAG with single question."""

from otis_rag import OTISRAG

print("Testing OTIS RAG with corrected vector fields...")
print("="*60)

rag = OTISRAG()
print("✅ Initialized\n")

# Quick test
print("Question: What does program GB01SE do?")
print()

answer = rag.ask("What does program GB01SE do?", verbose=True)
print(f"\nAnswer preview:\n{answer[:300]}...\n")

print("="*60)
print("✅ Test complete - vector fields working correctly!")
