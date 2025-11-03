#!/usr/bin/env python3
"""
Investigate OTIS Functional Domain Breakdown
"""

from otis_rag import OTISRAG

print("=" * 80)
print("INVESTIGATING OTIS FUNCTIONAL DOMAIN BREAKDOWN")
print("=" * 80)
print()

rag = OTISRAG()

# Ask comprehensive questions about OTIS functional organization
questions = [
    "What are the major functional modules or domains in the OTIS system?",
    "How is the OTIS system organized into functional areas?",
    "What are the main business functions handled by OTIS programs?",
    "List all the major menus and their purposes in OTIS",
    "What are the core functional areas: loans, dealers, addons, batch processing, etc?",
]

for i, question in enumerate(questions, 1):
    print(f"\n{'='*80}")
    print(f"QUESTION {i}: {question}")
    print("=" * 80)
    
    answer = rag.ask(question)
    
    print("\nANSWER:")
    print("-" * 80)
    print(answer)
    print()

print("\n" + "=" * 80)
print("SUMMARY: Domain Structure Analysis Complete")
print("=" * 80)
