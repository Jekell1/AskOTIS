#!/usr/bin/env python3
"""Simple script to ask OTIS RAG questions."""

from otis_rag.rag import OTISRAG

# Initialize once
rag = OTISRAG()

# Example questions
if __name__ == "__main__":
    print("OTIS RAG - Quick Test\n" + "="*60 + "\n")
    
    # Test 1: OTIS-specific question
    q1 = "What does the OTIS system do?"
    print(f"Q: {q1}")
    print(f"A: {rag.ask(q1, verbose=True)}\n")
    print("-"*60 + "\n")
    
    # Test 2: Code-specific question
    q2 = "Find programs that handle customer data"
    print(f"Q: {q2}")
    print(f"A: {rag.ask(q2, verbose=True)}\n")
    print("-"*60 + "\n")
    
    # Test 3: Follow-up (uses memory)
    q3 = "What fields are in the customer record?"
    print(f"Q: {q3}")
    print(f"A: {rag.ask(q3, verbose=True)}\n")
