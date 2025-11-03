"""
Ask the RAG to provide a text menu tree from GTFORM down 2 levels
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: Menu tree from top down 2 levels")
print("="*80)

question = """
Show me a text-based menu tree structure starting from GTFORM (the initial entry screen) 
going down 2 levels. 

I want to see:
- Level 0: GTFORM
- Level 1: All the menus that GTFORM calls (like WIMENU, SPMENU, PGMENU, etc.)
- Level 2: What each of those menus calls

Format it as a visual tree structure so I can see the hierarchy clearly.
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
