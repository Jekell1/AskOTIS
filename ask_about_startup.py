"""
Ask the RAG about OTIS startup screen
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: What is the initial startup screen when OTIS starts?")
print("="*80)

question = """
What is the initial startup screen or entry point when a user first starts OTIS? 
What program shows the first screen? Is it GTFORM, LOGUID, or something else?
How does a user get to the different menus like WIMENU, SPMENU, etc.?
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
