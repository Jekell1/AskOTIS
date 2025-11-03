"""
Ask the RAG to provide a menu tree with descriptions
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: Menu tree with descriptions")
print("="*80)

question = """
Show me the same text-based menu tree structure from GTFORM down 2 levels, 
but this time add a brief description of what each menu and program does.

For each item, show:
- The program name
- A short description of its business purpose

Format:
GTFORM - [description]
  ├── LPMENU - [description]
  ├── PGMENU - [description]
  │   ├── GBMENU - [description]
  │   ├── LTMAIN - [description]
  │   etc.

Include descriptions for all the menus at level 1 (LPMENU, PGMENU, WIMENU, SPMENU) 
and their child programs at level 2.
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
