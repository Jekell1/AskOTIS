"""
Ask RAG to summarize APIPAY comments with moderate detail
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: Summarize APIPAY comments with moderate detail")
print("="*80)

question = """
Please summarize ALL comments from the APIPAY program with moderate detail.

I want to see:
1. Program identification (PROGRAM-ID, DATE-WRITTEN, AUTHOR if present)
2. Program description and purpose
3. Usage instructions and how to run it
4. Historical information - where logic was taken from, what programs it was based on
5. Business rules or processing notes
6. Any other important comments about functionality

Provide moderate detail - not just a brief summary, but not exhaustive either. 
Include actual comment quotes when they provide important context.

Organize the summary in a clear, structured way so I can understand what the program 
does and its history.
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
