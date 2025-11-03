"""
Ask RAG to summarize comments from APIPAY program
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: Summarize all comments from APIPAY")
print("="*80)

question = """
Please find and summarize ALL comments from the APIPAY program. 

I want to see:
1. The IDENTIFICATION DIVISION comments (AUTHOR, DATE-WRITTEN, REMARKS, DESCRIPTION)
2. Any header or banner comments explaining what the program does
3. Inline comments that explain business logic or history
4. Any comments about modifications, changes, or updates over time
5. Comments about business rules or special handling

Show me the actual comment text and organize it so I can understand the program's 
history and purpose from the comments alone.
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
