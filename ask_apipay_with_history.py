"""
Ask RAG explicitly for historical comments from APIPAY
"""
import sys
sys.path.insert(0, '.')

from otis_rag import OTISRAG

rag = OTISRAG()

print("="*80)
print("ASKING RAG: APIPAY comments INCLUDING historical attribution")
print("="*80)

question = """
Summarize ALL comments from the APIPAY program including these specific sections:

1. IDENTIFICATION DIVISION (PROGRAM-ID, DATE-WRITTEN, AUTHOR)
2. Program description (DESC)
3. Usage instructions (how to run it)
4. **HISTORICAL COMMENTS** - specifically look for comments about:
   - "LOGIC WAS TAKEN FROM"
   - Which programs this was based on (REFUPD, LONPW9, OTHUPD, LONPWA, etc.)
   - Any comments about where the code came from
5. Return status and logging information
6. Transaction type codes (RP, PL, RE, OT, etc.)
7. Any other processing notes

For the historical section, I KNOW there are comments about logic being taken from 
other programs. Please find and include those specific comment lines.

Provide moderate detail with actual comment quotes.
"""

response = rag.ask(question, verbose=True)

print("\n" + "="*80)
print("RAG ANSWER:")
print("="*80)
print(response)
