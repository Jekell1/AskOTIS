"""Search the actual APIPAY source code for CALL statements."""

from otis_rag import OTISRAG

rag = OTISRAG()

print("=" * 80)
print("SEARCHING APIPAY SOURCE CODE FOR CALL STATEMENTS")
print("=" * 80)
print()

# Search for actual CALL statements in the source code
question = "Show me the source code from APIPAY.CBL that contains CALL statements"

print(f"Question: {question}")
print("=" * 80)

answer = rag.ask(question, verbose=True)

print("\nANSWER:")
print("-" * 80)
print(answer)
