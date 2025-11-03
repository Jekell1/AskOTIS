"""Quick test of the OTIS RAG system."""

from otis_rag import OTISRAG

print("Initializing OTIS RAG system...")
rag = OTISRAG()

print("\nAsking: 'What does the OTIS system do?'\n")
answer = rag.ask("What does the OTIS system do?", verbose=True)

print("\n" + "=" * 80)
print("ANSWER:")
print("=" * 80)
print(answer)
print("\n" + "=" * 80)
