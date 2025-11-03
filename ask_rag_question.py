"""Ask the RAG a question and show the full response."""

from otis_rag.rag import OTISRAG

# Initialize RAG
rag = OTISRAG()

# Ask about a field
question = "What does the FB field do?"

print("=" * 80)
print("ASKING THE RAG")
print("=" * 80)
print(f"\nQuestion: {question}\n")
print("-" * 80)

# Get answer
answer = rag.ask(question, verbose=True)

print("\n" + "=" * 80)
print("ANSWER")
print("=" * 80)
print()
print(answer)
print()
