"""Ask about the business purpose of the OTIS system."""

from otis_rag import OTISRAG

print("=" * 80)
print("ASKING ABOUT OTIS BUSINESS PURPOSE")
print("=" * 80)
print()

rag = OTISRAG()

# Try different business-focused questions
questions = [
    "What is the business purpose of the OTIS system?",
    "What business problems does OTIS solve?",
    "What industry or business domain is OTIS used in?",
]

for i, question in enumerate(questions, 1):
    print(f"\n{'=' * 80}")
    print(f"QUESTION {i}: {question}")
    print("=" * 80)
    
    answer = rag.ask(question, verbose=True)
    
    print("\nANSWER:")
    print("-" * 80)
    print(answer)
    print()
