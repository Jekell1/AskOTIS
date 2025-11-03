"""Find the specific programs called by APIPAY."""

from otis_rag import OTISRAG

rag = OTISRAG()

print("=" * 80)
print("FINDING PROGRAMS CALLED BY APIPAY")
print("=" * 80)
print()

# Ask more specifically about the call relationships
questions = [
    "Show me the CALL statements in APIPAY program",
    "What are the names of the programs that APIPAY calls?",
    "List all subprograms invoked by APIPAY",
]

for i, question in enumerate(questions, 1):
    print(f"\n{'=' * 80}")
    print(f"ATTEMPT {i}: {question}")
    print("=" * 80)
    
    answer = rag.ask(question, verbose=True)
    
    print("\nANSWER:")
    print("-" * 80)
    print(answer)
    print()
    
    # Check if we got actual program names
    if any(name in answer for name in ['CALL', 'program', 'subprogram']):
        print("✅ Got some call information")
    else:
        print("⚠️  Still looking for specific names...")
