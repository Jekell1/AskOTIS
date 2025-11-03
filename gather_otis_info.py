"""Gather information about OTIS for the modernization book."""

from otis_rag import OTISRAG
import json

print("Initializing OTIS RAG system...")
rag = OTISRAG()

questions = [
    "What is the OTIS/OTOS system and what business problems does it solve?",
    "What are the main functional modules or capabilities of OTIS?",
    "How many COBOL programs are in the OTIS codebase?",
    "What are the main technology components used in OTIS?",
    "What are the common business transactions processed by OTIS?",
    "What are the key data structures and copybooks in OTIS?",
    "What are the main menu options in the OTIS system?",
    "What challenges does the current COBOL architecture present?",
]

results = {}

for i, question in enumerate(questions, 1):
    print(f"\n{'='*80}")
    print(f"Question {i}/{len(questions)}: {question}")
    print('='*80)
    
    answer = rag.ask(question, verbose=False)
    results[question] = answer
    
    print(answer[:500] + "..." if len(answer) > 500 else answer)
    print()

# Save to JSON for reference
with open('otis_book_research.json', 'w', encoding='utf-8') as f:
    json.dump(results, f, indent=2, ensure_ascii=False)

print("\n" + "="*80)
print("Research complete! Results saved to otis_book_research.json")
print("="*80)
