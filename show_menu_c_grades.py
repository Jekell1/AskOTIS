import json

# Load test results
with open('rag_test_results_20251030_214653.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

# Find C-grade menu questions
c_grades = [q for q in data['results'] if q['grade'] == 'C']
menu_qs = [q for q in c_grades if 'MENU' in q['question'].upper() or 'option' in q['question'].lower()]

print("="*80)
print("MENU QUESTIONS THAT GOT C GRADES")
print("="*80)

for q in menu_qs:
    print(f"\n\n{'='*80}")
    print(f"Q{q['question_number']}: {q['question']}")
    print(f"{'='*80}")
    
    print(f"\n📊 Expected: {q['expected']}")
    
    print(f"\n🤖 RAG Answer:")
    answer = q['rag_answer']
    if len(answer) > 800:
        print(answer[:800] + "\n... [truncated]")
    else:
        print(answer)
    
    print(f"\n📝 Grade: {q['grade']}")
    print(f"Reasoning: {q['grading']['reasoning']}")
