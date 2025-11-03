import json

with open('rag_test_results_20251030_214653.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

print("="*80)
print("RAG TEST RESULTS - DETAILED ANALYSIS")
print("="*80)

summary = data.get('summary_statistics', {})
print(f"\n📊 Overall Performance:")
print(f"   Total Questions: {summary.get('total_questions', 0)}")
print(f"   Passing Rate: {summary.get('passing_rate', 0):.1f}%")
print(f"   Average GPA: {summary.get('average_gpa', 0):.2f}/4.0")

print(f"\n📈 Grade Distribution:")
grade_dist = summary.get('grade_distribution', {})
for grade in ['A', 'B', 'C', 'D', 'F']:
    count = grade_dist.get(grade, 0)
    total = summary.get('total_questions', 0)
    pct = (count / total * 100) if total > 0 else 0
    bar = '█' * int(pct / 2)
    print(f"   {grade}: {count:3} ({pct:5.1f}%) {bar}")

# Get unique C-grade questions
results = data.get('detailed_results', [])
c_questions = {}
for r in results:
    if r.get('grade') == 'C':
        qnum = r['question_number']
        if qnum not in c_questions:
            c_questions[qnum] = r

print(f"\n{'='*80}")
print(f"🔍 C-GRADE QUESTIONS ({len(c_questions)} unique)")
print("="*80)

# Group by category
categories = {}
for qnum, r in sorted(c_questions.items()):
    question = r['question']
    
    # Categorize
    if 'copybook' in question.lower() and 'fields' in question.lower():
        cat = "1. Copybook Fields"
    elif 'copybook' in question.lower() and 'file description' in question.lower():
        cat = "2. File Descriptions"
    elif 'MOVE CORRESPONDING' in question:
        cat = "3. Code Patterns"
    elif 'menu' in question.lower() or 'option' in question.lower():
        cat = "4. Menu Options"
    else:
        cat = "5. Other"
    
    if cat not in categories:
        categories[cat] = []
    categories[cat].append((qnum, r))

for cat in sorted(categories.keys()):
    print(f"\n{cat}:")
    print("-" * 80)
    for qnum, r in categories[cat]:
        print(f"\n  Q{qnum}: {r['question']}")
        print(f"  Expected: {r.get('expected', 'N/A')}")
        reasoning = r.get('reasoning', '')
        if reasoning:
            # Show first sentence of reasoning
            first_sentence = reasoning.split('.')[0] + '.'
            print(f"  Issue: {first_sentence}")

print(f"\n{'='*80}")
print("💡 RECOMMENDATIONS")
print("="*80)
print("""
Priority 1 (HIGH - Fixes 3 questions): Menu Option Text Extraction
  - Extract menu display text from COBOL programs
  - Update menu_trees index with option descriptions
  - Expected improvement: 10% → 7% C-grade rate

Priority 2 (MEDIUM - Fixes 2-3 questions): Copybook Field Indexing
  - Parse .CPY files for field definitions
  - Add to copybook_meta index
  - NOTE: Q41/Q45 ask about non-existent copybooks (test issue)

Priority 3 (LOW - Fixes 1 question): Code Pattern Detection
  - Add COBOL statement type indexing
  - Enable pattern-based search
  
Priority 4 (MEDIUM - Fixes 1 question): FD Copybook Tagging
  - Identify file description copybooks
  - Tag in metadata

Estimated Impact: 10% → 4-5% C-grade rate (86% → 93% A+B)
""")
