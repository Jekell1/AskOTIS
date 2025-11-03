import json

# Load test results
with open('rag_test_results_20251030_214653.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

print("="*80)
print("TEST RESULTS SUMMARY")
print("="*80)

# Get summary statistics
summary = data.get('summary_statistics', {})
print(f"\nTotal Questions: {summary.get('total_questions', 0)}")
print(f"Test Date: {data.get('test_metadata', {}).get('test_date', 'Unknown')}")

print("\nGrade Distribution:")
grade_dist = summary.get('grade_distribution', {})
for grade in ['A', 'B', 'C', 'D', 'F']:
    count = grade_dist.get(grade, 0)
    pct = summary.get('grade_percentages', {}).get(grade, 0)
    print(f"  {grade}: {count:3} ({pct:5.1f}%)")

print(f"\nPassing Rate: {summary.get('passing_rate', 0):.1f}%")
print(f"Average GPA: {summary.get('average_gpa', 0):.2f}")

# Get detailed results
results = data.get('detailed_results', [])

# Show C, D, F questions
failures = [r for r in results if r.get('grade') in ['C', 'D', 'F']]

if failures:
    print(f"\n{'='*80}")
    print(f"QUESTIONS NEEDING IMPROVEMENT ({len(failures)} total)")
    print("="*80)
    
    for r in failures:
        print(f"\nQ{r['question_number']}: {r['question']}")
        print(f"  Grade: {r['grade']}")
        expected = r.get('expected', '')
        if len(expected) > 80:
            print(f"  Expected: {expected[:80]}...")
        else:
            print(f"  Expected: {expected}")
        
        reasoning = r.get('reasoning', r.get('grading', {}).get('reasoning', ''))
        if len(reasoning) > 150:
            print(f"  Issue: {reasoning[:150]}...")
        elif reasoning:
            print(f"  Issue: {reasoning}")
