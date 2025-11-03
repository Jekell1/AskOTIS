import json

with open('rag_test_results_20251030_214653.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

print("="*80)
print("DETAILED TEST RESULTS INVESTIGATION")
print("="*80)

results = data.get('detailed_results', [])

print(f"\nTotal entries in detailed_results: {len(results)}")

# Check for duplicates
question_numbers = [r.get('question_number') for r in results]
unique_qnums = set(question_numbers)

print(f"Unique question numbers: {len(unique_qnums)}")
print(f"Duplicates: {len(results) - len(unique_qnums)}")

# Count actual grades
grade_counts = {}
for r in results:
    grade = r.get('grade')
    grade_counts[grade] = grade_counts.get(grade, 0) + 1

print("\nActual Grade Counts from detailed_results:")
for grade in ['A', 'B', 'C', 'D', 'F']:
    count = grade_counts.get(grade, 0)
    print(f"  {grade}: {count}")

# Check if there are failed questions (D or F)
failed = [r for r in results if r.get('grade') in ['D', 'F']]
print(f"\n❌ D or F grades: {len(failed)}")

if failed:
    print("\nFailed Questions:")
    for r in failed[:10]:  # Show first 10
        print(f"\nQ{r.get('question_number')}: {r.get('question', 'N/A')}")
        print(f"  Grade: {r.get('grade')}")
        print(f"  Reason: {r.get('reasoning', 'N/A')[:200]}")

# Check summary statistics too
summary = data.get('summary_statistics', {})
print("\n" + "="*80)
print("SUMMARY STATISTICS SECTION:")
print("="*80)
print(json.dumps(summary, indent=2))
