"""Show which indexes have the best expansion opportunities."""
import json

data = json.load(open('comprehensive_partial_coverage_analysis.json'))
results = sorted(data['results'], key=lambda x: x.get('coverage_pct', 0), reverse=True)

print("=" * 80)
print("EXPANSION OPPORTUNITIES - Sorted by Current Coverage")
print("=" * 80)
print(f"\n{'Index':<30} {'Current':>8} {'Have':>6} {'Missing':>8} {'Potential':>10}")
print("-" * 80)

for r in results:
    if r.get('programs_missing', 0) > 0:
        idx = r['index'].replace('new_cobol_', '')
        current = r['coverage_pct']
        have = r['programs_with_data']
        missing = r['programs_missing']
        potential_gain = (missing / 96.78) if missing > 0 else 0
        
        print(f"{idx:<30} {current:>7.1f}% {have:>6} {missing:>8} (+{potential_gain:>6.1f}%)")

print("\n" + "=" * 80)
print("RECOMMENDATION: Focus on indexes with highest current coverage")
print("=" * 80)
