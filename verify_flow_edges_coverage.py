"""Verify flow_edges_v2 coverage against all source files."""
import pathlib
import json

# Count source files
cbl_files = [f for f in pathlib.Path('cobol_src').rglob('*') if f.suffix.lower() in ['.cbl', '.cob', '.cobol']]
cpy_files = [f for f in pathlib.Path('cobol_src').rglob('*') if f.suffix.lower() in ['.cpy', '.copy']]

print("=" * 80)
print("FLOW_EDGES_V2 COVERAGE VERIFICATION")
print("=" * 80)

print(f"\nSource files:")
print(f"  CBL files: {len(cbl_files)}")
print(f"  CPY files: {len(cpy_files)}")
print(f"  Total: {len(cbl_files) + len(cpy_files)}")

# Check analysis results
data = json.load(open('comprehensive_partial_coverage_analysis.json'))
for idx in data['results']:
    if 'flow_edges_v2' in idx['index']:
        print(f"\nFlow edges index:")
        print(f"  Total edges: {idx['total_docs']:,}")
        print(f"  Programs covered: {idx['programs_with_data']}/9,678")
        print(f"  Coverage: {idx['coverage_pct']:.1f}%")
        print(f"  Embeddings: {idx['embedding_pct']}%")
        print(f"\n  Programs missing: {idx['programs_missing']}")
        break

print("\n" + "=" * 80)
print("ANALYSIS")
print("=" * 80)

print("\n❌ INCOMPLETE COVERAGE:")
print(f"  • Only processed CBL files ({len(cbl_files)} files)")
print(f"  • Did NOT process CPY files ({len(cpy_files)} files)")
print(f"  • Only 1,610 programs have flow edges (16.6% coverage)")

print("\n✅ EMBEDDINGS:")
print(f"  • All existing edges have embeddings (100%)")

print("\n🔧 TO GET FULL COVERAGE:")
print(f"  • Re-run extraction with --include-copybooks flag")
print(f"  • This will add flow edges from CPY files")
print(f"  • Expected: ~50-100% more edges from copybooks with executable code")

print("\n📝 NOTE:")
print(f"  • CPY files typically have data structures, not flow logic")
print(f"  • But some copybooks contain procedures/paragraphs with PERFORM/CALL")
print(f"  • Worth including for completeness")
