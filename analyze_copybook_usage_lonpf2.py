"""
Analyze copybook_usage index for LONPF2 to understand line number distribution.
"""
import os
import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'otis_rag'))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from config import Config

config = Config()

copy_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name=config.get_index_name("copybook_usage"),
    credential=AzureKeyCredential(config.search_key)
)

print("=" * 80)
print("ANALYZING COPYBOOK_USAGE INDEX FOR LONPF2")
print("=" * 80)

# Query all LONPF2 records
results = list(copy_client.search(
    search_text="*",
    filter="program_id eq 'LONPF2'",
    select=["copybook_name", "line_number", "line_start", "line_end", "paragraph_name"],
    order_by=["line_number asc"],
    top=1000
))

print(f"\nTotal copybook_usage records for LONPF2: {len(results)}")

if not results:
    print("\n❌ NO COPYBOOK_USAGE RECORDS FOUND!")
    print("\nThis explains why deterministic retrieval returns 0 copybooks,")
    print("which triggers the fallback to return ALL copybooks.")
    sys.exit(1)

# Analyze line number distribution
line_numbers = [r.get('line_number', 0) for r in results if r.get('line_number')]
print(f"\nLine number range: {min(line_numbers)} - {max(line_numbers)}")

# Check if RP transaction lines are covered
rp_ranges = [
    (2576, 2800),
    (2601, 2825),
    (5651, 5875),
    (9426, 9650)
]

print("\n" + "=" * 80)
print("CHECKING RP TRANSACTION LINE RANGES")
print("=" * 80)

for start, end in rp_ranges:
    in_range = [r for r in results if start <= r.get('line_number', 0) <= end]
    print(f"\nRange {start}-{end}: {len(in_range)} copybook usages")
    if in_range:
        copybooks = set(r.get('copybook_name', '') for r in in_range)
        print(f"  Unique copybooks: {len(copybooks)}")
        for cb in sorted(copybooks)[:10]:
            print(f"    - {cb}")

# Show distribution
print("\n" + "=" * 80)
print("COPYBOOK DISTRIBUTION (Top 20)")
print("=" * 80)

from collections import Counter
copybook_counts = Counter(r.get('copybook_name', '') for r in results)
for cb, count in copybook_counts.most_common(20):
    print(f"  {cb}: {count} usages")

print("\n" + "=" * 80)
print(f"TOTAL UNIQUE COPYBOOKS: {len(copybook_counts)}")
print("=" * 80)
