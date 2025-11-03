"""
Check where copybooks are actually used in LONPF2 vs where RP transaction logic is.
"""
import os
import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'otis_rag'))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from config import Config

config = Config()

print("=" * 80)
print("LONPF2: COPYBOOK LOCATIONS VS RP TRANSACTION LOGIC")
print("=" * 80)

# Get copybook_usage records
copy_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name=config.get_index_name("copybook_usage"),
    credential=AzureKeyCredential(config.search_key)
)

results = list(copy_client.search(
    search_text="*",
    filter="program_id eq 'LONPF2'",
    select=["copybook_name", "line_number", "section", "paragraph_name", "raw_copy_line"],
    order_by=["line_number asc"],
    top=1000
))

print(f"\nTotal COPY statements: {len(results)}")

# Show line number distribution
line_nums = [r['line_number'] for r in results]
print(f"COPY statement lines: {min(line_nums)} - {max(line_nums)}")

# Show section distribution
from collections import Counter
sections = Counter(r.get('section', 'UNKNOWN') for r in results)
print("\nCOPY statements by section:")
for section, count in sections.most_common():
    print(f"  {section}: {count}")

print("\n" + "=" * 80)
print("RP TRANSACTION ENTRY POINTS (from code)")
print("=" * 80)
print("  Lines 2676-2700: IF LP-TRCD = \"RP\" logic")
print("  Lines 2701-2725: IF LP-TRCD = \"RP\" logic")
print("  Lines 5751-5775: IF LP-TRCD = \"RP\" logic")
print("  Lines 9526-9550: IF LP-TRCD = \"RP\" logic")

print("\n" + "=" * 80)
print("DIAGNOSIS")
print("=" * 80)
print("\n❌ COPY statements are NOT in the PROCEDURE DIVISION near RP logic!")
print("   They are in the DATA DIVISION (lines 852-9881)")
print("\nThe RP transaction logic USES data structures defined in those copybooks,")
print("but the COPY statements themselves are at the top of the program.")
print("\n💡 SOLUTION: Need to change strategy:")
print("   1. Return ALL copybooks for the program (current fallback)")
print("   2. OR: Analyze which data structures are REFERENCED in RP logic")
print("   3. OR: Use semantic search on the RP code sections to find relevant copybooks")
