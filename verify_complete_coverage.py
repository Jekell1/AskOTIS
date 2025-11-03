"""
Verify coverage by sampling actual files directly from the index.
"""
import os
import sys
from pathlib import Path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'otis_rag'))

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from config import Config

config = Config()
search_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name=config.get_index_name("code_chunks"),
    credential=AzureKeyCredential(config.search_key)
)

print("=" * 80)
print("COMPLETE COVERAGE VERIFICATION")
print("=" * 80)

# Get all source files
cobol_src = Path("cobol_src")
all_cbl = sorted(cobol_src.rglob("*.CBL"))
all_cpy = sorted(cobol_src.rglob("*.CPY"))

print(f"\nSource files:")
print(f"  .CBL files: {len(all_cbl):,}")
print(f"  .CPY files: {len(all_cpy):,}")
print(f"  Total: {len(all_cbl) + len(all_cpy):,}")

# Sample 100 random files to check
import random
sample_cbl = random.sample(all_cbl, min(50, len(all_cbl)))
sample_cpy = random.sample(all_cpy, min(50, len(all_cpy)))

print(f"\nChecking random sample:")
print(f"  50 CBL files")
print(f"  50 CPY files")

missing = []

for file_path in sample_cbl + sample_cpy:
    # Construct the expected path pattern
    if file_path.is_absolute():
        relative_path = file_path.relative_to(Path.cwd())
    else:
        relative_path = file_path
    path_str = str(relative_path).replace('\\', '\\')
    
    # Search for this file
    results = list(search_client.search(
        search_text="*",
        filter=f"path eq '{path_str}'",
        select=["path", "name"],
        top=1
    ))
    
    if not results:
        missing.append(str(relative_path))

print(f"\n{'=' * 80}")
print(f"SAMPLE RESULTS")
print(f"{'=' * 80}")
print(f"\nChecked: 100 files")
print(f"Missing: {len(missing)} files")

if missing:
    print(f"\n⚠️ Missing files in sample:")
    for f in missing[:20]:
        print(f"  - {f}")
    if len(missing) > 20:
        print(f"  ... and {len(missing) - 20} more")
    
    # Estimate total coverage
    missing_rate = len(missing) / 100
    estimated_missing = int((len(all_cbl) + len(all_cpy)) * missing_rate)
    estimated_coverage = 100 - (missing_rate * 100)
    
    print(f"\n{'=' * 80}")
    print(f"ESTIMATED COVERAGE: {estimated_coverage:.1f}%")
    print(f"{'=' * 80}")
    print(f"Estimated missing files: ~{estimated_missing:,}")
else:
    print(f"\n✅ ALL SAMPLED FILES ARE INDEXED!")
    print(f"\n{'=' * 80}")
    print(f"ESTIMATED COVERAGE: ~100%")
    print(f"{'=' * 80}")

# Check specific known files
print(f"\n{'=' * 80}")
print(f"CHECKING SPECIFIC FILES")
print(f"{'=' * 80}")

test_files = [
    "cobol_src\\LP\\LONPF2.CBL",
    "cobol_src\\LIBCL\\AUDITWW.CPY",
    "cobol_src\\CL\\ICUNR1.CBL",  # User's current file
    "cobol_src\\LIBCL\\ADDINS_DEF.CPY"
]

for file_path in test_files:
    results = list(search_client.search(
        search_text="*",
        filter=f"path eq '{file_path}'",
        select=["path", "chunk_id"],
        top=1000
    ))
    status = "✓" if results else "✗"
    print(f"{status} {file_path}: {len(results)} chunks")
