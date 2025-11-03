#!/usr/bin/env python3
"""Find which file paths generate the LPMENU program_id."""

from pathlib import Path
from ingestion_common import SourceWalker, stable_hash

target_program_id = 'ea66581c142e6ba08a83d2dc773bc990db403c3c'

walker = SourceWalker('cobol_src')
matches = []

print("=" * 80)
print(f"SEARCHING FOR FILES THAT GENERATE: {target_program_id[:16]}...")
print("=" * 80)

for fp in walker.iter_files():
    # Normalize exactly as ingest_code_chunks.py does
    normalized = str(fp).replace('\\', '/').lower()
    program_id = stable_hash([normalized])
    
    if program_id == target_program_id:
        matches.append((fp, normalized, program_id))

print(f"\nFound {len(matches)} files:")
for fp, normalized, pid in matches:
    print(f"\n  File: {fp}")
    print(f"  Normalized: {normalized}")
    print(f"  Program ID: {pid}")

if len(matches) == 0:
    print("\n❌ No files found - this shouldn't happen!")
elif len(matches) == 1:
    print(f"\n✅ Only one file matches (expected)")
else:
    print(f"\n❌ HASH COLLISION: {len(matches)} files have the same program_id!")

print("\n" + "=" * 80)
