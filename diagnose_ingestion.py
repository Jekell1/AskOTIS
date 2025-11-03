#!/usr/bin/env python3
"""Diagnose why ingestion is missing chunks."""

import sys
from pathlib import Path
from collections import Counter
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load settings
cfg = json.load(open('local.settings.json'))['Values']
client = SearchClient(
    cfg['AZURE_SEARCH_ENDPOINT'],
    'new_code_chunks',
    AzureKeyCredential(cfg['AZURE_SEARCH_KEY'])
)

print("=" * 80)
print("INGESTION DIAGNOSTIC")
print("=" * 80)

# Count files that should be ingested
cobol_src = Path("cobol_src")
extensions = (".cbl", ".cob", ".cpy", ".copy")
files = [f for f in cobol_src.rglob("*") if f.suffix.lower() in extensions]

print(f"\n📁 Files on disk: {len(files)}")
for ext in extensions:
    count = len([f for f in files if f.suffix.lower() == ext])
    print(f"   {ext}: {count}")

# Estimate chunks (assuming ~25 lines per chunk window)
total_lines = 0
for f in files:
    try:
        with open(f, 'r', encoding='utf-8', errors='ignore') as fh:
            total_lines += len(fh.readlines())
    except:
        pass

estimated_chunks = total_lines // 25
print(f"\n📊 Total lines: {total_lines:,}")
print(f"📊 Estimated chunks (window=25): {estimated_chunks:,}")

# Check index
index_count = client.get_document_count()
print(f"\n💾 Chunks in index: {index_count:,}")

missing = estimated_chunks - index_count
missing_pct = (missing / estimated_chunks * 100) if estimated_chunks > 0 else 0
print(f"❌ Missing chunks: {missing:,} ({missing_pct:.1f}%)")

# Check unique program_ids
print(f"\n🔍 Checking unique program_ids in index...")
results = client.search("*", select="program_id", top=1000)
program_ids = set()
for doc in results:
    program_ids.add(doc['program_id'])

print(f"   Unique program_ids (sample): {len(program_ids)}")

# Check for LPMENU specifically
print(f"\n🔍 Checking LPMENU_SCN.CPY...")
lpmenu_file = cobol_src / "LIBLP" / "LPMENU_SCN.CPY"
if lpmenu_file.exists():
    with open(lpmenu_file, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    print(f"   File exists: {lpmenu_file}")
    print(f"   Total lines: {len(lines)}")
    print(f"   Expected chunks (window=25): {(len(lines) + 24) // 25}")
    
    # Calculate normalized program_id using stable_hash
    from ingestion_common import stable_hash
    normalized_path = str(lpmenu_file).replace('\\', '/').lower()
    program_id = stable_hash([normalized_path])
    print(f"   Normalized path: {normalized_path}")
    print(f"   Expected program_id: {program_id}")
    
    # Check chunks in index
    results = client.search(f'program_id:{program_id}', search_mode='all', select='chunk_id,start_line,end_line', top=20)
    chunks = list(results)
    print(f"   Chunks in index: {len(chunks)}")
    for chunk in chunks:
        print(f"      Lines {chunk['start_line']}-{chunk['end_line']}")
    
    # Check if first chunk (1-25) exists
    has_first_chunk = any(c['start_line'] == 1 for c in chunks)
    if not has_first_chunk:
        print(f"   ❌ Missing first chunk (lines 1-25) which contains 'MASTER MENU'")
    else:
        print(f"   ✅ First chunk present")

print("\n" + "=" * 80)
