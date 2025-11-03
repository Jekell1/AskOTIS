#!/usr/bin/env python3
"""Sample 25 file descriptions from the index (mix of CBL and CPY)."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json
import random

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new-cobol-files',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

# Get all files
print("Fetching all files...")
all_files = list(search_client.search(
    '',
    select=['id', 'path', 'programId', 'kind', 'summary'],
    top=10000
))

print(f"Total files: {len(all_files)}")

# Separate by kind
programs = [f for f in all_files if f.get('kind') == 'program']
copybooks = [f for f in all_files if f.get('kind') == 'copybook']

print(f"Programs (.CBL): {len(programs)}")
print(f"Copybooks (.CPY): {len(copybooks)}")

# Sample 12 programs and 13 copybooks
sample_programs = random.sample(programs, min(12, len(programs)))
sample_copybooks = random.sample(copybooks, min(13, len(copybooks)))

# Combine and display
samples = sample_programs + sample_copybooks
random.shuffle(samples)

print("\n" + "="*100)
print("SAMPLE OF 25 FILE DESCRIPTIONS (Mix of Programs and Copybooks)")
print("="*100 + "\n")

for i, file in enumerate(samples, 1):
    program_id = file.get('programId', 'Unknown')
    path = file.get('path', 'Unknown')
    kind = file.get('kind', 'unknown')
    summary = file.get('summary', 'No description')
    
    # Determine file extension
    ext = '.CBL' if kind == 'program' else '.CPY'
    
    print(f"{i}. {program_id}{ext} ({kind.upper()})")
    print(f"   Path: {path}")
    print(f"   Description: {summary}")
    print()

print("="*100)
print(f"Total: {len(samples)} files sampled ({len(sample_programs)} programs, {len(sample_copybooks)} copybooks)")
print("="*100)
