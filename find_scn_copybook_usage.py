"""
Query new_cobol_copybook_usage to find which files COPY screen (_SCN) files.
"""
import json
import re
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

# Load configuration
with open('local.settings.json') as f:
    config = json.load(f)['Values']

# Connect to Azure Search
client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_copybook_usage',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("Querying new_cobol_copybook_usage for _SCN.CPY file usage...\n")

# Search for copybooks with _SCN in the name
results = client.search(
    search_text="SCN",
    top=5000
)

# Organize results
scn_usage_by_program = {}
scn_usage_by_copybook = {}
count = 0

for result in results:
    using_program = result.get('using_program', 'unknown')
    using_file = result.get('using_file', 'unknown')
    copybook_name = result.get('copybook_name', 'unknown')
    copybook_path = result.get('copybook_path', 'unknown')
    
    # Only interested in _SCN copybooks
    if '_SCN' in copybook_name.upper() or '_SCN' in copybook_path.upper():
        count += 1
        
        # Track by program
        if using_file not in scn_usage_by_program:
            scn_usage_by_program[using_file] = {
                'program': using_program,
                'copybooks': []
            }
        scn_usage_by_program[using_file]['copybooks'].append(copybook_path or copybook_name)
        
        # Track by copybook
        copybook_key = copybook_path if copybook_path != 'unknown' else copybook_name
        if copybook_key not in scn_usage_by_copybook:
            scn_usage_by_copybook[copybook_key] = []
        scn_usage_by_copybook[copybook_key].append(using_file)

# Display results by program
print(f"Found {len(scn_usage_by_program)} programs that COPY _SCN files:\n")
print("=" * 120)

for using_file, info in sorted(scn_usage_by_program.items())[:30]:  # Show first 30
    print(f"\n{using_file}")
    print(f"  Program: {info['program']}")
    print("-" * 120)
    
    # Remove duplicates
    seen = set()
    for copybook in info['copybooks']:
        if copybook not in seen:
            seen.add(copybook)
            print(f"    COPY: {copybook}")

if len(scn_usage_by_program) > 30:
    print(f"\n... and {len(scn_usage_by_program) - 30} more programs")

# Display most used SCN copybooks
print("\n" + "=" * 120)
print("\nMost commonly used _SCN.CPY copybooks:")
print("-" * 120)
for copybook, programs in sorted(scn_usage_by_copybook.items(), key=lambda x: len(x[1]), reverse=True)[:30]:
    unique_programs = len(set(programs))
    print(f"{copybook:60s} used by {unique_programs:3d} programs")

# Check for LPCAMU specifically
print("\n" + "=" * 120)
print("\nLooking for LPCAMU_SCN.CPY specifically...")
lpcamu_found = False
for copybook, programs in scn_usage_by_copybook.items():
    if 'LPCAMU_SCN' in copybook.upper():
        lpcamu_found = True
        unique_programs = list(set(programs))
        print(f"  Found! '{copybook}' is used by {len(unique_programs)} program(s):")
        for prog in sorted(unique_programs):
            print(f"    {prog}")
        break

if not lpcamu_found:
    print("  LPCAMU_SCN.CPY is NOT referenced by any programs (orphaned file)")

print("\n" + "=" * 120)
print(f"\nTotal _SCN references: {count}")
print(f"Total unique _SCN copybooks: {len(scn_usage_by_copybook)}")
print(f"Total programs using _SCN files: {len(scn_usage_by_program)}")
