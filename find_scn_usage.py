"""
Query new_cobol_calls index to find which files COPY screen (_SCN) files.
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
    index_name='new_cobol_calls',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

print("Querying new_cobol_calls index for _SCN.CPY file references...\n")

# Search for any calls with _SCN in them
results = client.search(
    search_text="SCN",
    top=5000
)

# Organize results
scn_copies = {}
count = 0

for result in results:
    source = result.get('file_path', 'unknown')
    caller = result.get('caller_program', 'unknown')
    snippet = result.get('snippet', '')
    line = result.get('line', 0)
    call_type = result.get('call_type', 'unknown')
    
    # Extract SCN filename from snippet
    scn_match = re.search(r'COPY\s+"([^"]*_SCN[^"]*\.CPY)"', snippet, re.IGNORECASE)
    if scn_match:
        scn_file = scn_match.group(1)
        
        key = f"{source}"
        if key not in scn_copies:
            scn_copies[key] = {
                'caller': caller,
                'scn_files': []
            }
        
        scn_copies[key]['scn_files'].append({
            'file': scn_file,
            'line': line
        })
        count += 1

# Display results
print(f"Found {len(scn_copies)} source files that COPY _SCN.CPY files:\n")
print("=" * 100)

for source_path, info in sorted(scn_copies.items()):
    print(f"\n{source_path}")
    print(f"  Program: {info['caller']}")
    print("-" * 100)
    
    # Remove duplicates
    seen = set()
    for scn_info in info['scn_files']:
        scn_key = f"{scn_info['file']}@{scn_info['line']}"
        if scn_key not in seen:
            seen.add(scn_key)
            print(f"    Line {scn_info['line']:4d}: COPY \"{scn_info['file']}\"")

print("\n" + "=" * 100)
print(f"\nTotal source files: {len(scn_copies)}")
print(f"Total _SCN.CPY references: {count}")

# Group by SCN file to see which are most used
scn_usage = {}
for source_path, info in scn_copies.items():
    for scn_info in info['scn_files']:
        scn_file = scn_info['file']
        if scn_file not in scn_usage:
            scn_usage[scn_file] = []
        scn_usage[scn_file].append(source_path)

print("\n" + "=" * 100)
print("\nMost commonly used _SCN.CPY files:")
print("-" * 100)
for scn_file, sources in sorted(scn_usage.items(), key=lambda x: len(x[1]), reverse=True)[:20]:
    print(f"{scn_file:40s} used by {len(sources):3d} programs")

# Check for LPCAMU specifically
print("\n" + "=" * 100)
print("\nLooking for LPCAMU_SCN.CPY specifically...")
if 'LIBLP/LPCAMU_SCN.CPY' in scn_usage:
    print(f"  Found! Used by {len(scn_usage['LIBLP/LPCAMU_SCN.CPY'])} programs:")
    for source in scn_usage['LIBLP/LPCAMU_SCN.CPY']:
        print(f"    {source}")
else:
    print("  LPCAMU_SCN.CPY is NOT referenced by any programs (orphaned file)")
