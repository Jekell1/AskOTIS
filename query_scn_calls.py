"""
Query new_cobol_calls index to find which files call/copy SCN files.
"""
import json
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

print("Querying new_cobol_calls index for SCN file references...\n")

# First, get a sample document to see the fields
sample = client.search(search_text="*", top=1)
for doc in sample:
    print("Sample document fields:")
    for key in doc.keys():
        print(f"  {key}: {doc[key]}")
    print("\n")
    break

# Search for calls to files with SCN in the name
results = client.search(
    search_text="SCN",
    top=1000
)

# Organize results
scn_calls = {}
for result in results:
    source = result['source_file']
    target = result['target_name']
    call_type = result.get('call_type', 'unknown')
    
    if source not in scn_calls:
        scn_calls[source] = []
    
    scn_calls[source].append({
        'target': target,
        'type': call_type
    })

# Display results
print(f"Found {len(scn_calls)} source files that reference SCN files:\n")
print("=" * 80)

for source_file, calls in sorted(scn_calls.items()):
    print(f"\n{source_file}")
    print("-" * 80)
    for call in calls:
        print(f"  {call['type']:10s} -> {call['target']}")

print("\n" + "=" * 80)
print(f"\nTotal source files: {len(scn_calls)}")
print(f"Total SCN references: {sum(len(calls) for calls in scn_calls.values())}")

# Summary by call type
call_types = {}
for source_file, calls in scn_calls.items():
    for call in calls:
        call_type = call['type']
        if call_type not in call_types:
            call_types[call_type] = 0
        call_types[call_type] += 1

print("\nBreakdown by call type:")
for call_type, count in sorted(call_types.items()):
    print(f"  {call_type}: {count}")
