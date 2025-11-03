#!/usr/bin/env python3
"""Check for programs/screens that call REGPAY using new_cobol_calls."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

print("="*100)
print("FINDING WHAT CALLS REGPAY (REVERSE DEPENDENCIES)")
print("="*100)

# Check new_cobol_calls for reverse lookup
print("\nSearching new_cobol_calls where callee_program = 'REGPAY'...")
search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_calls',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(search_client.search(
    '',
    filter="callee_program eq 'REGPAY'",
    select=['caller_program', 'reference_type', 'category', 'line', 'snippet'],
    top=500
))

print(f"\nFound {len(results)} call records where REGPAY is called\n")

# Group by caller
callers = {}
for r in results:
    caller = r.get('caller_program')
    ref_type = r.get('reference_type')
    category = r.get('category')
    
    if caller not in callers:
        callers[caller] = {
            'count': 0,
            'types': set(),
            'categories': set()
        }
    
    callers[caller]['count'] += 1
    if ref_type:
        callers[caller]['types'].add(ref_type)
    if category:
        callers[caller]['categories'].add(category)

print(f"Unique programs/screens that call REGPAY: {len(callers)}\n")
print("="*100)

# Display callers
for i, (caller, info) in enumerate(sorted(callers.items()), 1):
    types_str = ', '.join(sorted(info['types']))
    cats_str = ', '.join(sorted(info['categories']))
    print(f"{i}. {caller}")
    print(f"   Calls: {info['count']}, Types: {types_str}")
    if cats_str:
        print(f"   Categories: {cats_str}")
    print()

print("="*100)
print(f"SUMMARY: {len(callers)} programs/screens call REGPAY")
print("="*100)

# Show a few sample snippets
print("\nSample call snippets:")
for r in results[:5]:
    caller = r.get('caller_program')
    line = r.get('line')
    snippet = r.get('snippet', '')[:100]
    print(f"\n{caller} (line {line}):")
    print(f"  {snippet}...")
