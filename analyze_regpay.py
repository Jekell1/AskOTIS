#!/usr/bin/env python3
"""Check REGPAY program details and how it's invoked."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load config
with open('local.settings.json', 'r') as f:
    config = json.load(f)['Values']

print("="*100)
print("REGPAY PROGRAM ANALYSIS")
print("="*100)

# Check in new-cobol-files
print("\n1. Checking new-cobol-files index...")
search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new-cobol-files',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(search_client.search(
    '',
    filter="programId eq 'REGPAY'",
    select=['programId', 'path', 'hasScreens', 'hasCICS', 'summary', 'definesNames'],
    top=5
))

if results:
    for r in results:
        print(f"\n   Program: {r.get('programId')}")
        print(f"   Path: {r.get('path')}")
        print(f"   Has Screens: {r.get('hasScreens')}")
        print(f"   Has CICS: {r.get('hasCICS')}")
        print(f"   Summary: {r.get('summary')}")
        defines = r.get('definesNames', [])
        if defines:
            print(f"   Defines: {', '.join(defines[:10])}")
else:
    print("   REGPAY not found in new-cobol-files")

# Check what REGPAY calls
print("\n2. What does REGPAY call?")
search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='new_cobol_calls',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(search_client.search(
    '',
    filter="caller_program eq 'REGPAY'",
    select=['callee_program', 'reference_type', 'category'],
    top=20
))

print(f"   REGPAY makes {len(results)} calls")
unique_callees = set()
for r in results[:20]:
    callee = r.get('callee_program')
    ref_type = r.get('reference_type')
    unique_callees.add(callee)
    print(f"   - Calls: {callee} ({ref_type})")

# Check program_deps
print("\n3. Checking program_deps...")
search_client = SearchClient(
    endpoint=config['SEARCH_ENDPOINT'],
    index_name='program_deps',
    credential=AzureKeyCredential(config['SEARCH_KEY'])
)

results = list(search_client.search(
    '',
    filter="program_name eq 'REGPAY'",
    select=['program_name', 'transaction_id', 'screen_name', 'calls_out', 'calls_in'],
    top=5
))

if results:
    for r in results:
        print(f"\n   Program: {r.get('program_name')}")
        print(f"   Transaction ID: {r.get('transaction_id')}")
        print(f"   Screen Name: {r.get('screen_name')}")
        
        calls_in = r.get('calls_in', [])
        if calls_in:
            print(f"   Called by: {', '.join(calls_in[:10])}")
        else:
            print(f"   Called by: (none - likely menu/transaction invoked)")
        
        calls_out = r.get('calls_out', [])
        if calls_out:
            print(f"   Calls out: {', '.join(calls_out[:10])}")
else:
    print("   REGPAY not found in program_deps")

print("\n" + "="*100)
print("CONCLUSION:")
print("="*100)
print("If REGPAY has no inbound calls, it's likely:")
print("1. Invoked by a CICS transaction code (e.g., user types 'REGP' at terminal)")
print("2. Called from a menu system via transaction ID")
print("3. A top-level entry point for payment processing")
print("="*100)
