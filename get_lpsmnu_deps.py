#!/usr/bin/env python3
"""Get complete dependencies for LPSMNU."""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag import OTISRAG
import json

def get_program_deps(client, program_id, visited=None):
    """Recursively get all dependencies for a program."""
    if visited is None:
        visited = set()
    
    if program_id in visited:
        return {'copybooks': set(), 'programs': set(), 'calls': []}
    
    visited.add(program_id)
    
    # Search for this program
    result = list(client.search(f'{program_id}', top=1))
    if not result:
        return {'copybooks': set(), 'programs': {program_id}, 'calls': []}
    
    doc = result[0]
    if doc.get('program_id') != program_id:
        # Not an exact match
        return {'copybooks': set(), 'programs': {program_id}, 'calls': []}
    
    # Get direct dependencies
    copybooks = set(json.loads(doc.get('copybooks_used_json', '[]')))
    calls_out = json.loads(doc.get('calls_out_json', '[]'))
    
    all_copybooks = copybooks.copy()
    all_programs = {program_id}
    call_tree = [(program_id, calls_out)]
    
    # Recursively get dependencies for called programs
    for called_prog in calls_out:
        child_deps = get_program_deps(client, called_prog, visited)
        all_copybooks.update(child_deps['copybooks'])
        all_programs.update(child_deps['programs'])
        call_tree.extend(child_deps['calls'])
    
    return {
        'copybooks': all_copybooks,
        'programs': all_programs,
        'calls': call_tree
    }

def main():
    rag = OTISRAG()
    
    client = SearchClient(
        endpoint=rag.config.search_endpoint,
        index_name='new_cobol_program_deps',
        credential=AzureKeyCredential(rag.config.search_key)
    )
    
    print("=" * 80)
    print("COMPLETE RECURSIVE DEPENDENCIES FOR LPSMNU")
    print("(Program called by LPMENU option 1 - DAILY PROCESSING)")
    print("=" * 80)
    print()
    
    deps = get_program_deps(client, 'LPSMNU')
    
    print(f"PROGRAMS IN CALL TREE ({len(deps['programs'])}):")
    for i, prog in enumerate(sorted(deps['programs']), 1):
        print(f"  {i}. {prog}.CBL")
    
    print(f"\nCOPYBOOKS USED ({len(deps['copybooks'])}):")
    for i, cb in enumerate(sorted(deps['copybooks']), 1):
        print(f"  {i}. {cb}")
    
    print(f"\nCALL TREE:")
    for prog, calls in deps['calls']:
        if calls:
            print(f"  {prog} calls:")
            for called in calls[:15]:
                print(f"    - {called}")
            if len(calls) > 15:
                print(f"    ... and {len(calls) - 15} more")

if __name__ == '__main__':
    main()
