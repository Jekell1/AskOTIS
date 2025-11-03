#!/usr/bin/env python3
"""
Find the actual program(s) called when LPMENU option 1 (DAILY PROCESSING) is selected.
"""

from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag import OTISRAG
import json

def main():
    rag = OTISRAG()
    
    # Check UI paths to see routing from LPMENU
    print("=" * 80)
    print("CHECKING UI PATHS FROM LPMENU")
    print("=" * 80)
    
    client = SearchClient(
        endpoint=rag.config.search_endpoint,
        index_name='new_cobol_ui_paths',
        credential=AzureKeyCredential(rag.config.search_key)
    )
    
    result = list(client.search('LPMENU', top=50))
    print(f"\nFound {len(result)} UI paths starting from LPMENU\n")
    
    # Group by second program in sequence (what LPMENU calls)
    next_programs = {}
    for doc in result:
        seq = json.loads(doc.get('program_sequence_json', '[]'))
        if len(seq) >= 2 and seq[0] == 'LPMENU':
            next_prog = seq[1]
            if next_prog not in next_programs:
                next_programs[next_prog] = []
            next_programs[next_prog].append(' -> '.join(seq[:5]))
    
    print(f"Programs called directly from LPMENU: {len(next_programs)}\n")
    for prog, paths in sorted(next_programs.items()):
        print(f"  {prog}: {len(paths)} paths")
        for path in paths[:3]:
            print(f"    - {path}")
        if len(paths) > 3:
            print(f"    ... and {len(paths) - 3} more")
        print()
    
    # Now check if there's a specific program for "DAILY PROCESSING"
    # Common patterns: DAILY, DPFILE, LPDAILY, etc.
    print("\n" + "=" * 80)
    print("CHECKING FOR DAILY PROCESSING PROGRAMS")
    print("=" * 80)
    
    daily_candidates = [p for p in next_programs.keys() if 'DAILY' in p.upper() or 'DP' in p.upper()]
    print(f"\nCandidates with 'DAILY' or 'DP' in name: {daily_candidates}\n")
    
    # Check program_deps for these candidates
    deps_client = SearchClient(
        endpoint=rag.config.search_endpoint,
        index_name='new_cobol_program_deps',
        credential=AzureKeyCredential(rag.config.search_key)
    )
    
    for candidate in daily_candidates[:5]:
        result = list(deps_client.search(f'program_id:{candidate}', top=1))
        if result:
            doc = result[0]
            prog_id = doc.get('program_id')
            copybooks = json.loads(doc.get('copybooks_used_json', '[]'))
            calls_out = json.loads(doc.get('calls_out_json', '[]'))
            
            print(f"Program: {prog_id}")
            print(f"  Copybooks used: {len(copybooks)}")
            if copybooks:
                for cb in copybooks[:10]:
                    print(f"    - {cb}")
                if len(copybooks) > 10:
                    print(f"    ... and {len(copybooks) - 10} more")
            print(f"  Calls out to: {len(calls_out)} programs")
            if calls_out:
                for call in calls_out[:10]:
                    print(f"    - {call}")
                if len(calls_out) > 10:
                    print(f"    ... and {len(calls_out) - 10} more")
            print()

if __name__ == '__main__':
    main()
