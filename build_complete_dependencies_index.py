"""
Build new_cobol_complete_dependencies index with pre-computed dependency trees.

For each program, compute ALL transitive dependencies (copybooks + programs).
This makes dependency questions answerable via simple RAG lookup.
"""
import json
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from collections import deque

def load_config():
    with open('local.settings.json', 'r') as f:
        return json.load(f).get('Values', {})

def get_all_dependencies(program_id: str, copybook_client, deps_client, max_depth=10):
    """
    Recursively get ALL dependencies for a program.
    
    Returns:
        {
            'programs': set of all programs in call tree,
            'copybooks': set of all copybooks used,
            'depth': max depth reached
        }
    """
    all_programs = set()
    all_copybooks = set()
    visited = set()
    queue = deque([(program_id, 0)])
    max_depth_reached = 0
    
    while queue:
        current_prog, depth = queue.popleft()
        
        if depth > max_depth or current_prog in visited:
            continue
        
        visited.add(current_prog)
        all_programs.add(current_prog)
        max_depth_reached = max(max_depth_reached, depth)
        
        # Get copybooks for this program
        try:
            cb_results = copybook_client.search(
                search_text='*',
                filter=f"program_id eq '{current_prog}'",
                top=100
            )
            for doc in cb_results:
                cb = doc.get('copybook_name') or doc.get('copybook_id')
                if cb:
                    all_copybooks.add(cb)
        except:
            pass
        
        # Get programs called by this program
        try:
            dep_results = deps_client.search(
                search_text='*',
                filter=f"program_id eq '{current_prog}'",
                top=1
            )
            for doc in dep_results:
                called_progs = doc.get('calls_programs', []) or []
                for prog in called_progs:
                    if prog not in visited:
                        queue.append((prog, depth + 1))
        except:
            pass
    
    return {
        'programs': all_programs,
        'copybooks': all_copybooks,
        'depth': max_depth_reached,
        'total_files': len(all_programs) + len(all_copybooks)
    }

def main():
    print("="*80)
    print("BUILD new_cobol_complete_dependencies INDEX")
    print("="*80)
    
    config = load_config()
    endpoint = config['AZURE_SEARCH_ENDPOINT']
    key = config['AZURE_SEARCH_KEY']
    
    # Clients for querying
    copybook_client = SearchClient(endpoint, 'new_cobol_copybook_usage', AzureKeyCredential(key))
    deps_client = SearchClient(endpoint, 'new_cobol_program_deps', AzureKeyCredential(key))
    meta_client = SearchClient(endpoint, 'new_cobol_program_meta', AzureKeyCredential(key))
    
    # Get all programs
    print("\n📊 Fetching all programs...")
    all_programs = []
    results = meta_client.search(search_text='*', select=['program_id'], top=10000)
    for doc in results:
        all_programs.append(doc['program_id'])
    
    print(f"   Found {len(all_programs)} programs")
    
    # Compute dependencies for each
    print(f"\n🔄 Computing dependencies for each program...")
    
    dependency_docs = []
    
    for i, prog_id in enumerate(all_programs[:10], 1):  # Start with first 10 for testing
        print(f"   {i}/{len(all_programs[:10])}: {prog_id}", end='')
        
        deps = get_all_dependencies(prog_id, copybook_client, deps_client)
        
        doc = {
            'id': prog_id,
            'program_id': prog_id,
            'all_programs': list(deps['programs']),
            'all_copybooks': list(deps['copybooks']),
            'program_count': len(deps['programs']),
            'copybook_count': len(deps['copybooks']),
            'total_file_count': deps['total_files'],
            'max_dependency_depth': deps['depth'],
            'doc_type': 'complete_dependencies',
            'generated_at': '2025-10-30T00:00:00Z'
        }
        
        dependency_docs.append(doc)
        print(f" ✅ ({deps['total_files']} files)")
    
    print(f"\n✅ Computed dependencies for {len(dependency_docs)} programs")
    
    # For now, just show sample
    print("\n📋 SAMPLE: SPMENU dependencies")
    print("-"*80)
    for doc in dependency_docs:
        if doc['program_id'] == 'SPMENU':
            print(f"Programs: {doc['program_count']}")
            print(f"Copybooks: {doc['copybook_count']}")
            print(f"Total files: {doc['total_file_count']}")
            print(f"Max depth: {doc['max_dependency_depth']}")
            print(f"\nPrograms: {', '.join(list(doc['all_programs'])[:10])}...")
            print(f"\nCopybooks: {', '.join(list(doc['all_copybooks'])[:10])}...")
    
    print("\n" + "="*80)
    print("💡 TO COMPLETE:")
    print("   1. Create the index schema")
    print("   2. Process all programs (not just first 10)")
    print("   3. Upload to Azure Search")
    print("   4. Add to RAG context")
    print("="*80)

if __name__ == "__main__":
    main()
