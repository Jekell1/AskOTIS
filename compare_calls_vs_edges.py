"""
Compare new_cobol_calls vs new_cobol_flow_edges_v2 to determine if redundant.
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

# Load credentials
with open('local.settings.json') as f:
    creds = json.load(f)['Values']

endpoint = creds['AZURE_SEARCH_ENDPOINT']
key = creds['AZURE_SEARCH_KEY']

print('=' * 80)
print('COMPARING: new_cobol_calls vs new_cobol_flow_edges_v2')
print('=' * 80)

# Analyze new_cobol_calls
print('\n--- INDEX: new_cobol_calls ---')
try:
    calls_client = SearchClient(endpoint, 'new_cobol_calls', AzureKeyCredential(key))
    
    # Get total count
    total = calls_client.search(search_text='*', top=0, include_total_count=True).get_count()
    print(f'Total documents: {total:,}')
    
    # Sample documents
    samples = list(calls_client.search(search_text='*', top=5))
    print(f'\nSample documents (first 5):')
    for i, doc in enumerate(samples, 1):
        print(f'\n  Document {i}:')
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            if isinstance(value, str) and len(value) > 150:
                print(f'    {key}: {value[:150]}...')
            elif isinstance(value, list) and len(value) > 5:
                print(f'    {key}: [{len(value)} items] {value[:5]}...')
            else:
                print(f'    {key}: {value}')
    
    # Get field names
    if samples:
        fields = [k for k in samples[0].keys() if not k.startswith('@')]
        print(f'\nFields in index: {", ".join(fields)}')
    
except Exception as e:
    print(f'Error: {e}')

# Analyze new_cobol_flow_edges_v2
print('\n\n--- INDEX: new_cobol_flow_edges_v2 ---')
try:
    edges_client = SearchClient(endpoint, 'new_cobol_flow_edges_v2', AzureKeyCredential(key))
    
    # Get total count
    total = edges_client.search(search_text='*', top=0, include_total_count=True).get_count()
    print(f'Total documents: {total:,}')
    
    # Sample documents
    samples = list(edges_client.search(search_text='*', top=5))
    print(f'\nSample documents (first 5):')
    for i, doc in enumerate(samples, 1):
        print(f'\n  Document {i}:')
        for key, value in doc.items():
            if key.startswith('@'):
                continue
            if isinstance(value, str) and len(value) > 150:
                print(f'    {key}: {value[:150]}...')
            elif isinstance(value, list) and len(value) > 5:
                print(f'    {key}: [{len(value)} items] {value[:5]}...')
            else:
                print(f'    {key}: {value}')
    
    # Get field names
    if samples:
        fields = [k for k in samples[0].keys() if not k.startswith('@')]
        print(f'\nFields in index: {", ".join(fields)}')
    
    # Get edge kind distribution
    print('\n\nEdge kind distribution (flow_edges_v2):')
    try:
        results = edges_client.search(
            search_text='*',
            facets=['edge_kind,count:100'],
            top=0
        )
        facets = results.get_facets()
        if 'edge_kind' in facets:
            for facet in facets['edge_kind'][:20]:
                print(f'  {facet["value"]}: {facet["count"]:,}')
    except Exception as e:
        print(f'  Could not get facets: {e}')
    
except Exception as e:
    print(f'Error: {e}')

# Compare edge types
print('\n\n' + '=' * 80)
print('COMPARISON ANALYSIS')
print('=' * 80)

print("""
Key Questions:
1. Does new_cobol_calls only track CALL statements?
2. Does new_cobol_flow_edges_v2 track CALL + PERFORM + other flow edges?
3. What is the scope difference?

Expected Findings:
- new_cobol_calls: Specific to CALL statements (program-to-program calls)
- new_cobol_flow_edges_v2: Comprehensive flow tracking:
  * CALL edges (program-to-program)
  * PERFORM edges (paragraph-to-paragraph)
  * COPY edges (copybook usage)
  * Data flow edges
  * Control flow edges

If new_cobol_calls ONLY has CALL statements and flow_edges_v2 has ALL of them
plus more, then calls is REDUNDANT.

If new_cobol_calls has unique data or different structure that's useful,
it should be KEPT.
""")

try:
    # Count CALL edges in flow_edges_v2
    call_edges_in_v2 = edges_client.search(
        search_text='*',
        filter="edge_kind eq 'call'",
        top=0,
        include_total_count=True
    ).get_count()
    
    calls_total = calls_client.search(search_text='*', top=0, include_total_count=True).get_count()
    
    print(f'\nDocument counts:')
    print(f'  new_cobol_calls: {calls_total:,}')
    print(f'  new_cobol_flow_edges_v2 (ALL types): {total:,}')
    print(f'  new_cobol_flow_edges_v2 (CALL type only): {call_edges_in_v2:,}')
    
    if call_edges_in_v2 >= calls_total * 0.9:  # Within 10%
        print(f'\n✅ VERDICT: new_cobol_calls is REDUNDANT')
        print(f'   flow_edges_v2 contains {call_edges_in_v2:,} CALL edges')
        print(f'   new_cobol_calls has {calls_total:,} documents')
        print(f'   Coverage: {call_edges_in_v2/calls_total*100:.1f}%')
        print(f'   Recommendation: DELETE new_cobol_calls (use flow_edges_v2 with filter)')
    else:
        print(f'\n⚠️ VERDICT: new_cobol_calls may have unique data')
        print(f'   Only {call_edges_in_v2:,} CALL edges in flow_edges_v2')
        print(f'   But {calls_total:,} documents in new_cobol_calls')
        print(f'   Difference: {calls_total - call_edges_in_v2:,}')
        print(f'   Need to investigate what the difference contains')
        
except Exception as e:
    print(f'\nError during comparison: {e}')
