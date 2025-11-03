"""Simple check for MAX=3 chunk"""
from otis_rag.rag import OTISRAG
from otis_rag.retriever import HybridRetriever

rag = OTISRAG()
retriever = HybridRetriever(rag.config)

query = 'What fields can a user modify in REGPAY?'
routing = rag.router.route(query)
results = retriever.retrieve(query, routing['search_indexes'], max_results=100)

print(f'Total results: {len(results)}')
print('\nSearching for MAX=3 definition chunk...')

found = False
for i, doc in enumerate(results):
    text = doc.get('text', '')
    if 'MAX' in text and 'VALUE 3' in text and 'PIC 99' in text:
        print(f'\nFOUND at position {i+1}!')
        print(f'  Index: {doc.get("_index_type")}')
        print(f'  Score: {doc.get("@search.score", 0):.4f}')
        print(f'  Program: {doc.get("program_id", "N/A")}')
        lines = text.split('\n')
        print(f'\n  Content preview:')
        for j, line in enumerate(lines[:10]):
            print(f'    {line.strip()[:100]}')
        found = True
        break

if not found:
    print('\nNOT FOUND in any of the retrieved documents')
    print('Checking if any code_new results exist...')
    code_results = [r for r in results if r.get('_index_type') == 'code_new']
    print(f'  Found {len(code_results)} code_new results')
    if code_results:
        print('\n  Top 3 code_new results:')
        for i, r in enumerate(code_results[:3], 1):
            preview = r.get('text', '')[:200]
            print(f'\n  #{i}: Score={r.get("@search.score", 0):.4f}')
            print(f'      {preview}...')
