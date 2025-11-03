"""
List all available menu trees
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv

load_dotenv()

from otis_rag.config import Config
config = Config()

client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config.search_key)
)

print("="*80)
print("Available Menu Trees (with 5+ nodes)")
print("="*80)

results = list(client.search(
    search_text='*',
    filter='total_nodes gt 5',
    top=50,
    select=['root_program_id', 'total_nodes', 'max_depth'],
    order_by=['total_nodes desc']
))

print(f"\nFound {len(results)} menu trees:\n")
print(f"{'Program':<20} {'Nodes':<8} {'Depth':<8}")
print("-" * 40)

for r in results[:20]:
    print(f"{r['root_program_id']:<20} {r['total_nodes']:<8} {r['max_depth']:<8}")
