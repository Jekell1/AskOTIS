"""
Get PGMENU directly by key
"""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from dotenv import load_dotenv
import json

load_dotenv()

from otis_rag.config import Config
config = Config()

client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='new_cobol_menu_trees',
    credential=AzureKeyCredential(config.search_key)
)

print("="*80)
print("PGMENU - Main Menu (1 Level)")
print("="*80)

try:
    doc = client.get_document(key='PGMENU')
    
    root = doc.get('root_program_id')
    total_nodes = doc.get('total_nodes')
    max_depth = doc.get('max_depth')
    
    print(f"\nRoot Program: {root}")
    print(f"Total Nodes: {total_nodes}")
    print(f"Max Depth: {max_depth}")
    
    tree_json_str = doc.get('tree_json')
    tree = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
    
    children = tree.get('children', [])
    
    print(f"\n📋 PGMENU (Main Menu)")
    print(f"\nDirect Children: {len(children)}")
    print("-" * 80)
    
    for i, child in enumerate(children, 1):
        program_id = child.get('program_id', 'UNKNOWN')
        role = child.get('role', 'UNKNOWN')
        ui = child.get('ui', False)
        child_children = child.get('children', [])
        
        ui_marker = "📋" if ui else "🔧"
        has_children = f" → ({len(child_children)} children)" if child_children else " [leaf]"
        
        print(f"  {i:2}. {ui_marker} {program_id:<20} ({role:<10}){has_children}")
    
    print("\n" + "="*80)
    print("Legend:")
    print("📋 = UI/Menu program | 🔧 = Business logic")
    print("[leaf] = No children | → (N children) = Has child programs")

except Exception as e:
    print(f"Error: {e}")
    print("\nTrying to list what keys exist...")
    results = list(client.search(
        search_text='*',
        filter='total_nodes gt 5',
        top=10,
        select=['root_program_id', 'total_nodes']
    ))
    for r in results:
        print(f"  {r['root_program_id']}: {r['total_nodes']} nodes")
