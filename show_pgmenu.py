"""
Show PGMENU tree structure (1 level)
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
print("PGMENU - Main Menu (1 Level Deep)")
print("="*80)

# Get PGMENU specifically
results = list(client.search(
    search_text='PGMENU',
    top=5,
    select=['root_program_id', 'tree_json', 'max_depth', 'total_nodes']
))

print(f"\nFound {len(results)} results for PGMENU")

for idx, tree in enumerate(results):
    root = tree.get('root_program_id', 'N/A')
    total_nodes = tree.get('total_nodes', 0)
    max_depth = tree.get('max_depth', 0)
    
    print(f"\nResult {idx + 1}: {root} ({total_nodes} nodes, depth {max_depth})")
    
    if total_nodes > 0:
        tree_json_str = tree.get('tree_json', '{}')
        
        try:
            if isinstance(tree_json_str, str):
                tree_data = json.loads(tree_json_str)
            else:
                tree_data = tree_json_str
            
            print(f"\n📋 {root} (root)")
            
            children = tree_data.get('children', [])
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
        
        except Exception as e:
            print(f"Error: {e}")
