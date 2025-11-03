"""
Show top-level menu trees (1 level deep only)
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
print("Top-Level Menu Trees (1 Level Deep)")
print("="*80)

# Search for PGMENU specifically (main menu)
results = client.search(
    search_text='PGMENU',
    filter="root_program_id eq 'PGMENU'",
    top=1,
    select=['root_program_id', 'tree_json', 'max_depth', 'total_nodes']
)

for tree in results:
    root = tree.get('root_program_id', 'N/A')
    max_depth = tree.get('max_depth', 0)
    total_nodes = tree.get('total_nodes', 0)
    
    print(f"\n{'='*80}")
    print(f"Menu: {root}")
    print("="*80)
    print(f"Total Nodes in Full Tree: {total_nodes}")
    print(f"Max Depth: {max_depth}")
    
    # Parse tree
    tree_json_str = tree.get('tree_json', '{}')
    
    try:
        if isinstance(tree_json_str, str):
            tree_data = json.loads(tree_json_str)
        else:
            tree_data = tree_json_str
        
        # Show only 1 level deep
        print(f"\n📋 {root} (root)")
        
        children = tree_data.get('children', [])
        print(f"\nDirect Children ({len(children)}):")
        print("-" * 80)
        
        for i, child in enumerate(children, 1):
            program_id = child.get('program_id', 'UNKNOWN')
            role = child.get('role', 'UNKNOWN')
            ui = child.get('ui', False)
            depth = child.get('depth', 0)
            child_children = child.get('children', [])
            
            ui_marker = "📋" if ui else "🔧"
            has_children = f"  → ({len(child_children)} children)" if child_children else "  [leaf]"
            
            print(f"  {i:2}. {ui_marker} {program_id:20} ({role:10}) {has_children}")
    
    except Exception as e:
        print(f"Error parsing tree: {e}")


print("\n" + "="*80)
print("Legend:")
print("="*80)
print("📋 = UI/Menu program")
print("🔧 = Business logic program")
print("[leaf] = No children (terminal)")
print("→ (N children) = Has N child programs")
