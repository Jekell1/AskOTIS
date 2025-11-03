"""
Retrieve and display a menu tree from the index
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
print("Menu Tree Structure Examples")
print("="*80)

# Get menu trees with actual structure (filter out empty ones)
results = client.search(
    search_text='*',
    filter='total_nodes gt 0',
    top=10,
    select=['root_program_id', 'tree_json', 'max_depth', 'total_nodes', 'total_ui_nodes', 'ui_ratio'],
    order_by=['total_nodes desc']
)

for i, tree in enumerate(results, 1):
    root = tree.get('root_program_id', 'N/A')
    max_depth = tree.get('max_depth', 0)
    total_nodes = tree.get('total_nodes', 0)
    total_ui = tree.get('total_ui_nodes', 0)
    ui_ratio = tree.get('ui_ratio', 0)
    
    print(f"\n{'='*80}")
    print(f"Menu Tree #{i}: {root}")
    print("="*80)
    print(f"Max Depth: {max_depth}")
    print(f"Total Nodes: {total_nodes}")
    print(f"Total UI Nodes: {total_ui}")
    print(f"UI Ratio: {ui_ratio:.2%}")
    
    # Parse and display tree structure
    tree_json_str = tree.get('tree_json', '{}')
    
    try:
        if isinstance(tree_json_str, str):
            tree_data = json.loads(tree_json_str)
        else:
            tree_data = tree_json_str
        
        print(f"\nTree Structure:")
        print(json.dumps(tree_data, indent=2))
        
        # Also show a visual hierarchy
        print(f"\nVisual Hierarchy:")
        
        def print_tree(node, indent=0):
            """Recursively print tree structure"""
            program_id = node.get('program_id', 'UNKNOWN')
            role = node.get('role', 'UNKNOWN')
            ui = node.get('ui', False)
            depth = node.get('depth', 0)
            terminal = node.get('terminal', False)
            
            prefix = "  " * indent
            ui_marker = "📋" if ui else "🔧"
            terminal_marker = " [TERMINAL]" if terminal else ""
            
            print(f"{prefix}{ui_marker} {program_id} ({role}){terminal_marker}")
            
            children = node.get('children', [])
            for child in children:
                print_tree(child, indent + 1)
        
        print_tree(tree_data)
        
    except Exception as e:
        print(f"Error parsing tree: {e}")
    
    if i >= 3:  # Show just 3 examples
        break

print("\n" + "="*80)
print("Legend:")
print("="*80)
print("📋 = UI/Menu program (shows menu to user)")
print("🔧 = LEAF/UNKNOWN program (business logic)")
print("[TERMINAL] = Terminal node (end of path)")
print("\nRoles:")
print("  UI = Menu/navigation program")
print("  LEAF = Business logic program")
print("  UNKNOWN = Role not determined")
