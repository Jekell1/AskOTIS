"""
Visual tree structure display for menu trees
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

def print_tree_visual(node, prefix="", is_last=True, max_depth=None, current_depth=0):
    """
    Print a visual tree structure using box-drawing characters
    """
    if max_depth is not None and current_depth >= max_depth:
        return
    
    # Get node info
    program_id = node.get('program_id', 'UNKNOWN')
    role = node.get('role', 'UNKNOWN')
    ui = node.get('ui', False)
    children = node.get('children', [])
    
    # Choose symbols
    ui_marker = "📋" if ui else "🔧"
    
    # Build the branch characters
    if current_depth == 0:
        connector = ""
        print(f"{ui_marker} {program_id} (root)")
    else:
        connector = "└── " if is_last else "├── "
        child_info = f" [{len(children)} children]" if children else ""
        print(f"{prefix}{connector}{ui_marker} {program_id}{child_info}")
    
    # Prepare prefix for children
    if current_depth > 0:
        extension = "    " if is_last else "│   "
        new_prefix = prefix + extension
    else:
        new_prefix = ""
    
    # Print children
    for i, child in enumerate(children):
        is_last_child = (i == len(children) - 1)
        print_tree_visual(child, new_prefix, is_last_child, max_depth, current_depth + 1)

print("="*80)
print("VISUAL MENU TREE STRUCTURES")
print("="*80)

# Get several interesting menu trees
menu_programs = ['PGMENU', 'WIMENU', 'SPMENU', 'OPMENU', 'LPMENU']

for menu_name in menu_programs:
    try:
        doc = client.get_document(key=menu_name)
        
        root = doc.get('root_program_id')
        total_nodes = doc.get('total_nodes')
        max_depth = doc.get('max_depth')
        ui_nodes = doc.get('total_ui_nodes', 0)
        
        print(f"\n{'='*80}")
        print(f"Menu: {root}")
        print(f"{'='*80}")
        print(f"Stats: {total_nodes} total nodes, {ui_nodes} UI nodes, depth {max_depth}")
        print()
        
        tree_json_str = doc.get('tree_json')
        tree = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
        
        # Print the visual tree (limit depth to 4 for readability)
        print_tree_visual(tree, max_depth=4)
        
    except Exception as e:
        print(f"\n{menu_name}: Not found or error - {e}")

print("\n" + "="*80)
print("Legend:")
print("="*80)
print("📋 = UI/Menu program (shows menus, screens)")
print("🔧 = Business logic program (performs operations)")
print("├── = Branch (more siblings below)")
print("└── = Last branch (no more siblings)")
print("│   = Continuation line")
print("[N children] = Has N child programs")
