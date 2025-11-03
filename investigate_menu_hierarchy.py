"""
Investigate the actual top-level menu structure
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
print("INVESTIGATING MENU HIERARCHY")
print("="*80)

# Check if WIMENU and SPMENU appear as children anywhere
major_menus = ['PGMENU', 'WIMENU', 'SPMENU', 'BPMENU', 'OPMENU', 'TWMENU', 
               'ASMENU', 'UPMENU', 'FDMENU', 'GLMENU', 'EOMENU', 'CLMENU', 
               'COMENU', 'FXMENU', 'IRMENU', 'SEMENU', 'SCMENU']

print("\n1. Checking each major menu as a ROOT:")
print("-" * 80)

for menu_name in major_menus[:10]:
    try:
        doc = client.get_document(key=menu_name)
        total_nodes = doc.get('total_nodes', 0)
        max_depth = doc.get('max_depth', 0)
        
        tree_json_str = doc.get('tree_json')
        tree = json.loads(tree_json_str) if isinstance(tree_json_str, str) else tree_json_str
        children = tree.get('children', [])
        
        child_names = [c.get('program_id') for c in children]
        
        print(f"\n{menu_name}:")
        print(f"  Nodes: {total_nodes}, Depth: {max_depth}")
        print(f"  Direct children ({len(children)}): {', '.join(child_names[:10])}")
        
        # Check if any other major menus are children
        menu_children = [c for c in child_names if c in major_menus]
        if menu_children:
            print(f"  *** Contains other major menus: {menu_children}")
        
    except Exception as e:
        print(f"\n{menu_name}: Not found")

print("\n" + "="*80)
print("2. Searching for who CALLS these major menus:")
print("="*80)

# Search in calls index to see who calls WIMENU, SPMENU, etc.
calls_client = SearchClient(
    endpoint=config.search_endpoint,
    index_name='calls',
    credential=AzureKeyCredential(config.search_key)
)

for target_menu in ['WIMENU', 'SPMENU', 'BPMENU', 'OPMENU']:
    results = list(calls_client.search(
        search_text=target_menu,
        filter=f"called_program_id eq '{target_menu}'",
        top=5,
        select=['caller_program_id', 'called_program_id']
    ))
    
    if results:
        callers = [r['caller_program_id'] for r in results]
        print(f"\n{target_menu} is called by: {', '.join(callers[:10])}")
    else:
        print(f"\n{target_menu}: No callers found in calls index")

print("\n" + "="*80)
print("3. Are these SEPARATE top-level menus?")
print("="*80)
print("Each major menu (WIMENU, SPMENU, etc.) might be accessed:")
print("  - Directly via transaction code (user types 'WI', 'SP', etc.)")
print("  - Not necessarily through PGMENU hierarchy")
print("  - They are parallel entry points, not nested")
