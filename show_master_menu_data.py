"""Query screen_nodes and ui_paths for MASTER MENU related documents."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
import json

cfg = json.load(open('local.settings.json'))['Values']

print("="*80)
print("SCREEN_NODES: MASTER MENU RELATED")
print("="*80)

client_screens = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_cobol_screen_nodes', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for MASTER MENU
results = client_screens.search('MASTER MENU', top=15)
screen_count = 0
for i, doc in enumerate(results, 1):
    screen_count += 1
    print(f"\n{i}. Screen ID: {doc['screen_id']}")
    print(f"   Screen Name: {doc.get('screen_name', 'N/A')}")
    print(f"   Program ID: {doc.get('program_id', 'N/A')[:60]}")
    
    # Show summary
    summary = doc.get('summary_text', '')
    if summary:
        print(f"   Summary: {summary[:300]}")
    
    # Show label literals if they exist
    labels_json = doc.get('label_literals_json', '[]')
    if labels_json and labels_json != '[]':
        try:
            labels = json.loads(labels_json)
            if labels:
                print(f"   Label Literals ({len(labels)} items):")
                for lbl in labels[:8]:
                    print(f"     Line {lbl['line']:>2} Col {lbl['col']:>2}: {lbl['text']}")
        except:
            pass
    
    # Check if has vector
    has_vec = doc.get('has_vector', False)
    print(f"   Has Vector: {has_vec}")

print(f"\n\nTotal screen_nodes with 'MASTER MENU': {screen_count}")

print("\n" + "="*80)
print("SCREEN_NODES: LPMENU SCREENS")
print("="*80)

# Search for LPMENU specifically
results = client_screens.search('LPMENU', top=10)
lpmenu_count = 0
for i, doc in enumerate(results, 1):
    lpmenu_count += 1
    print(f"\n{i}. Screen ID: {doc['screen_id']}")
    print(f"   Screen Name: {doc.get('screen_name', 'N/A')}")
    
    summary = doc.get('summary_text', '')
    if summary:
        print(f"   Summary: {summary[:400]}")
    
    labels_json = doc.get('label_literals_json', '[]')
    if labels_json and labels_json != '[]':
        try:
            labels = json.loads(labels_json)
            if labels:
                print(f"   Label Literals ({len(labels)} items):")
                for lbl in labels[:15]:
                    print(f"     Line {lbl['line']:>2} Col {lbl['col']:>2}: {lbl['text']}")
        except:
            pass

print(f"\n\nTotal LPMENU screens: {lpmenu_count}")

print("\n" + "="*80)
print("UI_PATHS: MASTER MENU RELATED")
print("="*80)

client_paths = SearchClient(cfg['SEARCH_ENDPOINT'], 'new_ui_paths', AzureKeyCredential(cfg['SEARCH_KEY']))

# Search for MASTER MENU in UI paths
results = client_paths.search('MASTER MENU', top=15)
path_count = 0
for i, doc in enumerate(results, 1):
    path_count += 1
    print(f"\n{i}. Path ID: {doc.get('path_id', 'N/A')}")
    print(f"   Root Program: {doc.get('root_program', 'N/A')}")
    print(f"   Leaf Program: {doc.get('leaf_program', 'N/A')}")
    print(f"   Depth: {doc.get('depth', 'N/A')}")
    
    # Show the path
    path_str = doc.get('path_str', '')
    if path_str:
        print(f"   Path: {path_str[:200]}")
    
    # Show path programs list
    path_programs = doc.get('path_programs', [])
    if path_programs:
        print(f"   Programs ({len(path_programs)}): {' → '.join(path_programs[:8])}")
    
    # Show screens if any
    screens_json = doc.get('screens_json', '[]')
    if screens_json and screens_json != '[]':
        try:
            screens = json.loads(screens_json)
            if screens:
                print(f"   Screens ({len(screens)}):")
                for scr in screens[:5]:
                    print(f"     - {scr.get('name', 'Unknown')}: {scr.get('text', '')[:100]}")
        except:
            pass

print(f"\n\nTotal ui_paths with 'MASTER MENU': {path_count}")

print("\n" + "="*80)
print("UI_PATHS: LPMENU RELATED")
print("="*80)

# Search for LPMENU in UI paths
results = client_paths.search('LPMENU', top=15)
lpmenu_path_count = 0
for i, doc in enumerate(results, 1):
    lpmenu_path_count += 1
    print(f"\n{i}. Path ID: {doc.get('path_id', 'N/A')}")
    print(f"   Root Program: {doc.get('root_program', 'N/A')}")
    print(f"   Leaf Program: {doc.get('leaf_program', 'N/A')}")
    print(f"   Path: {doc.get('path_str', '')[:200]}")
    
    path_programs = doc.get('path_programs', [])
    if path_programs:
        print(f"   Programs: {' → '.join(path_programs[:8])}")

print(f"\n\nTotal ui_paths with 'LPMENU': {lpmenu_path_count}")
