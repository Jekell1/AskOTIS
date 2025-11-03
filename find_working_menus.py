"""Find screens that have menu text."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()
client = SearchClient(config.search_endpoint, 'new_cobol_screen_nodes', AzureKeyCredential(config.search_key))

print("Finding screens with 'MENU' in their content...")
print("="*80)

results = list(client.search('MENU', top=50))

screens_with_content = []
for r in results:
    screen_id = r.get('screen_id', 'Unknown')
    program_id = r.get('program_id', 'Unknown')
    summary = r.get('summary_text', '')
    
    # Check if summary has substantial content (more than just "Program X screen Y")
    if len(summary) > 50 and any(keyword in summary.upper() for keyword in ['1.', '2.', 'F6', 'F7', 'OPTION', 'MENU']):
        screens_with_content.append({
            'screen': screen_id,
            'program': program_id,
            'summary_preview': summary[:100]
        })

print(f"\nFound {len(screens_with_content)} screens with menu content:\n")

for item in screens_with_content[:15]:  # Show first 15
    print(f"Screen: {item['screen']}")
    print(f"  Program: {item['program']}")
    print(f"  Content: {item['summary_preview']}...")
    print()

if screens_with_content:
    print("="*80)
    print("Try asking about these menus:")
    print("="*80)
    
    # Extract likely menu names
    menu_names = set()
    for item in screens_with_content:
        screen = item['screen']
        # Extract menu name from screen ID (e.g., APIPAY_SCR1 -> APIPAY)
        if 'MENU' in screen.upper():
            menu_names.add(screen.replace('_SCR1', '').replace('_', ' '))
        elif 'REPORTS' in item['summary_preview'].upper():
            menu_names.add(f"{item['program']} REPORTS")
    
    for name in sorted(menu_names)[:10]:
        print(f"  • Show me the {name} menu")
