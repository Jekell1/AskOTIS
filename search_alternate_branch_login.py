"""
Search for Alternate Branch Login functionality across all screens.
"""
import sys
sys.path.insert(0, '.')

import json
import os
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential

# Load environment from local.settings.json
settings_file = 'local.settings.json'
if os.path.exists(settings_file):
    with open(settings_file, 'r') as f:
        settings = json.load(f)
        for key, value in settings.get('Values', {}).items():
            if key not in os.environ:
                os.environ[key] = str(value)

endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
key = os.getenv('AZURE_SEARCH_API_KEY') or os.getenv('AZURE_SEARCH_KEY')

if not endpoint or not key:
    print("❌ Missing Azure Search credentials")
    sys.exit(1)

search_client = SearchClient(
    endpoint=endpoint,
    index_name='new_cobol_screens',
    credential=AzureKeyCredential(key)
)

print("🔍 Searching for Alternate Branch Login functionality...")
print("=" * 70)

# Search for various patterns
search_terms = [
    "alternate branch",
    "branch login",
    "OTHBR",
    "switch branch",
    "remote branch",
    "branch access",
    "11.*branch",
    "option 11"
]

found_screens = set()

for term in search_terms:
    print(f"\n🔎 Searching for: '{term}'")
    
    results = list(search_client.search(
        search_text=term,
        top=10,
        select=['screen_name', 'scn_filename', 'ai_menu_screen_info', 'ai_description']
    ))
    
    if results:
        print(f"   Found {len(results)} matches:")
        for r in results:
            screen_key = f"{r['screen_name']}|{r['scn_filename']}"
            if screen_key not in found_screens:
                found_screens.add(screen_key)
                print(f"\n   📄 {r['screen_name']} ({r['scn_filename']})")
                
                # Show menu options if available
                menu_info = r.get('ai_menu_screen_info', '')
                if menu_info and len(menu_info) > 0:
                    # Show first 500 chars of menu info
                    print(f"      Menu: {menu_info[:500]}...")
                
                # Show description
                desc = r.get('ai_description', '')
                if desc:
                    print(f"      Desc: {desc[:200]}...")
    else:
        print(f"   No matches found")

print("\n" + "=" * 70)
print(f"📊 Total unique screens found: {len(found_screens)}")

if not found_screens:
    print("\n💡 Suggestions:")
    print("   1. Search in COBOL source code for 'ALTERNATE BRANCH'")
    print("   2. Check OTHBR program (Other Branch)")
    print("   3. Look for security/login screens")
    print("   4. Check if it's a dynamic menu option")
