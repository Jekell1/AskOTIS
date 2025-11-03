"""Compare PGMENU vs COLLECTION screen to see the difference."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()
client = SearchClient(config.search_endpoint, 'new_cobol_screen_nodes', AzureKeyCredential(config.search_key))

print("COMPARISON: PGMENU vs COLLECTION screens")
print("="*80)

# Get PGMENU
pgmenu_results = list(client.search('*', filter="program_id eq 'PGMENU'", top=1))
if pgmenu_results:
    print("\n✓ PGMENU Screen:")
    r = pgmenu_results[0]
    print(f"  Screen ID: {r.get('screen_id')}")
    print(f"  Summary length: {len(r.get('summary_text', ''))}")
    print(f"  Summary: {r.get('summary_text', 'NONE')}")
    print(f"  Has display_literals_json: {'display_literals_json' in r}")
    print(f"  Has value_clauses_json: {'value_clauses_json' in r}")
    if 'display_literals_json' in r:
        print(f"  display_literals_json: {r.get('display_literals_json')}")

# Get COLLECTION
coll_results = list(client.search('COLLECTION REPORTS', top=1))
if coll_results:
    print("\n✓ COLLECTION REPORTS Screen:")
    r = coll_results[0]
    print(f"  Screen ID: {r.get('screen_id')}")
    print(f"  Program ID: {r.get('program_id')}")
    print(f"  Summary length: {len(r.get('summary_text', ''))}")
    print(f"  Summary preview: {r.get('summary_text', 'NONE')[:500]}")
    print(f"  Has display_literals_json: {'display_literals_json' in r}")
    print(f"  Has value_clauses_json: {'value_clauses_json' in r}")
