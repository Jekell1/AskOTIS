"""Find the COLLECTION REPORTS screen details."""
from azure.search.documents import SearchClient
from azure.core.credentials import AzureKeyCredential
from otis_rag.config import Config

config = Config()
client = SearchClient(config.search_endpoint, 'new_cobol_screen_nodes', AzureKeyCredential(config.search_key))

print("Searching for COLLECTION REPORTS...")
print("="*80)

results = list(client.search('COLLECTION REPORTS', top=5))

for i, r in enumerate(results, 1):
    print(f"\n--- Result {i} ---")
    print(f"Screen ID: {r.get('screen_id')}")
    print(f"Program ID: {r.get('program_id')}")
    print(f"Summary length: {len(r.get('summary_text', ''))}")
    print(f"Summary:\n{r.get('summary_text', 'NONE')}")
    print()

# Try to find screens with substantial summaries
print("\n" + "="*80)
print("Screens with longest summaries (likely to have menu text):")
print("="*80)

# Get a broader sample and sort by summary length
all_results = list(client.search('*', top=100))
sorted_by_summary = sorted(all_results, key=lambda x: len(x.get('summary_text', '')), reverse=True)

for i, r in enumerate(sorted_by_summary[:10], 1):
    summary = r.get('summary_text', '')
    if len(summary) > 100:  # Only show substantial ones
        print(f"\n{i}. {r.get('screen_id')} ({r.get('program_id')})")
        print(f"   Length: {len(summary)} chars")
        print(f"   Preview: {summary[:150]}...")
