"""Debug which screens are being retrieved and in what order."""
import sys
sys.path.insert(0, '.')

from otis_rag.config import Config
from otis_rag.router import QueryRouter
from otis_rag.retriever import HybridRetriever

config = Config()
router = QueryRouter()
retriever = HybridRetriever(config)

query = "What are the main menu options in OTIS?"
routing = router.route(query)

print(f"Query: {query}")
print(f"Question type: {routing['question_type']}")
print(f"Search indexes: {routing['search_indexes']}")
print()

# Get retrieval results
results = retriever.retrieve(
    query=routing['clean_query'],
    indexes=routing['search_indexes'],
    max_results=20,
    index_weights=routing.get('index_weights', {}),
    question_type=routing['question_type']
)

print(f"\nTOP 10 RETRIEVED SCREENS:")
print("=" * 80)

for i, result in enumerate(results[:10], 1):
    screen_id = result.get('screen_id', 'N/A')
    summary = result.get('summary_text', '')
    
    # Check if this is the LPMENU screen
    is_lpmenu = 'EA66581C142E6BA08A83D2DC773BC990DB403C3C' in screen_id
    is_master = 'M A S T E R' in summary
    
    marker = ""
    if is_lpmenu:
        marker = " ← LPMENU SCREEN!"
    elif is_master:
        marker = " ← has MASTER MENU text"
    
    print(f"\n{i}. {screen_id[:40]}...{marker}")
    print(f"   Summary: {summary[:200]}...")
    
    if is_lpmenu:
        print(f"   FULL SUMMARY: {summary}")

# Check if LPMENU is in results at all
lpmenu_found = any('EA66581C142E6BA08A83D2DC773BC990DB403C3C' in r.get('screen_id', '') for r in results)
print(f"\n\nLPMENU in results: {lpmenu_found}")
if not lpmenu_found:
    print("❌ LPMENU NOT IN RESULTS - This is the problem!")
